library(dplyr)
library(data.table)

setwd("/Users/jiwenyou/Desktop/project5")

shelter_data = fread("shelter_data.csv")
Austin_Animal_Center_FY14_Intakes = fread("Austin_Animal_Center_FY14_Intakes.csv")
Austin_Animal_Center_FY14_Outcomes = fread("Austin_Animal_Center_FY14_Outcomes.csv")
Austin_Animal_Center_FY15_Intakes_Updated_Hourly_ = fread("Austin_Animal_Center_FY15_Intakes__Updated_Hourly_.csv")
Austin_Animal_Center_FY15_Outcomes_Updated_Hourly_ = fread("Austin_Animal_Center_FY15_Outcomes__Updated_Hourly_.csv")

# rename the variable "Animal_ID"
names(Austin_Animal_Center_FY14_Intakes)[1] = "Animal ID"
names(Austin_Animal_Center_FY14_Outcomes)[1] = "Animal ID"
names(Austin_Animal_Center_FY15_Intakes_Updated_Hourly_)[1] = "Animal ID"
names(Austin_Animal_Center_FY15_Outcomes_Updated_Hourly_)[1] = "Animal ID"

# combine the intake and outcome datasets for 2014 and 2015
# the number of records is greater than the total amount of animals
# some dogs or cats may be adopted or intaken twice
intake = rbind(Austin_Animal_Center_FY14_Intakes, Austin_Animal_Center_FY15_Intakes_Updated_Hourly_)

outcome = rbind(Austin_Animal_Center_FY14_Outcomes, Austin_Animal_Center_FY15_Outcomes_Updated_Hourly_)

# unify the date format
time <- function(x){
  if (nchar(x) == 8){
    x = paste(substr(x,1,6), "20", substr(x,7,8), sep = "")
  } else(x = x)
}
intake$`Intake Date` = sapply(intake$`Intake Date`, function(x) time(x))
intake$`Intake Date` = as.Date(intake$`Intake Date`,"%m/%d/%Y")
outcome$`Outcome Date` = sapply(outcome$`Outcome Date`, function(x) time(x))
outcome$`Outcome Date` = as.Date(outcome$`Outcome Date`,"%m/%d/%Y")

# the variable "Count" represents the total times a pet adopted by the shelter
intake = intake %>% group_by(`Animal ID`) %>% mutate(Count = n())
# the variable "Frequency" orders the adoption by the date
# 1 represents the first time and 2 represents the second time
intake$Frequency = 1
intake = setorder(intake, `Animal ID`, `Intake Date`)
for (i in 2:dim(intake)[1]){
  if(intake$`Animal ID`[i] == intake$`Animal ID`[i-1]){
    intake$Frequency[i] = intake$Frequency[i-1] + 1
  }
}
save(intake, file = "intake.RData")

outcome = outcome %>% group_by(`Animal ID`) %>% mutate(Count = n())
outcome$Frequency = 1
outcome = setorder(outcome, `Animal ID`, `Outcome Date`)
for (i in 2:dim(outcome)[1]){
  if(outcome$`Animal ID`[i] == outcome$`Animal ID`[i-1]){
    outcome$Frequency[i] = outcome$Frequency[i-1] + 1
  }
}
save(outcome, file = "outcome.RData")

# merge income data and outcome into a new dataset
shelter_data_new = merge(intake, outcome, by = c("Animal ID", "Frequency", "Count"))
shelter_data_new = shelter_data_new %>% filter(Name.x == Name.y) %>%
  filter(Breed.x == Breed.y) %>% filter(Color.x == Color.y) %>%
  filter(`Animal Type.x` == `Animal Type.y`) %>%
  select(-ends_with(".y")) %>% 
  rename(Name = Name.x) %>% rename(Breed = Breed.x) %>% rename(Color = Color.x) %>%
  rename(`Animal Type` = `Animal Type.x`)

# create variable Breed_Mix
# if the dog breed is in "A Mix" or "A/B" format, then Breed_Mix equals to TRUE
shelter_data_new$Breed_Mix = grepl(" Mix|/", shelter_data_new$Breed)
shelter_data_new$Breed1 = gsub(" Mix", "", shelter_data_new$Breed)

# remove hair description
# combine these dog into one breed
shelter_data_new$Breed1 = gsub(" Shorthair| Longhair| Wirehaired| Wirehair|Wirehaired | Hair", "", shelter_data_new$Breed1)

# represent the breed of a dog by a combination of two breeds
# "A" -> "A" & "A" & mix = FALSE
# "A/B" -> "A" & "B" & mix = TRUE
# "A mix" -> "A" & "A" & mix = TRUE
split = function(x){
  if(grepl("/", x) == FALSE){
    return(x)
  } else{
    return(strsplit(x, '/')[[1]][2])
  }
}
shelter_data_new$Breed2 = sapply(shelter_data_new$Breed1, function(x) split(x))
shelter_data_new$Breed1 = sapply(shelter_data_new$Breed1, function(x) strsplit(x, '/')[[1]][1])

# the list of all type of breeds and their frequency
doglist = as.data.frame(table(append(shelter_data_new$Breed1, shelter_data_new$Breed2)))
names(doglist)[1] = "Breed"
write.csv(doglist, file = "doglist.csv")

# group: dog breeds and their belonging dog group
group = as.data.frame(read.csv("doggroup.csv"))
# group = group[match(unique(group$Breed), group$Breed),]

breed_group = function(x){
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Herding")]), collapse = "|"),x)] = 'Herding'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Hound")]), collapse = "|"),x)] = 'Hound'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Sporting")]), collapse = "|"),x)] = 'Sporting'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Terrier")]), collapse = "|"),x)] = 'Terrier'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Toy")]), collapse = "|"),x)] = 'Toy'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Working")]), collapse = "|"),x)] = 'Working'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Non-sporting")]), collapse = "|"),x)] = 'Non-sporting'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Domestic")]), collapse = "|"),x)] = 'Domestic'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Other-animal")]), collapse = "|"),x)] = 'Other-animal'
  x[grepl(paste(as.vector(group$Breed[which(group$Group == "Others")]), collapse = "|"),x)] = 'Others'
  return(x)
}

shelter_data_new$Breed1 = breed_group(shelter_data_new$Breed1)
shelter_data_new$Breed2 = breed_group(shelter_data_new$Breed2)

breed = function(x){
  if (x[1] == x[2]){
    return(x[1])
  } else {
    reorder = sort(x)
    return(paste(reorder[1],"&",reorder[2],sep = " "))
  }
}

shelter_data_new$Breed_New = apply(cbind(shelter_data_new$Breed1,shelter_data_new$Breed2),1, function(x) breed(x))

# modify the color variable
# define main color variable
shelter_data_new$MainColor = as.factor(sapply(shelter_data_new$Color, function(x){strsplit(as.character(x), '/')[[1]][1]}))
shelter_data_new$Color_Mix = grepl('/|Tricolor', shelter_data_new$Color)

save(shelter_data_new, file = "shelter_data_new.RData")

