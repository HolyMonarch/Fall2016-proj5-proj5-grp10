library(dplyr)
source('../lib/simplify_breeds.R')

D = data.table::fread('../data/shelter_data.csv')

reg_data = D %>% 
  filter(AnimalType == 'Dog') %>%
  filter(OutcomeType %in% c('Adoption', 'Transfer')) %>% 
  mutate(OutcomeType = OutcomeType=='Adoption') %>%
  mutate(HasName = Name != '') %>%
  mutate(IsMale = grepl('Male', SexuponOutcome)) %>%
  mutate(IsSpayed = grepl('Spayed|Neutered', SexuponOutcome)) %>%
  mutate(IsTwoBreeds = grepl('/', Breed)) %>%
  mutate(IsMulticolor = grepl('/|Tricolor', Color)) %>%
  mutate(IsMix = IsTwoBreeds | grepl('Mix', Breed))


# define main breed  
breeds = tolower(gsub(' Mix', '', reg_data$Breed))
breeds = sapply(breeds, function(x){strsplit(x, '/')[[1]][1]})
breeds = simplify_breeds(breeds)
reg_data$MainBreed = as.factor(breeds)

# define main color variable  
reg_data$MainColor = as.factor(sapply(reg_data$Color, function(x){strsplit(x, '/')[[1]][1]}))

# define age variable, in months
isageyears = grepl('year', reg_data$AgeuponOutcome)
isagemonts = grepl('month', reg_data$AgeuponOutcome)
isageweeks = grepl('week', reg_data$AgeuponOutcome)
age = as.numeric(sapply(reg_data$AgeuponOutcome, function(x){strsplit(x, ' ')[[1]][1]}))
reg_data$Age = age*(12*isageyears + 1*isagemonts + 0.25*isageweeks)


reg_data = reg_data %>% select(OutcomeType, 
                               MainBreed,
                               MainColor,
                               Age,
                               HasName,
                               IsMale,
                               IsSpayed,
                               IsTwoBreeds,
                               IsMix, 
                               IsMulticolor)

# save(reg_data, file='../output/reg_ready_data.RData')
# load('../output/reg_ready_data.RData')

m = glm(OutcomeType ~ ., data = reg_data, family="binomial") 

# show signif predictors:
coef_stats = coef(summary(m))
coef_stats[coef_stats[,4]<.05,]

# likelihood ratio test:
lrt = anova(object=m, test="Chisq")


# classifier:
library(gbm)
class_data = reg_data %>%
  mutate(HasName = as.integer(HasName)) %>%
  mutate(IsMale = as.integer(IsMale)) %>%
  mutate(IsSpayed = as.integer(IsSpayed)) %>%
  mutate(IsTwoBreeds = as.integer(IsTwoBreeds)) %>%
  mutate(IsMix = as.integer(IsMix)) %>%
  mutate(IsMulticolor = as.integer(IsMulticolor))
  
  
  fit = gbm(OutcomeType~. , data=class_data, distribution="bernoulli", n.trees=100, shrinkage=0.05)

pred = predict(fit, newdata = X_test, missing=NA)

cv_acc[k] = mean(pred == y_test) 
}			
accuracy[i] = mean(cv_acc)
}
accuracy





