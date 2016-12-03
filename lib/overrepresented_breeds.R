library(ggplot2)
library(plotly)
source('../lib/simplify_breeds.R')

# get shelter data and keep dogs only
D = read.csv('../data/shelter_data.csv')
D = D[D$AnimalType=='Dog',]
D$Breed = tolower(D$Breed)

# get baseline frequencies of breeds in U.S.
breeds = read.csv('../data/breed_count.csv')
breed_count = as.integer(gsub(',', '', breeds$count))
breeds_names = gsub('\\(', '', breeds$breed)
breeds_names = gsub('\\)', '', breeds_names)
breeds_names = gsub(' Imp', '', breeds_names)
breeds_names = tolower(breeds_names)
names(breed_count) = breeds_names
breed_count = breed_count[breed_count>0]

# simplify breeds
D$Breed = simplify_breeds(D$Breed)
names(breed_count) = simplify_breeds(names(breed_count))

# recount breeds after simplification
breed_count = tapply(breed_count, names(breed_count), sum)

# focus on pure breeds most common in shelter (n>9)
sh_breeds = table(D$Breed[!grepl('mix|/', D$Breed)])
sh_breeds = sh_breeds[sh_breeds>9]


# resolve conflicts between breed lists:
sh_breeds = sh_breeds[!names(sh_breeds)=='rat terrier']
sh_breeds = sh_breeds[!names(sh_breeds)=='anatol shepherd']
names(breed_count)[names(breed_count)=='retriever labrador'] = 'labrador retriever'
names(breed_count)[names(breed_count)=='retriever golden'] = 'golden retriever'
names(breed_count)[names(breed_count)=='german shepherd dog alsatian'] = 'german shepherd'
names(breed_count)[names(breed_count)=='poodle miniature'] = 'miniature poodle'
names(breed_count)[names(breed_count)=='dobermann'] = 'doberman pinsch'
names(breed_count)[names(breed_count)=='spaniel cocker'] = 'cocker spaniel'
names(breed_count)[names(breed_count)=='pyrenean mountain dog'] = 'great pyrenees'
names(breed_count)[names(breed_count)=='parson russell terrier'] = 'jack russell terrier'

# compute relative frequencies of main breeds in shelter vs country
sh_breeds_freq = sh_breeds/sum(sh_breeds)
breeds_freq = breed_count[names(sh_breeds)]/sum(breed_count[names(sh_breeds)])
rel_freqs = sort(sh_breeds_freq/breeds_freq, decreasing=T)

# plot:
plot_freqs = as.data.frame(rel_freqs)
colnames(plot_freqs) = c('breed', 'val')

color_overrep = colorRampPalette(c("red", "black")) (sum(plot_freqs$val>1)) 
color_underrep = colorRampPalette(c("black", "green")) (sum(plot_freqs$val<=1)) 

plot_ly(plot_freqs,
        x = breed,
        y = log(val),
        type = 'bar', 
        text = breed, 
        hoverinfo = 'text', 
        marker = list(color = c(color_overrep, color_underrep))) %>%
  
  layout(title = "Over-represented Dog Breeds in Shelter",
         xaxis = list(title = ""),
         yaxis = list(title = "<- Less than expected   |   More than expected ->"))



