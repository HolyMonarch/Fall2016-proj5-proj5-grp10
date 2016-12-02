
# D = data.table::fread('../data/shelter_data.csv')
# D = D[D$AnimalType=='Dog',]
# D$AnimalType = NULL
# 
# # replace 2-breed dogs with majoritary breed + "Mix"
# repl = function(x){ifelse(grepl('/',x), paste(strsplit(x, '/')[[1]][1], 'Mix'), x)}
# D$Breed = sapply(D$Breed, repl)
# write.csv(D ,"../output/shelter_dogs.csv", row.names=FALSE)

D = data.table::fread('../output/shelter_dogs.csv')
D$Breed = tolower(D$Breed)

# get baseline frequencies of breeds in U.S.
breeds = data.table::fread('../data/breed_count.csv')
breed_count = as.integer(gsub(',', '', breeds$count))
breeds_names = gsub('\\(', '', breeds$breed)
breeds_names = gsub('\\)', '', breeds_names)
breeds_names = gsub(' Imp', '', breeds_names)
breeds_names = tolower(breeds_names)
names(breed_count) = breeds_names
breed_count = breed_count[breed_count>0]


# define pit bull
pit_bull_types = c('pit bull', 
                   'staffordshire bull terrier',
                   'american pit bull terrier',
                   'american bulldog',
                   'american staffordshire terrier',
                   'staffordshire')
D$Breed = gsub(paste(pit_bull_types, collapse='|'), 'pit bull', D$Breed)
names(breed_count) = gsub(paste(pit_bull_types, collapse='|'), 'pit bull', names(breed_count))
# D$Breed[D$Breed %in% pit_bull_types] = 'pit bull'
# names(breed_count)[names(breed_count) %in% pit_bull_types] = 'pit bull'

# define chihuahua
chihuahua_types = c('chihuahua shorthair', 
                    'chihuahua longhair',
                    'chihuahua long coat',
                    'chihuahua smooth coat')
D$Breed = gsub(paste(chihuahua_types, collapse='|'), 'chihuahua', D$Breed)
names(breed_count) = gsub(paste(chihuahua_types, collapse='|'), 'chihuahua', names(breed_count))

# define dachshund
dachshund_types = c('dachshund', 
                    'dachshund longhair',
                    'dachshund wirehair',
                    'dachshund smooth haired',
                    'dachshund long haired',
                    'dachshund wire haired',
                    'dachshund miniature smooth haired',
                    'dachshund miniature long haired',
                    'dachshund miniature wire haired')
D$Breed = gsub(paste(dachshund_types, collapse='|'), 'dachshund', D$Breed)
names(breed_count) = gsub(paste(dachshund_types, collapse='|'), 'dachshund', names(breed_count))

# replace english bulldog with bulldog
D$Breed = gsub('english bulldog', 'bulldog', D$Breed)

# recount breeds
breed_count = tapply(breed_count, names(breed_count), sum)


# focus on pure breeds most common in shelter (n>8)
sh_breeds = table(D$Breed[!grepl('mix', D$Breed)])
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
barplot(log(rel_freqs))


# plot
plot_freqs = as.data.frame(rel_freqs)
colnames(plot_freqs) = c('breed', 'val')
a = ggplot(plot_freqs, aes(x=1:length(val), y=log(val), fill=log(val))) 
a = a + geom_bar(stat="identity") +
  xlab("") + 
  ylab("Relative frequency") + 
  scale_colour_gradientn(colours=rainbow(3)) + 
  theme_bw()



# names(breed_count)[grepl('aus', names(breed_count))]
# names(sh_breeds)[grepl('australian', names(sh_breeds))]


