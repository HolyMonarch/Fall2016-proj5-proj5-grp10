library(dplyr)
library(gbm)

load('../output/reg_ready_data.RData')


class_data = reg_data %>%
  mutate(HasName = as.integer(HasName)) %>%
  mutate(IsMale = as.integer(IsMale)) %>%
  mutate(IsSpayed = as.integer(IsSpayed)) %>%
  mutate(IsTwoBreeds = as.integer(IsTwoBreeds)) %>%
  mutate(IsMix = as.integer(IsMix)) %>%
  mutate(IsMulticolor = as.integer(IsMulticolor))
  
# cross validation:
K = 5
n = nrow(class_data)
n.fold = floor(n/K)
s = sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
for (i in 1:K){
  train_data = class_data[s != i,]
  test_data = class_data[s == i,]

  fit = gbm(OutcomeType~. , data = train_data, distribution="bernoulli", n.trees=100, shrinkage=0.05)
  pred = predict(fit, newdata=test_data, n.trees=100)  
  
  cv.error[i] = mean(pred != test_data$OutcomeType) 
  
}			

return(mean(cv.error))

  
fit = gbm(OutcomeType~. , data=class_data, distribution="bernoulli", n.trees=100, shrinkage=0.05)
summary(fit)





