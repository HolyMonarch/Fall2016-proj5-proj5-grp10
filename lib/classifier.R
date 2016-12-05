library(dplyr)
library(gbm)

load('../output/reg_ready_data.RData')


class_data = reg_data %>%
  mutate(OutcomeType = as.integer(OutcomeType)) %>%
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

shrinkages = c(.001, .01, .1, 1)
ntrees = c(1,2,5,10)
errs = rep(NA, length(shrinkages))
for(i in 1:length(shrinkages)){
  cv_err = rep(NA, K)
  for (k in 1:K){
    cat('i =',i, 'of', length(shrinkages), '; k =',k, 'of', K, '\n')
    train_data = class_data[s != k,]
    test_data = class_data[s == k,]
    
    fit = gbm(OutcomeType~. , data = train_data, 
              distribution="bernoulli", 
              n.trees=50, 
              interaction.depth=1,
              shrinkage=shrinkages[i])
    
    pred = predict(fit, newdata=test_data, n.trees=ntrees[i])>0
    
    cv_err[k] = mean(pred != test_data$OutcomeType) 
  }			
  
  errs[i] = mean(cv_err)
}

errs




