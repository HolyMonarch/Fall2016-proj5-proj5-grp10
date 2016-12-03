
load('../output/reg_ready_data.RData')

m = glm(OutcomeType ~ ., data = reg_data, family="binomial") 

# show signif predictors:
coef_stats = coef(summary(m))
coef_stats[coef_stats[,4]<.05,]

# likelihood ratio test:
lrt = anova(object=m, test="Chisq")
lrt


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






