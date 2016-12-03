library(dplyr)

load('../output/reg_ready_data.RData')

# fit logistic regression:
m = glm(OutcomeType ~ ., data = reg_data, family="binomial") 

# select signif predictors:
coef_stats = coef(summary(m))
coef_stats = coef_stats[coef_stats[,4]<.05,]

# plot
plot_z_val = data.frame(pred=rownames(coef_stats), val=coef_stats[,3])
plot_z_val = arrange(plot_z_val, desc(abs(val)))

plot_ly(plot_z_val,
        x = pred,
        y = val,
        type = 'bar', 
        text = pred, 
        hoverinfo = 'text') %>%
  layout(title = "Significant predictors in Adoption outcome",
         xaxis = list(title = ""),
         yaxis = list(title = "Influence (z-value)"))



# likelihood ratio test to determine importance of each predictor:

# lrt = anova(object=m, test="Chisq")
# save(lrt, file='../output/lrt_log_reg.RData')
load('../output/lrt_log_reg.RData')

# plot
importance = lrt[-1,2]/lrt[-1,1]
names(importance) = rownames(lrt)[-1]
importance = sort(importance, decreasing=T)
importance = as.data.frame(as.table(importance))
colnames(importance) = c('pred', 'val')

colors = rep(rgb(0,.4,0), nrow(importance))
colors[importance$pred %in% c('Age','IsMale')] = rgb(.6,0,0) # change colors for predictors with neg influence based on logistic reg results

plot_ly(x = importance$pred,
        y = log(importance$val),
        type = 'bar', 
        text = importance$pred, 
        hoverinfo = 'text', 
        marker = list(color = colors), 
        height = 400) %>%
  layout(title = "Predictors of Adoption",
         xaxis = list(title = "", position=.1, domain=c(0,.9)),
         yaxis = list(title = "Importance [log(deviance/df)]", domain=c(.1,1)))


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






