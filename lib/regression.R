library(dplyr)

load('../output/reg_ready_data.RData')

# fit logistic regression:
m = glm(OutcomeType ~ ., data = reg_data, family="binomial") 
coef_stats = coef(summary(m))


# plot signif predictors
sig_coefs = coef_stats[coef_stats[,4]<.05,]
plot_z_val = data.frame(pred=rownames(sig_coefs), val=sig_coefs[,3])
plot_z_val = arrange(plot_z_val, desc(abs(val)))
plot_ly(plot_z_val,
        x = ~pred,
        y = ~val,
        type = 'bar', 
        text = ~pred, 
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



# plot most infulential breeds for adoption outcome
data_pure_breeds = reg_data %>% filter(!IsMix) %>% select(-c(IsMix, IsTwoBreeds))
m2 = glm(OutcomeType ~ MainBreed, data = data_pure_breeds, family="binomial") 
breed_coefs = coef(summary(m2))
breed_coefs = breed_coefs[grepl('MainBreed', rownames(breed_coefs)),]
rownames(breed_coefs) = gsub('MainBreed', '', rownames(breed_coefs))
breed_coefs = breed_coefs[order(abs(breed_coefs[,3]), decreasing=T),]




