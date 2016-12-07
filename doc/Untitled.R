load("../data/shelter_data_new.RData")
library(dplyr)
library(plotly)
library(ggplot2)
D = shelter_data_new
D = D[D$`Animal Type`=='Dog',]
outcome = c("Return to Owner","Transfer","Adoption","Euthanasia")

adoption_rate = data.frame(Breed = c(unique(D$Breed_New)), Adoption = 0, Euthanasia = 0, `Return to Owner` = 0, Transfer = 0)
for (i in 1:dim(adoption_rate)[1]){
  x = adoption_rate[i,]
  table = D %>% filter(`Outcome Type` %in% outcome) %>%
    filter(`Breed_New` == x$Breed) %>% group_by(`Outcome Type`) %>% summarise(count = n())
  freq = table$count/sum(table$count)
  if ("Adoption" %in% table$`Outcome Type`){
    adoption_rate$Adoption[i] = freq[which(table$`Outcome Type` == "Adoption")]
  }
  if ("Euthanasia" %in% table$`Outcome Type`){
    adoption_rate$Euthanasia[i] = freq[which(table$`Outcome Type` == "Euthanasia")]
  }
  if ("Return to Owner" %in% table$`Outcome Type`){
    adoption_rate$Return.to.Owner[i] = freq[which(table$`Outcome Type` == "Return to Owner")]
  }
  if ("Transfer" %in% table$`Outcome Type`){
    adoption_rate$Transfer[i] = freq[which(table$`Outcome Type` == "Transfer")]
  }
}
adoption_rate = adoption_rate[order(adoption_rate$Adoption),]

adoption_rate$Breed = factor(adoption_rate$Breed, levels = adoption_rate$Breed)
m = melt(adoption_rate, id = "Breed")
ggplot(data = m, aes(x = Breed, y = value, group = variable, color = variable)) +geom_line()


f1 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "lightgrey"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 10,
  color = "black"
)
a <- list(
  title = "AXIS TITLE",
  titlefont = f1,
  showticklabels = TRUE,
  tickangle = 45,
  tickfont = f2,
  exponentformat = "E"
)
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
plot_ly(adoption_rate, x = ~Breed, y = ~Adoption, name = 'Adoption rate', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Euthanasia, name = 'Euthanasia rate', mode = 'lines') %>%
  add_trace(y = ~Return.to.Owner, name = 'Return to Owner rate', mode = 'lines') %>%
  add_trace(y = ~Transfer, name = 'Transfer rate', mode = 'lines') %>%
  layout(title = 'Dog Breeds versus Outcome Type',
         yaxis = list(title = ''),
         xaxis = a) %>% layout(autosize = F, margin = m)
  
  