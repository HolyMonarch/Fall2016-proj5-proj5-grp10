

load('../output/reg_ready_data.RData')

with(reg_data, c(mean(OutcomeType[IsSpayed]), mean(OutcomeType[!IsSpayed]) ))
