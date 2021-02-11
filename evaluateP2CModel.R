library(data.table); library(dplyr)
options(width = 200)
predData <- setDT(read.csv('p2c_results_20200413.csv'))
newAccounts <- setDT(read.csv('new_accounts.csv'))

predData = mutate(predData, percentile_val_final_score = ntile(predData$final_score,100))
predData = mutate(predData, percentile_p2c = ntile(predData$probability_to_close,100))
predData = mutate(predData, percentile_es_rev = ntile(predData$estimated_contract_value,100))


finalData <- merge(predData, newAccounts, by = 'sf_account_id')
write.csv(finalData, file = "evaluationSet.csv", row.names = F)

hgA <- density(finalData$total_spend, plot = FALSE)
hgB <- density(finalData$estimated_contract_value, plot = FALSE)

plot(hgA, col = "purple")
plot(hgB, col = "yellow", add = TRUE)

plot(density(finalData$estimated_contract_value), col = "purple", main = "Density Plots of Predicted (Purple) \n v/s Actual (Black) Revenue")
lines(density(finalData$total_spend), col = "black")
