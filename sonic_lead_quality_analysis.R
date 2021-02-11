#sonic_lead_quality_analysis.R
#roi_distribution_analysis.R
library(data.table)
library(plotly)
library(readr)
library(ggplot2)
library(scales)
options(width = 150)
sonic_cr_data <- setDT(read_csv("datasets/sonic_cr_data.csv"))
sonic_cr_data$sold_or_not <- ifelse(sonic_cr_data$sold_or_not == 'Sold', 1, 0)
sonic_cr_data <- unique(sonic_cr_data)
summary(sonic_cr_data$sold_or_not)
head(sonic_cr_data)

leadQualityData <- setDT(read_csv("datasets/sonic_lead_quality_data.csv"))
leadQualityData$intent_score <- as.numeric(leadQualityData$intent_score)
table(leadQualityData$major_account_name)
leadQualityData <- leadQualityData[, .(ci_class = max(ci_class),
                                       intent_score = max(intent_score),
                                       lead_sub_month = max(lead_sub_month)
                                      ), by = list(consumer_email, major_account_name)]
head(leadQualityData)
allLeadsData <- merge(leadQualityData, sonic_cr_data, by = 'consumer_email')
allLeadsData <- allLeadsData[!(is.na(ci_class))]
lb <- quantile(allLeadsData$intent_score, 0.02)
ub <- quantile(allLeadsData$intent_score, 0.99)
allLeadsData$intent_score <- ifelse(allLeadsData$intent_score <= lb, lb, allLeadsData$intent_score)
allLeadsData$intent_score <- ifelse(allLeadsData$intent_score >= ub, ub, allLeadsData$intent_score)
dataToPlot <- allLeadsData[, .(.N, mean_sold_or_not = round(100*mean(sold_or_not), 2)), by = list(intent_score)][order(-mean_sold_or_not)]
plot(dataToPlot$intent_score, dataToPlot$mean_sold_or_not)

library(plotly)
p <- plot_ly(dataToPlot, x = ~intent_score, y = ~mean_sold_or_not, type = 'scatter') %>%
  layout(xaxis = list(title = "Consumer Intent Score", tickangle = -45),
         yaxis = list(title = "Actual Sale Rates (%)"),
         title = "Consumer Intent Score versus Actual Sale Rates [Sales / Total Leads]\n for Sonic - 2020",
         margin = list(b = 100),
         barmode = 'group')
p
write.csv(allLeadsData, file = "datasets/allLeadsData_sonic.csv", row.names = F)
