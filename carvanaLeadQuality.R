#carvanaLeadQuality

library(data.table)
library(plotly)
library(readr)
options(width = 150)
leadQualityData <- setDT(read_csv("leadQualityData.csv"))
# leadQualityData$preds <- as.numeric(leadQualityData$preds)
# leadQualityData$intent_score <- as.numeric(leadQualityData$intent_score)
leadQualityData$date_id <- as.Date(leadQualityData$date_id)
table(leadQualityData$major_account_name)
leadQualityData <- leadQualityData[, .(ci_class = max(ci_class),
                                       # preds = max(preds),
                                       # intent_score = max(intent_score),
                                       lead_sub_month = max(lead_sub_month),
                                       date_id = max(date_id)
                                      ), by = list(consumer_email, major_account_name)]
head(leadQualityData)
write.csv(leadQualityData[, .(consumer_email, ci_class, lead_sub_month)], file = "leadCategoryFileCarvana.csv", row.names = F)
