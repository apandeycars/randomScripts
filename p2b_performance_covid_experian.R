# p2b_performance_covid_experian.R
library(DT)
library(data.table)
library(plotly)
library(readr)
options(width = 150)
'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
experian_mb_data <- setDT(read_csv("experian_mb_data.csv"))
scored_leads_data <- setDT(read_csv("scored_leads_data.csv"))
fullData <- merge(scored_leads_data, experian_mb_data, by.x = 'dl_lead_id', by.y = 'src_lead_id')
fullData <- fullData[, src_inferred_purchase_date:=as.Date(as.character(src_inferred_purchase_date))]
fullData <- fullData[, filedate:=as.Date(as.character(filedate))]
fullData <- fullData[, days_to_purchase:= src_inferred_purchase_date - filedate]
fullData <- fullData[, bought_in_30:= ifelse(days_to_purchase <= 30 & !is.na(src_inferred_purchase_date), 1, 0)]
table(fullData[, .(class, bought_or_not)])
table(fullData[, .(bought_in_30, bought_or_not)])
table(fullData[, .(bought_in_30, bought_or_not)])

lb <- quantile(fullData$intent_score, 0.02)
ub <- quantile(fullData$intent_score, 0.99)
fullData$intent_score <- ifelse(fullData$intent_score <= lb, lb, fullData$intent_score)
fullData$intent_score <- ifelse(fullData$intent_score >= ub, ub, fullData$intent_score)
dataToPlot <- fullData[, .(.N,
  mean_bought_or_not = round(100*mean(bought_or_not), 2),
  mean_bought_in_30 = round(100*mean(bought_in_30), 2)
), by = list(intent_score)][order(-mean_bought_or_not)]

p <- plot_ly(dataToPlot, x = ~intent_score, y = ~mean_bought_in_30, type = 'scatter') %>%
  layout(xaxis = list(title = "Consumer Intent Score", tickangle = -45),
         yaxis = list(title = "Actual Sale Rates (%)"),
         title = "Consumer Intent Score versus Actual Sale Rates [Within 30 Days]",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Experian\n 643,440 Data Points",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

monthlyStats <- fullData[!(is.na(class)), .(.N,
  mean_bought_or_not = round(100*mean(bought_or_not), 2),
  pr_v_good = round(100*mean(ifelse(class=='very good', bought_or_not, NA), na.rm = TRUE), 2),
  pr_good = round(100*mean(ifelse(class=='good', bought_or_not, NA), na.rm = TRUE), 2),
  pr_fair = round(100*mean(ifelse(class=='fair', bought_or_not, NA), na.rm = TRUE), 2),
  pr_bad = round(100*mean(ifelse(class=='bad', bought_or_not, NA), na.rm = TRUE), 2),
  pr_v_bad = round(100*mean(ifelse(class=='very bad', bought_or_not, NA), na.rm = TRUE), 2),
  mean_bought_in_30 = round(100*mean(bought_in_30), 2),
  pr_30d_v_good = paste(round(100*mean(ifelse(class=='very good', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_good = paste(round(100*mean(ifelse(class=='good', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_fair = paste(round(100*mean(ifelse(class=='fair', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_bad = paste(round(100*mean(ifelse(class=='bad', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_v_bad = paste(round(100*mean(ifelse(class=='very bad', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = '')
),by = list(src_year_month)]
monthlyStats <- monthlyStats[, .(src_year_month, N, pr_30d_v_good, pr_30d_good, pr_30d_fair, pr_30d_bad, pr_30d_v_bad)]
column_names <- c('Month', 'Total Leads', 'Purchase Rate \n Very Good', 'Purchase Rate \n Good', 'Purchase Rate \n Fair', 'Purchase Rate \n Bad', 'Purchase Rate \n Very Bad')
formatThousands <- JS(
  "function(data) {",
  "return (data / 1000).toFixed(1) + 'K'",
  "}")
datatable(monthlyStats,
  rownames = FALSE,
  colnames = column_names,
  options = list(
    columnDefs = list(
      list(className = 'dt-center', targets = 0:6),
      list(targets = 1:1, render = formatThousands)
    )
  )
)

# dataToPlot <- fullData[, .(.N,
#   mean_bought_in_30 = round(100*mean(bought_in_30), 2),
#   sdev_bought_in_30 = round(100*sd(bought_in_30), 2)
# ), by = list(intent_score)]
#
# plot(dataToPlot$intent_score, dataToPlot$mean_bought_in_30,
#     ylim=range(c(dataToPlot$mean_bought_in_30-dataToPlot$sdev_bought_in_30, dataToPlot$mean_bought_in_30+dataToPlot$sdev_bought_in_30)),
#     pch=19, xlab="Measurements", ylab="Mean +/- SD",
#     main="Scatter plot with std.dev error bars"
# )
# # hack: we draw arrows but with very special "arrowheads"
# arrows(dataToPlot$intent_score,
#   dataToPlot$mean_bought_in_30-dataToPlot$sdev_bought_in_30,
#   dataToPlot$intent_score,
#   dataToPlot$mean_bought_in_30+dataToPlot$sdev_bought_in_30,
#   length=0.05, angle=90, code=3
# )

carvana_look_back <- setDT(read.csv("carvana_look_back.csv"))
carvana_look_back[, .(.N, mean_lock_rate = mean(HasAtLeastOnePostEmailLock)),
by = list(ci_class)]
