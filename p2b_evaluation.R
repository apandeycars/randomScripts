library(DT)
library(data.table)
library(plotly)
library(readr)
options(width = 150)
'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
p2b_evaluation <- setDT(read_csv("p2b_evaluation.csv"))
fullData <- p2b_evaluation
fullData <- fullData[, inferred_purchase_date:=as.Date(as.character(inferred_purchase_date))]
fullData <- fullData[, date_id:=as.Date(as.character(date_id))]
fullData <- fullData[, days_to_purchase:= inferred_purchase_date - date_id]
fullData <- fullData[, bought_in_30:= ifelse(days_to_purchase <= 30 & !is.na(inferred_purchase_date), 1, 0)]
fullData <- fullData[, month_id:=substr(date_id, 1, 7)]
fullData <- fullData[, bought_or_not:=sold_or_not]
#head(fullData)
monthlyStats <- fullData[!(is.na(p2b_class)) & month_id != '2020-04', .(.N,
  mean_bought_or_not = round(100*mean(bought_or_not), 2),
  pr_good = round(100*mean(ifelse(p2b_class %in% c('good', 'very good'), bought_or_not, NA), na.rm = TRUE), 2),
  pr_fair = round(100*mean(ifelse(p2b_class=='fair', bought_or_not, NA), na.rm = TRUE), 2),
  pr_bad = round(100*mean(ifelse(p2b_class %in% c('very bad', 'bad'), bought_or_not, NA), na.rm = TRUE), 2),
  mean_bought_in_30 = round(100*mean(bought_in_30), 2),
  pr_30d_good = paste(round(100*mean(ifelse(p2b_class %in% c('good', 'very good'), bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_fair = paste(round(100*mean(ifelse(p2b_class=='fair', bought_in_30, NA), na.rm = TRUE), 2), '%', sep = ''),
  pr_30d_bad = paste(round(100*mean(ifelse(p2b_class %in% c('very bad', 'bad'), bought_in_30, NA), na.rm = TRUE), 2), '%', sep = '')
),by = list(month_id, lead_type)][order(month_id, lead_type)]

monthlyStats <- fullData[!(is.na(p2b_class)) & month_id != '2020-04', .(.N,
  mean_bought_or_not = round(100*mean(bought_or_not), 2),
  mean_preds = round(mean(preds, na.rm = T), 2)
),by = list(month_id, lead_type)][order(month_id, lead_type)]


# select
#   dl.date_id date_id,
#   dl.lead_id dl_lead_id,
#   case
#     when dl.stock_type_id = '2' then 'Used'
#     else 'New'
#   end as new_used_ind,
#   ci.class p2b_class,
#   case when ci.class in ('good', 'very good') then 1 else 0 end as good_lead,
#   case when ci.class in ('bad', 'very bad') then 1 else 0 end as bad_lead,
#   case when ci.class in ('fair') then 1 else 0 end as fair_lead,
#   ci.intent_score intent_score,
#   ci.preds preds,
#   ci.lead_type,
#   case when lmb.src_purchased_vehicle_make is null then 0 else 1 end sold_or_not,
#   lmb.lead_stock_type,
#   substring(lmb.lead_submitted_date_time, 1, 10) lead_submitted_date,
#   lmb.src_inferred_purchase_date inferred_purchase_date
# from insight_prod.consumer_intent ci
# join dw_prod.lead dl
#   on dl.src_lead_id = ci.src_lead_id
#   and dl.date_id = ci.filedate
# join dw.lead_matchback lmb
#   on dl.lead_id = lmb.src_lead_id
# where dl.date_id >= '2020-01-01'
#   and dl.vehicle_id is not null
#   and dl.vehicle_id <> ''
#   and dl.stock_type_id in ('1', '2')
# ;
