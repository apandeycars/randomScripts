library(data.table)
library(plotly)
library(readr)
options(width = 150)
leadQualityData <- setDT(read_csv("leadQualityData.csv"))
leadQualityData$preds <- as.numeric(leadQualityData$preds)
leadQualityData$intent_score <- as.numeric(leadQualityData$intent_score)
table(leadQualityData$major_account_name)
leadQualityData <- leadQualityData[, .(ci_class = max(ci_class),
                                       preds = max(preds),
                                       intent_score = max(intent_score),
                                       lead_sub_month = max(lead_sub_month)
                                      ), by = list(consumer_email, major_account_name)]
head(leadQualityData)
write.csv(leadQualityData[, .(consumer_email, ci_class, lead_sub_month)], file = "leadCategoryFileCarvana.csv", row.names = F)

# select
#   da.major_account_name,
#   dl.consumer_email,
#   dl.date_id,
#   case
#     when ci.class = 'very bad' then 'A'
#     when ci.class = 'bad' then 'B'
#     when ci.class = 'fair' then 'C'
#     when ci.class = 'good' then 'D'
#     when ci.class = 'very good' then 'E'
#     else NULL
#   end as ci_class
# from insight.consumer_intent ci
# join dw.lead dl
#   on dl.src_lead_id = cast(ci.lead_id as char)
#   and dl.date_id = ci.filedate
# join (
#   select
#       dealer_legacy_id,
#       customer_id,
#       da.major_account_name
#   from insight.dealer_activity da
#   where filedate >= '2020-01-01'
#     and filedate <= '2020-07-01'
#     and (lower(da.major_account_name) like '%carvana%'
#       or lower(da.customer_name) like '%carvana%'
#     )
#   group by 1, 2, 3
# ) da
#   on dl.customer_id = da.customer_id
# where dl.date_id >= '2020-01-01'
# and dl.date_id <= '2020-07-01'
# and dl.vehicle_id is not null
# and dl.vehicle_id <> ''
# group by
#   da.major_account_name,
#   dl.consumer_email,
#   dl.date_id,
#   ( case
#     when ci.class = 'very bad' then 'A'
#     when ci.class = 'bad' then 'B'
#     when ci.class = 'fair' then 'C'
#     when ci.class = 'good' then 'D'
#     when ci.class = 'very good' then 'E'
#     else NULL
#   end
#   )
# ;
