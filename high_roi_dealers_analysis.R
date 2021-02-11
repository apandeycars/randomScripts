# select
#   sar.customer_id,
#   sar.lead_type_name,
#   sar.lead_type_category,
#   case
#     when sar.sort_field like '%searchRank%' then 'best_match'
#     when sar.sort_field like '%price%' then 'price'
#     else 'others'
#   end as sort_field,
#   dp.roi_value_30_day,
#   dp.total_leads_30_days,
#   srp_perf.n_vins_seen,
#   srp_perf.avg_srp2vdp
# from insight.search_activity_raw sar
# join (
#   select
#     dp.customer_id,
#     dp.roi_value_30_day,
#     da.total_leads_30day - da.other_leads_30day as total_leads_30_days
#   from insight.dealer_predictions dp
#   join insight.dealer_activity da
#     on da.customer_id = dp.customer_id
#     and da.filedate = dp.filedate
#     and da.total_spend > 10
#   where dp.filedate = '2020-07-27'
# ) dp
#   on sar.customer_id = dp.customer_id
# join (
#   select
#     sar.customer_id,
#     count(*) n_srps,
#     count(distinct classified_ad_id) n_vins_seen,
#     sum(srp2vdp) n_vdps,
#     cast(sum(srp2vdp)/count(*) as float) avg_srp2vdp
#   from insight.search_activity_raw sar
#   where sar.filedate = '2020-07-27'
#   group by sar.customer_id
# ) srp_perf
#   on sar.customer_id = srp_perf.customer_id
# where sar.sublead = '1'
#   and sar.filedate = '2020-07-27'


library(data.table)
inputData <- setDT(read.csv("high_roi_analysis.csv"))
inputData <- inputData[!(is.na(roi_value_30_day))]
inputData$roi_10_plus <- ifelse(inputData$roi_value_30_day >= 10, 1, 0)

# Are best match connections going to high ROI customers?
# Ans: No.
aggData <- inputData[, .(mean_roi = mean(roi_value_30_day, na.rm = T), .N), by = list(sort_field)][order(-N)]
aggData

# Are best match leads going to high ROI customers?
# Ans: No.
aggData <- inputData[ lead_type_category %in% c('Used Car E-Mail Lead', 'Used Car Toll Free Lead', 'New Car E-Mail Lead', 'New Car Toll Free Lead')
  , .(mean_roi = mean(roi_value_30_day, na.rm = T), .N), by = list(sort_field)][order(-N)]
aggData

leadData <- inputData[ lead_type_category %in% c('Used Car E-Mail Lead', 'Used Car Toll Free Lead', 'New Car E-Mail Lead', 'New Car Toll Free Lead')]
leadData[ , .(
        n_customers = length(unique(customer_id)),
        avg_n_vins_seen = mean(n_vins_seen),
        avg_srp2vdp = mean(avg_srp2vdp),
        bm_leads = sum(ifelse(sort_field == 'best_match', 1, 0)),
        price_leads = sum(ifelse(sort_field == 'price', 1, 0)),
        others_leads = sum(ifelse(sort_field == 'others', 1, 0))
), by = list(roi_10_plus)]
