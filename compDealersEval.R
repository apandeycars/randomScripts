#compDealersEval.R
library(data.table)
library(arrow)
options(width = 150)
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
inputData <- setDT(read_parquet('top50_competitors.pq'))
#dealer_activity <- setDT(read.csv('dealer_activity.csv'))
inputData <- inputData[, .(dealer, dealer_name, competitor, competitor_name, competitor_rank, dealer_competitor_distance)]
names(inputData)[1] <- 'dealer_customer_id'; names(inputData)[3] <- 'competitor_customer_id';
inputData <- merge(inputData, dealer_activity[, .(dealer_legacy_id, customer_id, shipping_state_code, franchise_independent, shipping_city)], by.x = 'dealer_customer_id', by.y = 'customer_id')

#sampleDealers <- setDT(read.csv('sampleDealers.csv'))
inputData <- inputData[dealer_legacy_id %in% sampleDealers$dealer_legacy_id]
inputData <- inputData[competitor_rank <= 5]
write.csv(inputData, file = "evaluation.csv", row.names = F)

#Same City competitor. 
# select
#   a.dealer_name,
#   a.dealer_f_i,
#   a.competitor_name,
#   a.competitor_f_i,
#   a.n_total_vehicles,
#   a.n_similar_vehicles,
#   a.n_similar_vehicles*100.00/a.n_total_vehicles per_inventory_match,
#   a.tmp_rank comp_rank
# from (
# select
#   ia.customer_id customer_id,
#   ia.customer_name dealer_name,
#   ia.franchise_independent dealer_f_i,
#   ia_next.customer_id comp_customer_id,
#   ia_next.customer_name competitor_name,
#   ia_next.franchise_independent competitor_f_i,
#   count(distinct ia.classified_ad_id) + count(distinct ia_next.classified_ad_id) n_total_vehicles,
#   count(
#     distinct case
#       when ia.new_used_ind = ia_next.new_used_ind
#         and ia.make_name = ia_next.make_name
#         and ia.model_name = ia_next.model_name
#         and ia.city = ia_next.city
#         then ia.classified_ad_id
#       else null
#     end
#     ) as n_similar_vehicles,
#     RANK() OVER (PARTITION BY ia.customer_id, ia.customer_name, ia.franchise_independent ORDER BY n_similar_vehicles DESC) as tmp_rank
# from insight_prod.inventory_activity ia
# join insight_prod.inventory_activity ia_next
#   on ia.classified_ad_id <> ia_next.classified_ad_id
#   and ia.customer_id <> ia_next.customer_id
#   and ia.filedate = ia_next.filedate
# where ia.filedate = '2020-10-01'
#   and lower(ia.customer_name) like '%fletcher jones audi%'
#   and ia.city = 'Chicago'
# group by 1, 2, 3, 4, 5, 6
# having n_similar_vehicles >= 2
# ) a
# where a.tmp_rank <= 10 ;
#
