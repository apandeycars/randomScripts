#cg_ih_price_algo_comparison.R
library(data.table)
library(plotly)
library(readr)
options(width = 150)
'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
photo_count_data <- setDT(read_csv("cg_ih_price_compare.csv"))
#names(photo_count_data)[which(names(photo_count_data) == 'inferred_sale_price')] <- 'inferred_sale_filedate'
photo_count_data$per_pred_price_diff_cg <- photo_count_data$per_pred_price_diff_cg
photo_count_data <- photo_count_data[price_badge_cg != 'No Price Analysis']
photo_count_data <- photo_count_data[!(is.na(per_pred_price_diff_cg))]
photo_count_data <- photo_count_data[!(is.na(per_pred_price_diff_ih))]
ih_median_error_rate <- round(median(abs(photo_count_data$per_pred_price_diff_ih)), 2)
cg_median_error_rate <- round(median(abs(photo_count_data$per_pred_price_diff_cg)), 2)
message("Algo's median error rate are: 1. In-house = ",ih_median_error_rate, "% and CG = ", cg_median_error_rate, "%")
message("Monthly Error Rates")
photo_count_data$inferred_sale_month <- substr(photo_count_data$inferred_sale_filedate, 0, 7)
max_inferred_sale_month <- max(photo_count_data$inferred_sale_month)
photo_count_data <- photo_count_data[inferred_sale_month!=max_inferred_sale_month]
dataByMonth <- photo_count_data[, .(n_vins_analysed = .N,
  ih_median_error_rate = round(median(abs(per_pred_price_diff_ih)), 2),
  cg_median_error_rate = round(median(abs(per_pred_price_diff_cg)), 2),
  diff_performance = round(median(abs(per_pred_price_diff_cg)), 2) - round(median(abs(per_pred_price_diff_ih)), 2)
), by = list(inferred_sale_month)][order(inferred_sale_month)]
write.csv(dataByMonth, file = "dataByMonth.csv", row.names = F)

p <- plot_ly(dataByMonth) %>%
  add_trace(x = ~inferred_sale_month, y = ~ih_median_error_rate, type = 'scatter', mode = 'lines+markers', name = 'Cars.com') %>%
  add_trace(x = ~inferred_sale_month, y = ~cg_median_error_rate, type = 'scatter', mode = 'lines+markers', name = "CarGurus") %>%
  layout(xaxis = list(title = "Month", tickangle = -45),
         yaxis = list(title = "MdAPE (%)"),
         title = "Median Error Rate of Cars.com and CG Pricing Algorithms",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Yipit",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

dataByBS <- photo_count_data[bodystyle_name != 'Unknown', .(n_vins_analysed = .N, median_price = round(median(price), 0),
  ih_median_error_rate = round(median(abs(per_pred_price_diff_ih)), 2),
  cg_median_error_rate = round(median(abs(per_pred_price_diff_cg)), 2),
  diff_performance = round(median(abs(per_pred_price_diff_cg)), 2) - round(median(abs(per_pred_price_diff_ih)), 2)
), by = list(bodystyle_name)][order(-1*n_vins_analysed)]

p <- plot_ly(dataByBS) %>%
  add_trace(x = ~bodystyle_name, y = ~ih_median_error_rate, type = 'bar', name = 'Cars.com') %>%
  add_trace(x = ~bodystyle_name, y = ~cg_median_error_rate, type = 'bar', name = "CarGurus") %>%
  layout(xaxis = list(title = "Bodystyle", tickangle = -45),
         yaxis = list(title = "MdAPE (%)"),
         title = "Median Error Rate of Cars.com and CG Pricing Algorithms \n by Bodystyle",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Yipit \nJan-July of 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

dataByMake <- photo_count_data[, .(n_vins_analysed = .N, per_pop = 100*.N/nrow(photo_count_data),
  median_price = round(median(price), 0),
  ih_median_error_rate = round(median(abs(per_pred_price_diff_ih)), 2),
  cg_median_error_rate = round(median(abs(per_pred_price_diff_cg)), 2),
  diff_performance = round(median(abs(per_pred_price_diff_cg)), 2) - round(median(abs(per_pred_price_diff_ih)), 2)
), by = list(make_name)][order(-1*n_vins_analysed)]
dataByMake <- dataByMake[per_pop >= 1]
p <- plot_ly(dataByMake[per_pop >= 1]) %>%
  add_trace(x = ~make_name, y = ~ih_median_error_rate, type = 'bar', name = 'Cars.com') %>%
  add_trace(x = ~make_name, y = ~cg_median_error_rate, type = 'bar', name = "CarGurus") %>%
  layout(xaxis = list(title = "OEM", tickangle = -45),
         yaxis = list(title = "MdAPE (%)"),
         title = "Median Error Rate of Cars.com and CG Pricing Algorithms \n for major OEMs",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Yipit \nJan-July of 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

metropolitan_city_state <- read.csv('metropolitan_city_state.csv')
metropolitan_city_state$city_state <- paste(metropolitan_city_state$city, metropolitan_city_state$state, sep = ',')
photo_count_data$city_state <- paste(photo_count_data$city, photo_count_data$state, sep = ', ')
dataByCityState <- merge(photo_count_data, metropolitan_city_state, by = 'city_state')

metropolitan_area <- read.csv('metropolitan_area.csv')
dataByCityState <- merge(photo_count_data, metropolitan_area, by.x = 'city', by.y = 'principal_city_name')

dataByCityState <- dataByCityState[, .(n_vins_analysed = .N,
  per_pop = 100*.N/nrow(photo_count_data),
  median_price = round(median(price), 0),
  ih_median_error_rate = round(median(abs(per_pred_price_diff_ih)), 2),
  cg_median_error_rate = round(median(abs(per_pred_price_diff_cg)), 2),
  diff_performance = round(median(abs(per_pred_price_diff_cg)), 2) - round(median(abs(per_pred_price_diff_ih)), 2)
), by = list(metropolitan_area_name)][order(-1*n_vins_analysed)]


# select
#   a.*,
#   (cast(a.pred_price_cg as float) - cast(a.price as float)) as pred_price_diff_cg,
#   100.00*(cast(a.pred_price_cg as float) - cast(a.price as float))/cast(a.price as float) as per_pred_price_diff_cg
# from (
# select
#   ia.vin,
#   ia.filedate inferred_sale_filedate,
#   ia.city,
#   ia.state,
#   ia.make_name,
#   ia.model_name,
#   ia.bodystyle_name,
#   ia.model_year,
#   pb.price,
#   pb.pred_price pred_price_ih,
#   pb.per_pred_price_diff per_pred_price_diff_ih,
#   pb.pred_price_diff pred_price_diff_ih,
#   pb.price_badge price_badge_ih,
#   cg.expected_price pred_price_cg,
#   cg.savings_recommendation price_badge_cg,
#   count(*) n_rows
# from dw_vw.infer_sale_vehicle_vw isvd
# JOIN insight.inventory_activity ia
#   ON isvd.vin = ia.vin
#   AND isvd.date_id = CAST(DATEADD(day, 1, CAST(ia.filedate AS DATE)) AS DATE)
# join insight.price_badge_v2 pb
#   on ia.classified_ad_id = pb.classified_ad_id
#   and ia.filedate = pb.filedate
# join raw_ext.yipit_carguru_listing_raw cg
#   on ia.vin = cg.vehicle_identifier
#   and ia.filedate = cg.filedate
# where isvd.date_id = '2020-05-01'
# and lower(ia.customer_name) not like '%carvana%'
# and lower(ia.customer_name) not like '%vroom%'
# and lower(ia.customer_name) not like '%offlease%'
# and pb.price is not null and pb.price_badge is not null
# group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
# having n_rows = 1
# ) a
# ;
