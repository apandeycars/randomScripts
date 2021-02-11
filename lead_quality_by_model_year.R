
#lead_quality_by_model_year.R
#lead_id, classified_ad_id, class, intent_score, model_year, pred
library(data.table)
library(plotly)
library(readr)
library(ggplot2)
library(scales)
options(width = 150)

'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
experian_mb_data <- setDT(read_csv("experian_mb_data.csv"))
scored_leads_data <- setDT(read_csv("p2c_distance_analysis.csv"))

fullData <- merge(scored_leads_data, experian_mb_data, by.x = 'dl_lead_id', by.y = 'src_lead_id')
fullData <- fullData[, src_inferred_purchase_date:=as.Date(as.character(src_inferred_purchase_date))]
fullData <- fullData[, date_id:=as.Date(as.character(date_id))]

zipcode <- setDT(read_csv('zipcode.csv'))
fullData <- fullData[consumer_zip_code %in% unique(zipcode$zip_code)]
fullData <- fullData[customer_zip_code %in% unique(zipcode$zip_code)]
zipcode <- as.data.frame(zipcode)
fullData <- as.data.frame(fullData)
fullData$distance_miles<-apply(fullData, 1, function(x){
   startindex<-which(x[["consumer_zip_code"]]==zipcode$zip_code)
   endindex<-which(x[["customer_zip_code"]]==zipcode$zip_code)
   distGeo(p1=c(zipcode[startindex, "longitude"], zipcode[startindex, "latitude"]), p2=c(zipcode[endindex, "longitude"], zipcode[endindex, "latitude"]), a=3958)
 })
setDT(fullData)
fullData <- fullData[, days_to_purchase:= src_inferred_purchase_date - date_id]
fullData <- fullData[, bought_in_30:= ifelse(days_to_purchase <= 30 & !is.na(src_inferred_purchase_date), 1, 0)]
fullData <- fullData[, is_same_state:= ifelse(consumer_state == customer_state_code, 1, 0)]

# table(fullData[, .(class, bought_or_not)])
# table(fullData[, .(bought_in_30, bought_or_not)])
# table(fullData[, .(bought_in_30, bought_or_not)])

aggData <- fullData [, .(.N,
              purchase_rate = mean(bought_or_not),
              pr_in_30 = mean(bought_in_30),
              mean_is = mean(intent_score),
              mean_preds = mean(preds),
              mean_good = mean(good_lead),
              mean_fair = mean(fair_lead),
              mean_bad = mean(bad_lead),
              md_distance = median(distance_miles)
), by = list(model_year)]

aggDataUsed <- fullData [new_used_ind == 'Used', .(.N,
              purchase_rate = mean(bought_or_not),
              pr_in_30 = mean(bought_in_30),
              mean_is = mean(intent_score),
              mean_preds = mean(preds),
              mean_good = mean(good_lead),
              mean_fair = mean(fair_lead),
              mean_bad = mean(bad_lead),
              md_distance = median(distance_miles)
), by = list(model_year)]

aggData[N>1000][order(model_year)]
data_to_plot <- aggData[N>1000][order(model_year)]
p <- plot_ly(data_to_plot) %>%
  add_lines(x = ~model_year, y = ~purchase_rate, type = 'scatter', mode = 'lines', name = 'Purchase\nRate (PR)') %>%
  add_lines(x = ~model_year, y = ~pr_in_30, type = 'scatter', mode = 'lines', name = '30 Days\nPurchase Rate') %>%
  add_lines(x = ~model_year, y = ~mean_preds, type = 'scatter', mode = 'lines', name = '30 Days\nP2B') %>%
  layout(xaxis = list(title = "Model Year", tickangle = -45),
         yaxis = list(title = "Rates", range = c(0.1, 0.5)),
         title = "Purchase Rate and P2B By Model Year \n For Used Vehicles Only",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan through April 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

data_to_plot <- aggDataUsed[N>1000][order(model_year)]
p <- plot_ly(data_to_plot) %>%
  add_lines(x = ~model_year, y = ~purchase_rate, type = 'scatter', mode = 'lines', name = 'Purchase\nRate (PR)') %>%
  add_lines(x = ~model_year, y = ~pr_in_30, type = 'scatter', mode = 'lines', name = '30 Days\nPurchase Rate') %>%
  add_lines(x = ~model_year, y = ~mean_preds, type = 'scatter', mode = 'lines', name = '30 Days\nP2B') %>%
  layout(xaxis = list(title = "Model Year", tickangle = -45),
         yaxis = list(title = "Rates", range = c(0.1, 0.5)),
         title = "Purchase Rate and P2B By Model Year",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan through April 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

aggData <- fullData [, .(.N,
              purchase_rate = mean(bought_or_not),
              pr_in_30 = mean(bought_in_30),
              mean_is = mean(intent_score),
              mean_preds = mean(preds),
              mean_good = mean(good_lead),
              mean_fair = mean(fair_lead),
              mean_bad = mean(bad_lead),
              md_distance = median(distance_miles)
), by = list(is_same_state, new_used_ind)]
aggData[N>1000][order(model_year)]

fullData$rounded_dist <- round(fullData$distance_miles, 0)

aggDataUsed <- fullData [new_used_ind == 'Used' & rounded_dist <= 1000, .(.N,
              purchase_rate = mean(bought_or_not),
              pr_in_30 = mean(bought_in_30),
              mean_is = mean(intent_score),
              mean_preds = mean(preds)
), by = list(rounded_dist)]

aggDataNew <- fullData [new_used_ind == 'New' & rounded_dist <= 1000, .(.N,
              purchase_rate = mean(bought_or_not),
              pr_in_30 = mean(bought_in_30),
              mean_is = mean(intent_score),
              mean_preds = mean(preds)
), by = list(rounded_dist)]

data_to_plot <- aggDataUsed[N>100][order(rounded_dist)]
p <- plot_ly(data_to_plot) %>%
  add_lines(x = ~rounded_dist, y = ~purchase_rate, type = 'scatter', name = 'Purchase\nRate (PR)') %>%
  add_lines(x = ~rounded_dist, y = ~pr_in_30, type = 'scatter', name = '30 Days\nPurchase Rate') %>%
  add_lines(x = ~rounded_dist, y = ~mean_preds, type = 'scatter', name = '30 Days\nP2B') %>%
  layout(xaxis = list(title = "Distance", tickangle = -45),
         yaxis = list(title = "Rates", range = c(0, 1)),
         title = "Purchase Rate and P2B By Model Year",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan through April 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

data_to_plot <- aggDataNew[N>100][order(rounded_dist)]
p <- plot_ly(data_to_plot) %>%
  add_lines(x = ~rounded_dist, y = ~purchase_rate, type = 'scatter', name = 'Purchase\nRate (PR)') %>%
  add_lines(x = ~rounded_dist, y = ~pr_in_30, type = 'scatter', name = '30 Days\nPurchase Rate') %>%
  add_lines(x = ~rounded_dist, y = ~mean_preds, type = 'scatter', name = '30 Days\nP2B') %>%
  layout(xaxis = list(title = "Distance", tickangle = -45),
         yaxis = list(title = "Rates", range = c(0, 1)),
         title = "Purchase Rate and P2B By Model Year",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan through April 2020",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

library('rpart.plot')
a <- rpart(bought_or_not~distance_miles + new_used_ind, fullData, control = rpart.control(cp = 0.0001))
rpart.plot(a)

a <- rpart(bought_or_not~distance_miles + model_year + new_used_ind, fullData, control = rpart.control(cp = 0.0001))
rpart.plot(a)

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
#   substring(dl.model_year_id, 1, 4) model_year,
#   case
#     when substring(coalesce(dl.search_zip_code_id, dl.consumer_zip_code_id), 1, 5) = 0 then null
#     else substring(coalesce(dl.search_zip_code_id, dl.consumer_zip_code_id), 1, 5)
#   end as consumer_zip_code,
#   dl.consumer_city,
#   dl.consumer_state,
#   da.customer_zip_code customer_zip_code,
#   da.customer_city customer_city,
#   da.customer_state_name customer_state_name,
#   da.customer_state_code customer_state_code,
#   pcd.distance distance_user_inventory
# from insight.consumer_intent ci
# join dw.lead dl
#   on dl.src_lead_id = ci.lead_id
#   and dl.date_id = ci.filedate
# join (
#   select
#       da.dealer_legacy_id,
#       da.customer_id,
#       substring(da.billing_postal_code, 1, 5) customer_zip_code,
#       da.billing_city customer_city,
#       sc.state_name customer_state_name,
#       sc.state_abbrev customer_state_code
#   from insight.dealer_activity da
#   join (select distinct state_name, state_abbrev from master_data.state_county) sc
#     on da.billing_state_code = sc.state_abbrev
#   where da.filedate >= '2019-12-01'
#     and da.filedate < '2020-05-01'
#   group by 1, 2, 3, 4, 5, 6
# ) da
#   on dl.customer_id = da.customer_id
# left join master_data.postal_code_distance pcd
#   on substring(coalesce(dl.search_zip_code_id, dl.consumer_zip_code_id), 1, 5) = pcd.from_postal_cd
#   and da.customer_zip_code = pcd.to_postal_cd
# where dl.date_id >= '2019-12-01'
#   and dl.date_id < '2020-05-01'
#   and dl.vehicle_id is not null
#   and dl.vehicle_id <> ''
#   and dl.stock_type_id in ('1', '2')
#   and substring(coalesce(dl.search_zip_code_id, dl.consumer_zip_code_id), 1, 5) is not null
#   and substring(coalesce(dl.search_zip_code_id, dl.consumer_zip_code_id), 1, 5) <> 0
#   and dl.model_year_id is not null
# ;
