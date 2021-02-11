library(data.table)
library(plotly)
options(width = 150)
setwd("/Users/addhyanpandey/Desktop/randomRcodes/datasets/")
hprClicks <- setDT(read.csv("recSysEngData.csv"))
hprClicks <- hprClicks[, filedate:= as.Date(filedate)]

hprClicks$filedate <- as.Date(hprClicks$filedate)
hprClicks <- hprClicks[order(filedate)]
hprClicks$n_wired_views <- hprClicks$n_wired_used_vdp_views + hprClicks$n_wired_new_vdp_views
hprClicks$n_app_views <- hprClicks$n_app_used_vdp_views + hprClicks$n_app_new_vdp_views
hprClicks$n_used_views <- hprClicks$n_wired_used_vdp_views + hprClicks$n_app_used_vdp_views
hprClicks$n_new_views <- hprClicks$n_wired_new_vdp_views + hprClicks$n_app_new_vdp_views
hprClicks$ctr_app <- 100*hprClicks$n_app_views/hprClicks$n_app_impressions
hprClicks$ctr_android <- 100*hprClicks$n_android_vdp_views/hprClicks$n_android_impressions
hprClicks$ctr_iphone <- 100*hprClicks$n_iphone_vdp_views/hprClicks$n_iphone_impressions
hprClicks$ctr_wired <- 100*hprClicks$n_wired_views/hprClicks$n_site_impressions
t(head(hprClicks))

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)

p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~t_vdp_views, type = 'scatter', mode = 'lines', name = 'HPR VDPs') %>%
  add_lines(x = ~filedate, y = ~t_revenue, name = "Revenue", yaxis = "y2") %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "VDP Views", range = c(5000, max(hprClicks$t_vdp_views) + 5000)),
         yaxis2 = list(
           tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "Proxy Revenue (USD)",
           range = c(1000, max(hprClicks$t_revenue) + 1000)
         ),
         title = "VDP Clicks andRevenue from HPR by Date",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Revenue = Sum (Rolling Cost per VDP View) across groups",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~n_iphone_vdp_views, type = 'scatter', mode = 'lines', name = 'iPhone VDPs') %>%
  add_lines(x = ~filedate, y = ~n_android_vdp_views, name = "android VDPs") %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "VDP Views", range = c(0, max(hprClicks$n_iphone_vdp_views))),
         title = "VDP Clicks from Homepage across different Apps",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from ALS Click Enrich",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~n_wired_views, type = 'scatter', mode = 'lines', name = 'Wired VDPs') %>%
  add_lines(x = ~filedate, y = ~n_app_views, name = "Mobile VDPs", name = "Mobile VDPs") %>%
  layout(xaxis = list(title = "Date", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "VDP Views"),
         title = "VDP Clicks from HPR by Date and Device",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Mobile means all Apps",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~n_used_views, type = 'scatter', mode = 'lines', name = 'Used VDPs') %>%
  add_lines(x = ~filedate, y = ~n_new_views, name = "New VDPs", name = "New VDPs") %>%
  layout(xaxis = list(title = "Date", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "VDP Views"),
         title = "VDP Clicks from HPR by Date and Stock Type",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from ALS Click Enrich",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p


p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~n_iphone_vdp_views, type = 'scatter', mode = 'lines', name = 'iPhone VDPs') %>%
  add_lines(x = ~filedate, y = ~n_android_vdp_views, name = "android VDPs") %>%
  layout(xaxis = list(title = "Date", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "VDP Views", range = c(0, 50000)),
         title = "VDP Clicks from Homepage across different Apps",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from ALS Click Enrich",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p


p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~ctr_app, type = 'scatter', mode = 'lines', name = 'App') %>%
  add_lines(x = ~filedate, y = ~ctr_wired, type = 'scatter', mode = 'lines', name = 'Wired') %>%
  add_lines(x = ~filedate, y = ~ctr_android, type = 'scatter', mode = 'lines', name = 'Android') %>%
  add_lines(x = ~filedate, y = ~ctr_iphone, type = 'scatter', mode = 'lines', name = 'iPhone') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "CTR", range = c(0, 8)),
         title = "CTR Across Devices on Recommendations",
         annotations = list(x = 1, y = -0, text = "Data from ALS Click Enrich",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(hprClicks) %>%
  add_lines(x = ~filedate, y = ~n_iphone_impressions, type = 'scatter', mode = 'lines', name = 'iPhone') %>%
  add_lines(x = ~filedate, y = ~n_site_impressions, type = 'scatter', mode = 'lines', name = 'Wired') %>%
  add_lines(x = ~filedate, y = ~n_android_impressions, type = 'scatter', mode = 'lines', name = 'Android') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Total Views"),
         title = "Total Views Based on Category",
         annotations = list(x = 1, y = -0, text = "Data from ALS Click Enrich",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

# Reason for Poor Engagement -> No new listings
invData <- setDT(read.csv("invData.csv"))
invData <- invData[, filedate:= as.Date(filedate)]
invData <- invData[order(filedate)]

p <- plot_ly(invData) %>%
  add_lines(x = ~filedate, y = ~n_listings, type = 'scatter', mode = 'lines', name = 'Total') %>%
  add_lines(x = ~filedate, y = ~n_used_listings, type = 'scatter', mode = 'lines', name = 'Used') %>%
  add_lines(x = ~filedate, y = ~n_new_listings, type = 'scatter', mode = 'lines', name = 'New') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(0, 5000000)),
         title = "Total Listings With Time"
  )
p

p <- plot_ly(invData) %>%
  add_lines(x = ~filedate, y = ~n_vins, type = 'scatter', mode = 'lines', name = 'Total') %>%
  add_lines(x = ~filedate, y = ~n_used_vins, type = 'scatter', mode = 'lines', name = 'Used') %>%
  add_lines(x = ~filedate, y = ~n_new_vins, type = 'scatter', mode = 'lines', name = 'New') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(0, 4000000)),
         title = "Total Vins With Time"
  )
p

# Checking Demand Data to Access Conversion Across
demandData <- setDT(read.csv("demandData.csv"))
demandData <- demandData[, filedate:= as.Date(filedate)]
demandData <- demandData[order(filedate)]
demandData$ctr_search_page <- 100*demandData$n_vdp_views/demandData$n_srp_views
demandData$lead_to_viewability <- 100*demandData$n_leads/demandData$n_srp_views
demandData$per_used_users <- 100*demandData$n_used_users/demandData$n_users
demandData$per_new_users <- 100*demandData$n_new_users/demandData$n_users
t(head(demandData))

p <- plot_ly(demandData[ctr_search_page > 0]) %>%
  add_lines(x = ~filedate, y = ~ctr_search_page, type = 'scatter', mode = 'lines', name = 'CTR - SRP') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(4, 7)),
         title = "CTR on the SRP"
  )
p

p <- plot_ly(demandData[per_used_users > 0]) %>%
  add_lines(x = ~filedate, y = ~per_used_users, type = 'scatter', mode = 'lines', name = 'Used') %>%
  add_lines(x = ~filedate, y = ~per_new_users, type = 'scatter', mode = 'lines', name = 'New') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(0, 100)),
         title = "% Users who see at least one New/Used Vehicle"
  )
p

conversionData <- setDT(read.csv("conversionData.csv"))
conversionData <- conversionData[, filedate:= as.Date(filedate)]
conversionData <- conversionData[order(filedate)]
conversionData$visitor_vdp_view_wired <- 100*conversionData$n_users_with_vdp_view_wired/conversionData$n_users_wired
conversionData$visitor_lead_wired <- 100*conversionData$n_users_with_lead_wired/conversionData$n_users_wired

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)
conversionData <- conversionData[mean_srp2vdp_wired > 0]
conversionData <- conversionData[mean_srp2vdp_app > 0]
p <- plot_ly(conversionData) %>%
  add_lines(x = ~filedate, y = ~mean_srp2vdp_wired, type = 'scatter', mode = 'lines', name = 'Wired') %>%
  add_lines(x = ~filedate, y = ~mean_srp2vdp_app, type = 'scatter', mode = 'lines', yaxis = "y2", name = 'App') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "CTR on the SRP for Wired", range = c(.03, .04)),
         yaxis2 = list(
           tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "CTR on the SRP for App",
           range = c(0.07, 0.09)
         ),
         title = "VDPs per View aka CTR on the SRP By Devices"
  )
p

p <- plot_ly(conversionData[visitor_lead > 0 & visitor_vdp_view > 0]) %>%
  add_lines(x = ~filedate, y = ~n_users_with_lead_wired, type = 'scatter', mode = 'lines', name = '% Users with \n a Lead') %>%
  add_lines(x = ~filedate, y = ~n_users_with_vdp_view_wired, type = 'scatter', mode = 'lines', yaxis = "y2", name = '% Users with \n VDP View') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Lead Conversion", range = c(2, 6)),
         yaxis2 = list(
           tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "VDP View Conversion",
           range = c(40, 65)
         ),
         title = "% Users who see at least one VDP/make one connection \n Wired Only"
  )
p

impPerUser <- setDT(read.csv("impPerUser_android.csv"))
impPerUser <- impPerUser[, filedate:= as.Date(filedate)]
impPerUser <- impPerUser[order(filedate)]

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)

p <- plot_ly(impPerUser) %>%
  add_lines(x = ~filedate, y = ~vehicles_per_user, type = 'scatter', mode = 'lines', name = 'Vehicles \nPer User') %>%
  add_lines(x = ~filedate, y = ~android_imps_per_user, type = 'scatter', mode = 'lines', yaxis = "y2", name = 'Imps \nPer User') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Vehicles per User", range = c(2, 7)),
         yaxis2 = list(
           tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "Imps per User",
           range = c(4, 9)
         ),
         title = "Recommendations being made per user\n Android Only"
  )
p

p <- plot_ly(impPerUser) %>%
  add_lines(x = ~filedate, y = ~n_users, type = 'scatter', mode = 'lines', name = 'Used') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(8000, 25000)),
         title = "Users who see at least one Recommendation (Android Only)"
  )
p

impPerUser <- setDT(read.csv("impPerUser_iphone.csv"))
impPerUser <- impPerUser[, filedate:= as.Date(filedate)]
impPerUser <- impPerUser[order(filedate)]

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "second y axis"
)

p <- plot_ly(impPerUser) %>%
  add_lines(x = ~filedate, y = ~vehicles_per_user, type = 'scatter', mode = 'lines', name = 'Vehicles \nPer User') %>%
  add_lines(x = ~filedate, y = ~iphone_imps_per_user, type = 'scatter', mode = 'lines', yaxis = "y2", name = 'Imps \nPer User') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Vehicles per User", range = c(2, 7)),
         yaxis2 = list(
           tickfont = list(color = "red"),
           overlaying = "y",
           side = "right",
           title = "Imps per User",
           range = c(8, 17)
         ),
         title = "Recommendations being made per user\n iPhone Only"
  )
p

p <- plot_ly(impPerUser) %>%
  add_lines(x = ~filedate, y = ~n_users, type = 'scatter', mode = 'lines', name = 'Used') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Count", range = c(50000, 120000)),
         title = "Users who see at least one Recommendation (iPhone Only)"
  )
p

# recSysEngData.csv
# select
#   ct.filedate filedate,
#   aie.n_impressions,
#   aie.n_app_impressions,
#   aie.n_site_impressions,
#   aie.n_android_impressions,
#   aie.n_iphone_impressions,
#   sum(da.cpv_30) t_revenue,
#   avg(cast(ct.price as float)) mean_price,
#   count(*) t_vdp_views,
#   sum(case when ct.device_type in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'U' then da.cpv_30 else 0 end) sum_wired_used_revenue,
#   sum(case when ct.device_type not in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'U' then da.cpv_30 else 0 end) sum_app_used_revenue,
#   sum(case when ct.device_type in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'N' then da.cpv_30 else 0 end) sum_wired_new_revenue,
#   sum(case when ct.device_type not in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'N' then da.cpv_30 else 0 end) sum_app_new_revenue,
#   sum(case when ct.device_type in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'U' then 1 else 0 end) n_wired_used_vdp_views,
#   sum(case when ct.device_type not in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'U' then 1 else 0 end) n_app_used_vdp_views,
#   sum(case when ct.device_type in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'N' then 1 else 0 end) n_wired_new_vdp_views,
#   sum(case when ct.device_type not in ('tablet', 'wired') and ct.src_vehicle_stock_typ = 'N' then 1 else 0 end) n_app_new_vdp_views,
#   sum(case when ct.src_channel_id in ('carsappl') then 1 else 0 end) n_iphone_vdp_views,
#   sum(case when ct.src_channel_id in ('carsandr') then 1 else 0 end) n_android_vdp_views
# from enrich.als_clickthru_enrich ct
# left join (
#     select
#         aie.filedate,
#         count(*) n_impressions,
#         sum(case when aie.src_channel_id in ('carsappl', 'carsandr') then 1 else 0 end) n_app_impressions,
#         sum(case when aie.src_channel_id not in ('carsappl', 'carsandr') then 1 else 0 end) n_site_impressions,
#         sum(case when aie.src_channel_id in ('carsandr') then 1 else 0 end) n_android_impressions,
#         sum(case when aie.src_channel_id in ('carsappl') then 1 else 0 end) n_iphone_impressions
#     from enrich.als_impression_enrich aie
#     where filedate >= '2020-07-01'
#         and aie.web_page_type_name in ('Recommended Vehicles Listing', 'Recommended Vehicles Module')
#     group by aie.filedate
# ) aie
#   on ct.filedate = aie.filedate
# left join (
#   select
#     customer_id,
#     filedate,
#     case
#       when cast(total_spend as float) <= 10 then 0.25
#       when cast(total_spend as float) > 10 and (vdp_new_30day + vdp_used_30day) < 1 then 0.25
#       else cast(total_spend as float)/(vdp_new_30day + vdp_used_30day)
#     end as cpv_30
#   from insight.dealer_activity
# ) da
#   on ct.customer_id = da.customer_id
#   and ct.filedate = da.filedate
# where ct.filedate >= '2020-07-01'
# and lower(ct.web_page_type_from_name) like '%recommended%' and lower(ct.web_page_type_to_name) like '%more details page%'
# group by 1, 2, 3, 4, 5, 6
# order by ct.filedate ;

# -- Demand
# select
#   sar.filedate,
#   count(distinct sar.user_id) n_users,
#   count(*) n_srp_views,
#   sum(sar.srp2vdp) n_vdp_views,
#   sum(sar.sublead) n_leads,
#   sum(case when ia.new_used_ind = 'New' then 1 else 0 end) n_new_srp_views,
#   sum(case when ia.new_used_ind = 'Used' then 1 else 0 end) n_used_srp_views,
#   count(distinct case when ia.new_used_ind = 'Used' then sar.user_id else null end) n_used_users,
#   count(distinct case when ia.new_used_ind = 'New' then sar.user_id else null end) n_new_users
# from insight.search_activity_raw sar
# join insight.inventory_activity ia
#   on sar.filedate = ia.filedate
#   and sar.classified_ad_id = ia.classified_ad_id
# where sar.filedate >= '2020-01-01'
# group by sar.filedate
# order by sar.filedate ;
#
# -- Supply
# select
#   ia.new_used_ind,
#   count(*) n_rows
# from insight.search_activity_raw sar
# join insight.inventory_activity ia
#   on sar.filedate = ia.filedate
#   and sar.classified_ad_id = ia.classified_ad_id
# where sar.filedate = '2020-01-01'
# group by 1 ;

# -- Conversion Rate Data
# select
#   sar.filedate,
#   count(distinct case when src_channel_id = 'wired' then sar.user_id else null end) n_users_wired,
#   count(distinct sar.user_id) n_users,
#   1.00*sum(case when src_channel_id = 'wired' then cast(sublead as float) else 0 end)/sum(case when src_channel_id = 'wired' then 1 else 0 end) mean_sublead_wired,
#   1.00*sum(case when src_channel_id = 'wired' then cast(srp2vdp as float) else 0 end)/sum(case when src_channel_id = 'wired' then 1 else 0 end) mean_srp2vdp_wired,
#   1.00*sum(case when src_channel_id <> 'wired' then cast(srp2vdp as float) else 0 end)/sum(case when src_channel_id <> 'wired' then 1 else 0 end) mean_srp2vdp_app,
#   count(distinct case when srp2vdp = 1 and src_channel_id = 'wired' then sar.user_id else null end) n_users_with_vdp_view_wired,
#   count(distinct case when sublead = 1 and src_channel_id = 'wired' then sar.user_id else null end) n_users_with_lead_wired
# from insight.search_activity_raw sar
# where sar.filedate >= '2020-01-01'
# group by sar.filedate
# order by sar.filedate
# ;


# impPerUser_iphone.csv
# select
#   filedate,
#   count(*) n_users,
#   1.00*sum(n_unique_vehicles)/count(*) vehicles_per_user,
#   1.00*sum(n_iphone_impressions)/count(*) iphone_imps_per_user
# from (
#   select
#     src_user_id,
#     filedate,
#     count(distinct src_classified_ad_id) n_unique_vehicles,
#     sum(case when aie.src_channel_id in ('carsappl') then 1 else 0 end) n_iphone_impressions
#   from enrich.als_impression_enrich aie
#   where filedate >= '2020-01-01'
#     and aie.web_page_type_name in ('Recommended Vehicles Listing')
#     and aie.src_channel_id in ('carsappl')
#   group by 1, 2
# ) aie
# group by 1
# order by 1 ;

# impPerUser_android.csv
# select
#   filedate,
#   count(*) n_users,
#   1.00*sum(n_unique_vehicles)/count(*) vehicles_per_user,
#   1.00*sum(n_iphone_impressions)/count(*) iphone_imps_per_user
# from (
#   select
#     src_user_id,
#     filedate,
#     count(distinct src_classified_ad_id) n_unique_vehicles,
#     sum(case when aie.src_channel_id in ('carsappl') then 1 else 0 end) n_iphone_impressions
#   from enrich.als_impression_enrich aie
#   where filedate >= '2020-01-01'
#     and aie.web_page_type_name in ('Recommended Vehicles Listing')
#     and aie.src_channel_id in ('carsappl')
#   group by 1, 2
# ) aie
# group by 1
# order by 1 ;
