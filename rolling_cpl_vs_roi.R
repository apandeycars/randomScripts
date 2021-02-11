library(data.table)
library(plotly)
options(width = 150)
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
cpl_vs_roi <- setDT(read.csv("cpl_vs_roi.csv"))
cpl_vs_roi <- cpl_vs_roi[!is.na(roi_value_30_day)]

summary(cpl_vs_roi[rolling_cpl_30_day<=5, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>5 & rolling_cpl_30_day<=15, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>15 & rolling_cpl_30_day<=30, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>30 & rolling_cpl_30_day<=45, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>45 & rolling_cpl_30_day<=60, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>60 & rolling_cpl_30_day<=100, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])
summary(cpl_vs_roi[rolling_cpl_30_day>100, .(roi_value_30_day, rolling_cpl_30_day, leads_per_dollar_30_day)])

p <- plot_ly(rollingCPL) %>%
  add_lines(x = ~filedate, y = ~rolling_cpl_30_day, type = 'scatter', mode = 'lines', name = 'CPL') %>%
  layout(xaxis = list(title = "Date", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "CPL ($)"),
         title = "Rolling 30 Days CPL",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Only Local Dealers (Exclude Online/SIY)",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p




# select
#   da.filedate,
#   da.customer_name,
#   dp.total_spend_30_day,
#   dp.roi_value_30_day,
#   da.total_leads_30day - da.other_leads_30day leads_30_day,
#   da.cost_per_lead,
#   case
#     when (da.total_leads_30day - da.other_leads_30day) = 0 then dp.total_spend_30_day
#     else dp.total_spend_30_day/(da.total_leads_30day - da.other_leads_30day)
#   end as rolling_cpl_30_day,
#   (da.total_leads_30day - da.other_leads_30day)/dp.total_spend_30_day leads_per_dollar_30_day
# from insight.dealer_activity da
# join insight.dealer_predictions dp
#   on da.customer_id = dp.customer_id
#   and da.filedate =  dp.filedate
# where dp.filedate = '2020-09-01'
#   and dp.total_spend_30_day > 0
#   and da.total_spend > 0
#   and lower(da.customer_name) not like '%vroom%'
#   and lower(da.customer_name) not like '%carvana%'
#   and lower(da.major_account_name) not like '%vroom%'
#   and lower(da.major_account_name) not like '%carvana%'
# ;
