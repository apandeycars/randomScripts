rm(list = ls())
options(scipen = 500)
options(width = 200)
options(java.parameters = c("-Xss2560k", "-Xmx16g"))
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx32768m"))
source('/app/workarea/apandey/utility/connect_to_non_prod_redshift.R')

bm_performance_data <- "select
  sar.filedate,
  lower(ia.new_used_ind) new_used_ind,
  case when sar.source_category = 'Paid' then 'paid' else 'organic' end as traffic_source,
  case
    when sar.src_channel_id = 'wired' and sar.mobile_ind = 'Yes' then 'wired_mobile'
    when sar.src_channel_id = 'wired' and sar.mobile_ind = 'No' then 'desktop'
    else 'app'
  end as device_type,
  1.00*sum(cast(sar.srp2vdp as float))/count(*) avg_srp2vdp,
  1.00*sum(cast(sar.sublead as float))/sum(cast(sar.srp2vdp as float)) avg_lead_per_vdpview,
  100.00*sum(cast(sar.sublead as float))/count(*) avg_lead_per_100_srpview
from insight_prod.search_activity_raw sar
join insight_prod.inventory_activity ia
  on sar.filedate = ia.filedate
  and sar.classified_ad_id = ia.classified_ad_id
where sar.filedate >= '2020-12-15'
  and lower(sar.sort_field) like '%search%'
group by 1, 2, 3, 4
order by 1, 2, 3, 4
"
bm_performance_data <- gsub("[\r\n]", " ", bm_performance_data)
bm_performance_data <- gsub("  ", " ", bm_performance_data)
bm_performance_data <- data.table(dbGetQuery(con, bm_performance_data))

bm_performance_app_used <- bm_performance_data[new_used_ind == 'used' & device_type == "app" & traffic_source == 'organic']
bm_performance_app_new <- bm_performance_data[new_used_ind == 'new' & device_type == "app" & traffic_source == 'organic']

bm_performance_mobile_used_organic <- bm_performance_data[new_used_ind == 'used' & device_type == "wired_mobile" & traffic_source == 'organic']
bm_performance_mobile_used_paid <- bm_performance_data[new_used_ind == 'used' & device_type == "wired_mobile" & traffic_source == 'paid']
bm_performance_mobile_new_organic <- bm_performance_data[new_used_ind == 'new' & device_type == "wired_mobile" & traffic_source == 'organic']
bm_performance_mobile_new_paid <- bm_performance_data[new_used_ind == 'new' & device_type == "wired_mobile" & traffic_source == 'paid']

bm_performance_desktop_used_organic <- bm_performance_data[new_used_ind == 'used' & device_type == "desktop" & traffic_source == 'organic']
bm_performance_desktop_used_paid <- bm_performance_data[new_used_ind == 'used' & device_type == "desktop" & traffic_source == 'paid']
bm_performance_desktop_new_organic <- bm_performance_data[new_used_ind == 'new' & device_type == "desktop" & traffic_source == 'organic']
bm_performance_desktop_new_paid <- bm_performance_data[new_used_ind == 'new' & device_type == "desktop" & traffic_source == 'paid']

p <- plot_ly() %>%
  add_lines(data = bm_performance_app_new, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'App-Used') %>%
  add_lines(data = bm_performance_app_used, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'App-New') %>%
  add_lines(data = bm_performance_mobile_used_organic, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'Mobile-Used-Organic') %>%
  add_lines(data = bm_performance_mobile_used_paid, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'Mobile-Used-Paid') %>%
  add_lines(data = bm_performance_mobile_new_organic, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'Mobile-New-Organic') %>%
  add_lines(data = bm_performance_mobile_new_paid, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'Mobile-New-Paid') %>%
  add_lines(data = bm_performance_desktop_used_organic, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'desktop-Used-Organic') %>%
  add_lines(data = bm_performance_desktop_used_paid, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'desktop-Used-Paid') %>%
  add_lines(data = bm_performance_desktop_new_organic, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'desktop-New-Organic') %>%
  add_lines(data = bm_performance_desktop_new_paid, x = ~filedate, y = ~avg_srp2vdp, type = 'scatter', mode = 'lines', hoverinfo = 'text', text = ~paste('</br> Stock: ', new_used_ind, '</br> : Traffic', traffic_source, '</br> Device: ', device_type, '</br> : SRP2VDP', avg_srp2vdp), name = 'desktop-New-Paid') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Percentage"),
         title = "CTR and Conversion on the SRP/VDP of Best Match
          \n by Stock Type, Platform and Traffic Source",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Dec'20 through Jan 21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p
