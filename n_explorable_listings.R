

n_explorable_listings <- "
  select
    customer_id,
    sum(n_vehicles) n_vehicles,
    count(*) n_explorable,
    sum(case when n_explored >= 1 then 1 else 0 end) n_explored,
    100.00*sum(case when n_explored >= 1 then 1 else 0 end) / sum(n_vehicles) per_explored
  from (
    select
      ia.make_name,
      ia.model_name,
      ia.customer_id,
      ia.trim_name,
      count(*) n_vehicles,
      sum(case when ia.photo_count >= 2 and cast(ia.price as float) > 100
        and (sarv.wired_organic_desktop_sublead_5 + sarv.wired_organic_mobile_sublead_5) < 4
        and (sarv.wired_organic_desktop_srp2vdp_5_viewed + sarv.wired_organic_mobile_srp2vdp_5_viewed) <= 80
        then 1 else 0 end
      ) n_explored
    from insight_prod.inventory_activity ia
    join insight_prod.agg_search_activity_raw_v2 sarv
      on ia.classified_ad_id = sarv.classified_ad_id
      and sarv.filedate = '2021-02-01'
    where ia.classified_ad_status_id = 0
      and ia.new_used_ind = 'Used'
      and not (lower(ia.customer_name) like '%carvana%' or lower(ia.customer_name) like '%vroom%')
      and ia.filedate = '2021-02-01'
    group by 1, 2, 3, 4
  ) a
  group by 1
"
n_explorable_listings <- gsub("[\r\n]", " ", n_explorable_listings)
n_explorable_listings <- gsub("  ", " ", n_explorable_listings)
n_explorable_listings <- data.table(dbGetQuery(con, n_explorable_listings))

library(data.table)
n_explorable_listings <- setDT(read.csv("/Users/addhyanpandey/Downloads/explorable_inventory_share.csv"))
n_explorable_listings <- n_explorable_listings[, per_explored:=round(100.00*n_explored/n_vehicles, 2)]
n_explorable_listings <- n_explorable_listings[, per_explored_used:=round(100.00*n_explored_used/n_used_vehicles, 2)]
n_explorable_listings <- n_explorable_listings[, per_explored_new:=round(100.00*n_explored_new/n_new_vehicles, 2)]
n_explorable_listings <- n_explorable_listings[, per_distinct_mmts:=round(100.00*n_distinct_mmts/n_vehicles, 2)]

par(mfrow=c(3,1))
hist(n_explorable_listings[per_explored > 0 & per_distinct_mmts < 25 & n_vehicles > 20, per_explored], main = "Distribution of % Explored Listings For a Dealership", xlab = "% Value", breaks = 100)
hist(n_explorable_listings[per_explored_used > 0 & per_distinct_mmts < 25 & n_vehicles > 20, per_explored_used], main = "Distribution of % USED Explored Listings For a Dealership", xlab = "% Value", breaks = 100)
hist(n_explorable_listings[per_explored_new > 0 & per_distinct_mmts < 25 & n_vehicles > 20, per_explored_new], main = "Distribution of % NEW Explored Listings For a Dealership", xlab = "% Value", breaks = 100)

par(mfrow=c(3,1))
hist(n_explorable_listings[, per_explored], main = "Distribution of % Explored Listings For a Dealership", xlab = "% Value", breaks = 100)
hist(n_explorable_listings[, per_explored_used], main = "Distribution of % USED Explored Listings For a Dealership", xlab = "% Value", breaks = 100)
hist(n_explorable_listings[, per_explored_new], main = "Distribution of % NEW Explored Listings For a Dealership", xlab = "% Value", breaks = 100)

hist(n_explorable_listings[, n_vehicles], main = "Distribution of Total Listings For a Dealership", xlab = "Total Vehicles", breaks = 100)

# select
#   ia.customer_id,
#   ia.customer_name,
#   count(*) n_vehicles,
#   sum(case when bm.new_used_ind = 'used' then 1 else 0 end) n_used_vehicles,
#   sum(case when bm.new_used_ind <> 'used' then 1 else 0 end) n_new_vehicles,
#   sum(case when notes = 'pre_explore' then 1 else 0 end) n_explored,
#   sum(case when notes = 'pre_explore' and bm.new_used_ind = 'used' then 1 else 0 end) n_explored_used,
#   sum(case when notes = 'pre_explore' and bm.new_used_ind <> 'used' then 1 else 0 end) n_explored_new
# from insight_prod.bestmatch bm
# join insight_prod.inventory_activity ia
#   on bm.classified_ad_id = ia.classified_ad_id
#   and CAST(DATEADD(day, 2, CAST(ia.filedate AS DATE)) AS DATE) = bm.filedate
# where bm.model_nm = 'overall'
#   and bm.filedate = '2021-02-09'
#   and ia.filedate = '2021-02-07'
#   and not (ia.customer_name like '%carvana%' or ia.customer_name like '%vroom%')
#   and ia.classified_ad_status_id = 0
# group by 1, 2

library(data.table)
library(plotly)

per_ol_listings <- setDT(read.csv("/Users/addhyanpandey/Downloads/per_ol_listings.csv"))
per_ol_listings$filedate <- as.Date(as.character(per_ol_listings$filedate))
per_ol_listings$per_ol <- round(100.00*per_ol_listings$n_ol_vehicles/per_ol_listings$n_vehicles, 0)

data_to_plot <- per_ol_listings[filedate >= '2020-01-18']
p <- plot_ly() %>%
  add_lines(data = data_to_plot, x = ~filedate, y = ~per_ol_active, type = 'scatter', mode = 'lines', name = '% Active') %>%
  add_lines(data = data_to_plot, x = ~filedate, y = ~n_ol_active_vehicles, type = 'scatter', mode = 'lines', yaxis = "y2", name = 'Total OL\nListings') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "% Online Listings", range = c(3, 28)),
         yaxis2 = list(
         tickfont = list(color = "black"),
         overlaying = "y",
         side = "right",
         title = "Total OL Inventory"
         ),
         title = "% Online (Carvana & Vroom) Listings out of Total USED Listings",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan'20 through Feb'21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p
#
# select
#   filedate,
#   count(*) n_vehicles,
#   sum(case when classified_ad_status_id = 0 then 1 else 0 end) n_vehicles_active,
#   sum( case when classified_ad_status_id = 0 and (lower(customer_name) like '%vroom%' or lower(customer_name) like '%carvana%') then 1 else 0 end) n_ol_active_vehicles,
#   sum( case when (lower(customer_name) like '%vroom%' or lower(customer_name) like '%carvana%') then 1 else 0 end) n_ol_vehicles,
#   100.00*sum( case when classified_ad_status_id = 0 and (lower(customer_name) like '%vroom%' or lower(customer_name) like '%carvana%') then 1 else 0 end)/sum(case when classified_ad_status_id = 0 then 1 else 0 end) per_ol_active
# from insight.inventory_activity
# where filedate >= '2019-01-01'
# and new_used_ind = 'Used'
# group by 1
# having n_vehicles_active > 1000000
# order by 1 ;


library(data.table)
library(plotly)


ia_da_vroom_vdps <- setDT(read.csv("/Users/addhyanpandey/Downloads/ia_da_vroom_vdps.csv"))
ia_da_vroom_vdps$filedate <- as.Date(as.character(ia_da_vroom_vdps$filedate))
ia_da_vroom_vdps$diff <- ia_da_vroom_vdps$n_vdps_da - ia_da_vroom_vdps$n_vdps_ia

data_to_plot <- ia_da_vroom_vdps[order(filedate)]
p <- plot_ly() %>%
  add_lines(data = ia_da_vroom_vdps, x = ~filedate, y = ~diff, type = 'scatter', mode = 'lines', name = 'Difference') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Difference in VDP Views"),
         title = "Difference in VDP Views for Vroom in IA and DA",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Jan'20 through Feb'21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

# select
#   ia.filedate,
#   sum(ia.n_vdps) n_vdps_ia,
#   sum(da.vdp_used) n_vdps_da
# from (
#   select
#     ia.customer_id,
#     ia.filedate,
#     sum(ia.total_vdp_impressions) n_vdps
#   from insight.inventory_activity ia
#   where ia.filedate >= '2021-01-01'
#   group by 1, 2
# ) ia
# join (
#   select
#     da.customer_id,
#     da.filedate,
#     da.vdp_used
#   from insight.dealer_activity da
#   where da.filedate >= '2021-01-01'
#     and (lower(da.customer_name) like 'vroom%' or lower(da.major_account_name) like 'vroom%')
# ) da
#   on ia.customer_id = da.customer_id
#   and ia.filedate = da.filedate
# group by 1 order by 1 ;
