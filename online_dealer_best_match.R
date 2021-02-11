rm(list = ls())
options(scipen = 500)
options(width = 200)
options(java.parameters = c("-Xss2560k", "-Xmx16g"))
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx32768m"))
source('/app/workarea/apandey/utility/connect_to_non_prod_redshift.R')

top_n_ranking_data <- "
select
  sar.filedate,
  case
    when lower(ia.customer_name) like '%carvana%'
      or lower(ia.customer_name) like '%vroom%'

    then 'online'
    else 'local'
  end as ol_or_local,
  sum(case when sar.raw_srp_display_position = '1' then 1 else 0 end) top_1_count,
  sum(case when cast(sar.raw_srp_display_position as float) <= 5 then 1 else 0 end) top_5_count,
  1.00*sum(case when sar.raw_srp_display_position = '1' then cast(srp2vdp as float) else 0 end)/sum(case when sar.raw_srp_display_position = '1' then 1 else 0 end) top_1_srp2vdp,
  1.00*sum(case when cast(sar.raw_srp_display_position as float) <= 5 then cast(srp2vdp as float) else 0 end)/sum(case when cast(sar.raw_srp_display_position as float) <= 5 then 1 else 0 end) top_5_srp2vdp
from insight_prod.search_activity_raw sar
left join insight_prod.dealer_activity ia
  on sar.customer_id = ia.customer_id
  and sar.filedate = ia.filedate
where sar.filedate >= '2020-12-01'
  and sar.source_type in ('Organic', 'SEO', 'Email')
  and lower(sar.sort_field) like '%search%'
  and sar.src_channel_id = 'wired'
group by 1, 2
order by 1, 2
"
top_n_ranking_data <- gsub("[\r\n]", " ", top_n_ranking_data)
top_n_ranking_data <- gsub("  ", " ", top_n_ranking_data)
top_n_ranking_data <- data.table(dbGetQuery(con, top_n_ranking_data))
top_n_ranking_data_stats <- top_n_ranking_data[, .(n_top_1_occurance = sum(top_1_count), n_top_5_occurance = sum(top_5_count)), by = list(filedate)]
head(top_n_ranking_data_stats)
top_n_stats <- merge(top_n_ranking_data, top_n_ranking_data_stats, by = 'filedate')
top_n_stats <- top_n_stats[, per_top_1_count:=round(100.00*top_1_count/n_top_1_occurance, 2)]
top_n_stats <- top_n_stats[, per_top_5_count:=round(100.00*top_5_count/n_top_5_occurance, 2)]
head(top_n_stats)
scores_comparison <- "
select
  ia.filedate,
  count(*) n_vehicles,
  sum(case when bm.notes = 'pre_explore' then 1 else 0 end) n_explored_vehicles,
  sum(case when lower(ia.customer_name) like '%carvana%'
    or lower(ia.customer_name) like '%vroom%' then 1 else 0 end) n_ol_vehicles,
  sum(case when bm.notes = 'pre_explore' and lower(ia.customer_name) like '%carvana%'
    or lower(ia.customer_name) like '%vroom%' then 1 else 0 end) n_explored_ol_vehicles,
  100.00*sum(case when lower(ia.customer_name) like '%carvana%'
    or lower(ia.customer_name) like '%vroom%' then 1 else 0 end)/count(*) per_ol_vehicles,
  100.00*sum(case when bm.notes = 'pre_explore' and lower(ia.customer_name) like '%carvana%'
    or lower(ia.customer_name) like '%vroom%' then 1 else 0 end)/sum(case when bm.notes = 'pre_explore' then 1 else 0 end) per_ol_vehicles_in_explored,
  avg(case when lower(ia.customer_name) like '%carvana%' or lower(ia.customer_name) like '%vroom%'
       then cast(preds as float) else null end) on_avg_score,
  avg(case when lower(ia.customer_name) like '%carvana%' or lower(ia.customer_name) like '%vroom%'
       then null else cast(preds as float) end) local_avg_score
from insight_prod.inventory_activity ia
left join insight_prod.bestmatch bm
  on ia.classified_ad_id = bm.classified_ad_id
  and CAST(DATEADD(day, 2, CAST(ia.filedate AS DATE)) AS DATE) = bm.filedate
  and bm.model_nm = 'overall'
where ia.new_used_ind = 'Used'
  and ia.filedate >= '2020-12-01' and ia.filedate <= '2021-01-14'
  and ia.photo_count >= 1
  and cast(ia.mileage as float) > 10
group by 1
order by 1
"
scores_comparison <- gsub("[\r\n]", " ", scores_comparison)
scores_comparison <- gsub("  ", " ", scores_comparison)
scores_comparison <- data.table(dbGetQuery(con, scores_comparison))
dealership_bm_scores_agg <- "
select
  ia.filedate,
  case when da.major_account_name = 'Unknown' then coalesce(da.dealer_group_name, da.customer_name) else da.major_account_name end as customer_name,
  min(da.franchise_independent) franchise_independent,
  count(*) n_vehicles,
  avg(cast(preds as float)) avg_bm_score
from insight_prod.inventory_activity ia
inner join insight_prod.dealer_activity da
  on ia.customer_id = da.customer_id
  and ia.filedate = da.filedate
inner join insight_prod.bestmatch bm
  on ia.classified_ad_id = bm.classified_ad_id
  and CAST(DATEADD(day, 2, CAST(ia.filedate AS DATE)) AS DATE) = bm.filedate
  and bm.model_nm = 'overall'
where ia.new_used_ind = 'Used'
  and ia.filedate in ('2021-01-03', '2021-01-04')
  and ia.photo_count > 1
group by 1, 2
order by 1, 2 ;
"
dealership_bm_scores_agg <- gsub("[\r\n]", " ", dealership_bm_scores_agg)
dealership_bm_scores_agg <- gsub("  ", " ", dealership_bm_scores_agg)
dealership_bm_scores_agg <- data.table(dbGetQuery(con, dealership_bm_scores_agg))
dealer_f_i <- "
select
  case when da.major_account_name = 'Unknown' then coalesce(da.dealer_group_name, da.customer_name) else da.major_account_name end as customer_name,
  min(da.franchise_independent) franchise_independent
from insight_prod.dealer_activity da
where da.filedate in ('2021-01-03', '2021-01-04')
group by 1 ;
"
dealer_f_i <- gsub("[\r\n]", " ", dealer_f_i)
dealer_f_i <- gsub("  ", " ", dealer_f_i)
dealer_f_i <- data.table(dbGetQuery(con, dealer_f_i))
dealership_bm_scores_agg <- merge(dealership_bm_scores_agg, dealer_f_i, by = 'customer_name')
d_bm_scores_pre <- dealership_bm_scores_agg[filedate == '2021-01-03']
d_bm_scores_post <- dealership_bm_scores_agg[filedate == '2021-01-04']
d_bm_scores_pre$bm_pre <- d_bm_scores_pre$avg_bm_score
d_bm_scores_post$bm_post <- d_bm_scores_post$avg_bm_score
d_bm_scores <- merge(d_bm_scores_pre[, .(customer_name, franchise_independent, bm_pre)], d_bm_scores_post[, .(customer_name, bm_post)], by = 'customer_name')
d_bm_scores$diff <- d_bm_scores$bm_post - d_bm_scores$bm_pre
d_bm_scores <- d_bm_scores[order(diff)]

# Plot Inventory Count and % of OL Inventory by date

p <- plot_ly(scores_comparison) %>%
  add_lines(x = ~filedate, y = ~per_ol_vehicles, type = 'scatter', mode = 'lines', name = 'Purchase\nRate (PR)') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "% Online Listings", range = c(20, 30)),
         title = "% Online Listings (Carvana, Vroom, Carmax) By Date",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Dec'20 through Jan 21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(scores_comparison) %>%
  add_lines(x = ~filedate, y = ~on_avg_score, type = 'scatter', mode = 'lines', name = 'OL') %>%
  add_lines(x = ~filedate, y = ~local_avg_score, type = 'scatter', mode = 'lines', name = 'Local') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Best Match Score"),
         title = "Avg Best Match Score by Dealer Type",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Dec'20 through Jan 21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

ol_top_stats <- top_n_stats[ol_or_local == 'online']
local_top_stats <- top_n_stats[ol_or_local == 'local']

p <- plot_ly() %>%
  add_lines(data = ol_top_stats, x = ~filedate, y = ~per_top_1_count, type = 'scatter', mode = 'lines', name = 'OL') %>%
  add_lines(data = local_top_stats, x = ~filedate, y = ~per_top_1_count, type = 'scatter', mode = 'lines', name = 'Local') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Percentage"),
         title = "% of times Rank #1 was allocated to Local/Online",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Dec'20 through Jan 21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly() %>%
  add_lines(data = ol_top_stats, x = ~filedate, y = ~per_top_5_count, type = 'scatter', mode = 'lines', name = 'OL') %>%
  add_lines(data = local_top_stats, x = ~filedate, y = ~per_top_5_count, type = 'scatter', mode = 'lines', name = 'Local') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Percentage"),
         title = "% of times Rank #1-#5 was allocated to Local/Online",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "Data from Dec'20 through Jan 21",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

d_bm_scores_pre$avg_bm_score <- d_bm_scores_pre$bm_pre
d_bm_scores_post$avg_bm_score <- d_bm_scores_pre$bm_post
d_bm_scores <- merge(d_bm_scores_pre[, .(customer_name, bm_pre)], d_bm_scores_post[, .(customer_name, bm_post)], by = 'customer_name')
