select
  bm.filedate,

  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then 1 else 0 end ) n_vehicles_local,
  sum( case when lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%' then 1 else 0 end ) n_vehicles_ol,

  avg( case when (not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%')) and cast(bm.preds as float) > 0 and bm.preds <> '-1' then cast(bm.preds as float) else null end ) avg_bm_local,
  avg( case when (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and cast(bm.preds as float) > 0 and bm.preds <> '-1' then cast(bm.preds as float) else null end ) avg_bm_ol,

  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'GREAT' then 1 else 0 end ) n_great_local,
  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'GOOD' then 1 else 0 end ) n_good_local,
  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'FAIR' then 1 else 0 end ) n_fair_local,

  sum( case when  (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'GREAT' then 1 else 0 end ) n_great_ol,
  sum( case when  (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'GOOD' then 1 else 0 end ) n_good_ol,
  sum( case when  (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and pb.price_badge = 'FAIR' then 1 else 0 end ) n_fair_ol,

  avg( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then cast(ia.mileage as float) else null end ) avg_mileage_local,
  avg( case when (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then cast(ia.mileage as float) else null end ) avg_mileage_ol,

  avg( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then (2021 - cast(ia.model_year as float)) else null end ) avg_age_local,
  avg( case when (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then (2021 - cast(ia.model_year as float)) else null end ) avg_age_ol

from insight.bestmatch bm
join insight.price_badge_v2 pb
  on bm.classified_ad_id = pb.classified_ad_id
  and pb.filedate = '2021-02-02'
join insight.inventory_activity ia
on bm.classified_ad_id = ia.classified_ad_id
  and bm.filedate = '2021-02-04'
  and ia.filedate = '2021-02-02'
  and bm.new_used_ind = 'used'
  and bm.model_nm = 'overall'
group by 1
order by 1 ;

select

  ia.filedate,

  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') then 1 else 0 end ) n_vehicles_local,
  sum( case when lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%' then 1 else 0 end ) n_vehicles_ol,
  sum( case when not (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and demand_90_per.n_users is not null then 1 else 0 end ) demand_90_per_local,
  sum( case when (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%') and demand_90_per.n_users is not null then 1 else 0 end ) demand_90_per_ol

from insight.inventory_activity ia
left join (
  select
    sar.filedate,
    sar.make_name,
    sar.model_name,
    sar.dma_code,
    count(distinct user_id) n_users,
    percent_rank () over(partition by sar.filedate order by n_users) percentile_value
  from insight.search_activity_raw sar
  join insight.inventory_activity ia
    on sar.classified_ad_id = ia.classified_ad_id
    and sar.filedate = ia.filedate
    and ia.new_used_ind = 'Used'
  join (
    select distinct dma_code from insight.inventory_activity ia
    where ia.filedate = '2021-02-02' and (lower(ia.customer_name) like '%vroom%' or lower(ia.customer_name) like '%carvana%')
  ) dc
  on dc.dma_code = sar.dma_code
  where sar.filedate = '2021-02-02'
    and sar.srp2vdp = 1
  group by 1, 2, 3, 4
) demand_90_per
on ia.make_name = demand_90_per.make_name
  and ia.model_name = demand_90_per.model_name
  and ia.dma_code = demand_90_per.dma_code
  and ia.filedate = demand_90_per.filedate
  and demand_90_per.percentile_value >= 0.66
where ia.new_used_ind = 'Used'
and ia.filedate = '2021-02-02'
group by 1
