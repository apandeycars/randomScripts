select
  sar.filedate,
  count(distinct bm.classified_ad_id) n_vehicles,
  count(distinct case when (lower(da.customer_name) like '%carvana%' or lower(da.customer_name) like '%vroom%') then bm.classified_ad_id else null end) n_vehicles,
  count(*) n_srp_views,
  sum(cast(srp2vdp as float)) n_vdps,
  sum(case when (lower(da.customer_name) like '%carvana%' or lower(da.customer_name) like '%vroom%') then 1 else 0 end) n_ol,
  sum(case
    when (lower(da.customer_name) like '%carvana%' or lower(da.customer_name) like '%vroom%')
    and lower(sort_field) like '%searchrank%'
    then 1 else 0 end
  ) n_ol_bm,
  sum(case when (lower(da.customer_name) like '%carvana%' or lower(da.customer_name) like '%vroom%') then cast(srp2vdp as float) else 0 end) n_ol_vdp,
  sum(case
    when (lower(da.customer_name) like '%carvana%' or lower(da.customer_name) like '%vroom%')
    and lower(sort_field) like '%searchrank%'
    then cast(srp2vdp as float) else 0 end
  ) n_ol_bm_vdp
from insight_prod.search_activity_raw sar
join insight_prod.bestmatch bm
  on sar.classified_ad_id = bm.classified_ad_id
  and sar.filedate = bm.filedate
join insight_prod.dealer_activity da
  on sar.customer_id = da.customer_id
  and sar.filedate = da.filedate
  and bm.model_nm = 'overall'
where sar.filedate >= '2021-01-15'
group by 1 order by 1
;
