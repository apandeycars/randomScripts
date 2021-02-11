select
  filedate,
  100.00*sum(case when lower(sort_field) like '%search%' then 1 else 0 end)/count(*) per_bm_srp_views,
  100.00*sum(case when lower(sort_field) like '%search%' then cast(srp2vdp as float) else 0 end)/sum(cast(srp2vdp as float)) per_bm_vdps,
  100.00*sum(case when lower(sort_field) like '%search%' then cast(sublead as float) else 0 end)/sum(cast(sublead as float)) per_bm_leads
from insight_prod.search_activity_raw sar
where filedate >= '2020-10-01'
and src_channel_id = 'wired'
group by 1
order by 1 ;
