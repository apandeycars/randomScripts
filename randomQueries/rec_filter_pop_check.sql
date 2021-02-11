select
  filedate,
  variation_name,
  count(*) n_usrs
from (
select
  device,
  filedate,
  variation_name,
  src_user_id
from (
select
  aawr.*,
  asr.src_sid,
  asr.src_user_id
from (
  select
    aawr.filedate,
    aawr.post_evar64 variation_name,
    aawr.post_evar16,
    aawr.post_evar37 device,
    aawr.post_channel search_type
  from adobe_analytics_dr.adobe_analytics_wired_raw aawr
  where aawr.filedate >= '2019-08-28'
    and aawr.post_evar64 in ('cars-recfilters-control', 'cars-recfilters-v2', 'cars-recfilters-v3', 'cars-recfilters-v4')
    and aawr.exclude_hit <= '0'
    and aawr.post_evar16<>''
    and aawr.hit_source NOT IN('5','7','8','9')
    and aawr.post_channel like '%Search%'
    and aawr.post_prop8 = 'srp'
  group by 1, 2, 3, 4, 5
) aawr
join activity_enrich_dr.als_search_enrich asr
  on asr.src_sid = aawr.post_evar16
where asr.filedate >= '2019-08-28'
) a
) b group by 1 , 2
