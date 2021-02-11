SELECT
  dfp.filedate,
  sar.user_id,
  sar.n_srp2vdp,
  sar.n_sublead,
  sar.n_srp_viewability,
  dfp.n_dfp_impressions
from (
  select
    dfp.*
  from activity_enrich_dr.dfp_impression_enrich as dfp
  where filedate = '2020-01-01'
    and dfp.recordstatus = 'Accepted'
    and dfp.pdid = '1355911185678453781622512785338527'
--  group by dfp.pdid, dfp.filedate
) dfp
join (
  select
    sar.user_id,
    sar.date_id,
    sum(cast(srp2vdp as float)) n_srp2vdp,
    sum(cast(sublead as float)) n_sublead,
    count(*) n_srp_viewability
  from consumer_dr.search_activity_raw sar
  where date_id in ('2019-12-31', '2020-01-01', '2020-01-02')
    and sar.user_id in ('1355911185678453781622512785338527', '1355911189990263034887560997178982', '1355911190764179760827991337077674')
  group by sar.user_id, sar.date_id
) sar
  on sar.user_id = dfp.pdid
  and sar.date_id = dfp.filedate
limit 100;

select date_id, count(*) from consumer_dr.search_activity_raw sar
where sar.date_id >= '2019-12-10'
group by 1 order by 1 ;
