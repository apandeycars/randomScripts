select
  dl.date_id date_id,
  count(*) n_leads,
  100.000*sum(case when ci.class in ('good', 'very good') then 1 else 0 end)/count(*) per_good_leads,
  avg(ci.intent_score) avg_intent_score
from insight.consumer_intent ci
join dw.lead dl
  on dl.src_lead_id = ci.lead_id
  and dl.date_id = ci.filedate
join (
  select
      dealer_legacy_id,
      customer_id,
      da.major_account_name
  from insight.dealer_activity da
  where filedate >= '2020-01-01'
    and filedate <= '2020-08-01'
    and (lower(da.major_account_name) like '%autonation%'
      or lower(da.customer_name) like '%autonation%'
    )
  group by 1, 2, 3
) da
  on dl.customer_id = da.customer_id
where dl.date_id >= '2020-01-01'
and dl.date_id <= '2020-08-01'
and dl.vehicle_id is not null
and dl.vehicle_id <> ''
group by 1
order by 1 ;
