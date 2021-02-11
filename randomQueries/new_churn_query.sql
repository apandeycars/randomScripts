select
  dealer_legacy_id,
  churn_result_30
from (
  select
    da.customer_id
  from insight.dealer_activity da
  join (select max(date_id) date_id from insight.dealer_activity) di
    on da.date_id = di.date_id
) da
join insight.dealer_churn_stats dc
  on da.customer_id = dc.customer_id
join (
  select
    max(date_id) as date_ids
  from (
    select
      date_id,
      sum(case when churn_result_30 = 'as is' then 1 else 0 end) as_is_count,
      count(*) total_count,
      sum(case when churn_result_30 = 'as is' then 1 else 0 end)/count(*) per_as_is
    from insight.dealer_churn_stats
    where date_id > date_sub(date_id, 30)
    group by date_id
  ) a
  where a.per_as_is >= 0.85 and per_as_is <= 0.995
) b
on dc.date_id=b.date_ids
;
