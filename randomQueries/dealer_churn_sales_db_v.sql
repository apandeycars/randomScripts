select
  dcr.date_id,
  dcr.dealer_legacy_id,
  dcr_stats.n_days_data,
  dcr_stats.sum_risky_days,
  dcr_stats.mean_30d_risk_score,
  dcr.reasoning churn_result_30,
  da.n_total_spend
from (
  select
    da.customer_id,
    count(distinct da.total_spend) n_total_spend
  from insight.dealer_activity da
  join (
    select
      max(date_id) date_id
    from insight.dealer_activity
  ) di on da.date_id = di.date_id
  where da.days_until_cancellation = 0
  group by da.customer_id
) da
join insight.dealer_churn_reason dcr
  on da.customer_id = dcr.customer_id
  and dcr.time_period = 30
join (
  select
    max(date_id) as date_ids
  from (
    select
      date_id,
      sum(case when reasoning = 'as is' then 1 else 0 end) as_is_count,
      count(*) total_count,
      sum(case when reasoning = 'as is' then 1 else 0 end)/count(*) per_as_is
    from insight.dealer_churn_reason
    where date_id > date_sub(CURRENT_DATE, 30)
      and time_period = 30
    group by date_id
  ) a
  where a.per_as_is >= 0.80
    and per_as_is <= 0.995
) b on dcr.date_id=b.date_ids
join (
  select
    dcr.dealer_legacy_id,
    count(*) n_days_data,
    sum(case when reasoning <> 'as is' then 1 else 0 end) sum_risky_days,
    sum(case when reasoning <> 'as is' then 1 else 0 end)/count(*) mean_30d_risk_score
  from insight.dealer_churn_reason dcr
  where date_id > date_sub(CURRENT_DATE, 30)
    and time_period = 30
  group by dcr.dealer_legacy_id
) dcr_stats
  on dcr.dealer_legacy_id = dcr_stats.dealer_legacy_id
;
