select
  a.customer_id,
  case
    when a.run_date = a.max_date_id and a.max_duc = 0 then 'active'
    when a.run_date = a.max_date_id and a.max_duc <> 0 then 'active_cancelled'
    else 'inactive'
  end as dealer_status,
  a.run_date,
  a.max_date_id last_record_date_id,
  a.avg_pred_30d avg_pred_30d,
  a.avg_raw_pr_30d avg_raw_pr_30d,
  a.max_duc,
  a.min_duc,
  a.n_predictions n_predictions
from (
select
  dcr.customer_id,
  substr(cast(date_sub(now(), 2) as string), 1, 10)  as run_date,
  max(dcr.date_id) as max_date_id,
  avg(case when dcr.reasoning = 'as is' then 0 else 1 end) as avg_pred_30d,
  avg(cast(dcr.label_pred_old as float)) avg_raw_pr_30d,
  count(*) n_predictions,
  max(da.days_until_cancellation) max_duc,
  min(da.days_until_cancellation) min_duc
from insight.dealer_churn_reason dcr
join insight.dealer_activity da
  on dcr.date_id = da.date_id
  and dcr.customer_id = da.customer_id
where dcr.date_id >= date_sub(now(), 32)
  and dcr.date_id <= date_sub(now(), 2)
  and dcr.time_period = 30
group by 1, 2
) a
;
