-- Create a table with past 30 days average predictions for every dealer.

-- Following query generates last 60 days cancelled dealers.

select
  a.*,
  b.current_pred,
  b.pred_30_days,
  b.max_date_id_pred,
  b.min_date_id_pred,
  b.n_rows ndays_predictions,
  substring(a.date_id, 1, 7) month_id
from (
  select
    a.customer_id,
    max(a.date_id) date_id
  from (
    select
      da.customer_id,
      da.date_id,
      max(da.date_id) max_date_id
    from insight.dealer_activity da
    join insight.dealer_churn_reason dcr
      on da.customer_id = dcr.customer_id
      and da.date_id = dcr.date_id
    where da.days_until_cancellation = 1
      and da.date_id >= '2019-12-01'
      and dcr.time_period = 30
    group by da.customer_id, da.date_id
  ) a where a.date_id = a.max_date_id
  group by a.customer_id
) a
join (
  select
    dcr.customer_id,
    dcr.date_id,
    case when dcr.reasoning = 'as is' then 0 else 1 end as current_pred,
    avg(dcro.pred) pred_30_days,
    max(dcro.date_id) max_date_id_pred,
    min(dcro.date_id) min_date_id_pred,
    count(*) n_rows
  from insight.dealer_churn_reason dcr
  left join (
    select
      dcr.customer_id,
      case when dcr.reasoning = 'as is' then 0 else 1 end as pred,
      dcr.date_id
    from insight.dealer_churn_reason dcr
    where dcr.date_id >= '2019-11-01'
      and dcr.time_period = 30
  ) dcro on dcr.customer_id = dcro.customer_id
  where dcr.time_period = 30 and dcr.date_id >= '2019-12-01'
    and dcro.date_id between date_sub(dcr.date_id, 31) and dcr.date_id
  group by dcr.customer_id,
    dcr.date_id,
    case when dcr.reasoning = 'as is' then 0 else 1 end
) b on a.customer_id = b.customer_id
  and a.date_id = b.date_id
where substring(a.date_id, 1, 7) in ('2019-12', '2020-01') ;


##### R Script
library(data.table)
inputData <- setDT(read.csv("dealer-churn.csv"))
inputData[, .(.N, n_catches = sum(ifelse(pred_30_days >= 0.3, 1, 0))), by = list(month_id)]



select
  dcr.customer_id,
  date_sub(now(), 2) as run_date,

  avg(case when dcr.reasoning = 'as is' then 0 else 1 end) as pred,
  count(*) n_predictions
from insight_dr.dealer_churn_reason dcr
where date_id >= date_sub(now(), 32)
  and date_id <= date_sub(now(), 2)
  and dcr.time_period = 30
group by dcr.customer_id


select
  dcr.customer_id,
  date_sub(now(), 2) as run_date,
  max(dcr.date_id) as max_date_id,
  avg(case when dcr.reasoning = 'as is' then 0 else 1 end) as pred,
  count(*) n_predictions,
  max(da.days_until_cancellation) max_days_until_cancellation
from insight_dr.dealer_churn_reason dcr
join insight_dr.dealer_activity da
  on dcr.date_id = da.date_id
  and dcr.customer_id = da.customer_id
where dcr.date_id >= date_sub(now(), 32)
  and dcr.date_id <= date_sub(now(), 2)
  and dcr.time_period = 30
group by 1, 2


select
  dcr.customer_id,
  case when c.customer_id is null then 0 else 1 end as cancelled,
  max(case when dcr.reasoning = 'as is' then 0 else 1 end) pred_cancel_30d_max,
  max(case when c.customer_id is not null and dcr.reasoning = 'as is' then 0 else 1 end) pred_cancel_30d
from insight_dr.dealer_churn_reason dcr
left join (
select
  customer_id
from (
select
  a.*,
  b.customer_id b_customer_id,
  case when b.customer_id is null then 1 else 0 end as cancelled_dealer
from (
  select
    date_id,
    customer_id,
    total_spend,
    commencement_date,
    lead_feedback_cnt,
    franchise_independent,
    days_until_cancellation
  from insight_dr.dealer_activity
  where date_id >= '2019-08-01'
  and date_id <= date_sub(now(), 8)
) a
left join (
  select
    date_id,
    customer_id
  from insight_dr.dealer_activity
  group by date_id, customer_id
) b on a.customer_id = b.customer_id
  and b.date_id > a.date_id
  and b.date_id <= date_add(a.date_id, 7)
) a
where cancelled_dealer = 1
) c
on dcr.customer_id = c.customer_id
where dcr.time_period = 30
and dcr.date_id > '2019-07-01'
and dcr.date_id <= date_sub(now(), 8)
group by 1, 2
;
