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
join insight.dealer_churn dc
  on da.customer_id = dc.customer_id
join
(
	select max(filedate) as filedates from
  (
  select
    filedate,
    sum(case when churn_result_30 = 'as is' then 1 else 0 end) as_is_count,
    count(*) total_count,
    sum(case when churn_result_30 = 'as is' then 1 else 0 end)/count(*) per_as_is
  from insight.dealer_churn
  where filedate > date_sub(filedate, 30)
  group by filedate
) a where a.per_as_is >= 0.85 and per_as_is <= 0.995
) b
on dc.filedate=b.filedates
;
