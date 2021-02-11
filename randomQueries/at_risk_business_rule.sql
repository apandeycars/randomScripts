select
  date_id,
  franchise_independent,
  n_dealers,
  (n_dealers - n_safe_dealers) n_risky_dealers,
  (n_dealers - n_safe_dealers)/n_dealers per_risky_dealers,
  avg_mrev_at_risk,
  avg_mrev
from (
select
  da.date_id,
  da.franchise_independent,
  count(distinct da.customer_id) n_dealers,
  count(distinct case when dcr.reasoning = 'as is' then da.customer_id else null end) n_safe_dealers,
  avg(cast(da.total_spend as float)) avg_spend,
  avg(cast(dcr.mrev_acq as float)) avg_mrev,
  avg(case when dcr.reasoning <> 'as is' then cast(dcr.mrev_acq as float) else null end) avg_mrev_at_risk,
  avg(case when dcr.reasoning = 'as is' then cast(dcr.mrev_acq as float) else null end) avg_mrev_safe
from insight.dealer_activity da
join insight.dealer_churn_reason dcr
  on da.customer_id = dcr.customer_id
  and date_add(da.date_id, 1) = dcr.date_id
where da.date_id >= '2020-04-01'
and dcr.time_period = 30
group by da.date_id, da.franchise_independent
) a
;

select
  da.customer_id,
  da.active_product_suite,
  da.total_spend,
  da.total_leads_30day,
  case
    when da.total_spend < 10 then 0
    when (da.total_leads_30day - da.other_leads_30day) = 0 then da.total_spend
    else da.total_spend / (da.total_leads_30day - da.other_leads_30day)
  end as cpl_rolling_30day,
  case
    when da.total_spend < 10 then 0
    when (da.vdp_new_30day + da.vdp_used_30day) = 0 then da.total_spend
    else da.total_spend / (da.vdp_new_30day + da.vdp_used_30day)
  end as cpv_rolling_30day,
  da.unique_vin,
  dcr.mrev_acq
from insight.dealer_activity da
join insight.dealer_churn_reason dcr
  on da.customer_id = dcr.customer_id
  and date_add(da.date_id, 1) = dcr.date_id
  and dcr.reasoning <> 'as is'
  and dcr.time_period = 30
where da.date_id = '2020-04-07'
  and da.franchise_independent = 'Independent'
order by total_spend;

select
  da.customer_id,
  da.active_product_suite,
  da.total_spend,
  da.total_leads_30day,
  case
    when da.total_spend < 10 then 0
    when (da.total_leads_30day - da.other_leads_30day) = 0 then da.total_spend
    else da.total_spend / (da.total_leads_30day - da.other_leads_30day)
  end as cpl_rolling_30day,
  case
    when da.total_spend < 10 then 0
    when (da.vdp_new_30day + da.vdp_used_30day) = 0 then da.total_spend
    else da.total_spend / (da.vdp_new_30day + da.vdp_used_30day)
  end as cpv_rolling_30day,
  da.unique_vin,
  dcr.mrev_acq
from insight.dealer_activity da
join insight.dealer_churn_reason dcr
  on da.customer_id = dcr.customer_id
  and date_add(da.date_id, 1) = dcr.date_id
  and dcr.time_period = 30
where da.date_id = '2020-04-07'
  and da.franchise_independent = 'Independent'
  and da.days_until_cancellation <> 0
order by total_spend;
