select
  da.date_id,
  da.dealer_legacy_id,
  case
    when da.total_spend = 0 then 0
    when dcr.incremental_lead_value > 60 then 60
    when ((da.legit_inventory_new + da.legit_inventory_used) < 5) and (dcr.incremental_lead_value is null) then 0
    when (da.total_spend > 100)
        and ((da.legit_inventory_new + da.legit_inventory_used) > 10)
        and (dcr.incremental_lead_value is null or dcr.incremental_lead_value = 0)
        and da.total_spend/(da.legit_inventory_new + da.legit_inventory_used) <= 60
        then da.total_spend/(da.legit_inventory_new + da.legit_inventory_used)
    when (da.total_spend > 100)
        and ((da.legit_inventory_new + da.legit_inventory_used) > 10)
        and (dcr.incremental_lead_value is null or dcr.incremental_lead_value = 0)
        and da.total_spend/(da.legit_inventory_new + da.legit_inventory_used) > 60
        then 60
    else dcr.incremental_lead_value
  end as m_revenue
from insight.dealer_activity da
left join (
  select
    customer_id,
    label_pred_old,
    reasoning,
    date_id,
    substr(cast(date_sub(dcr.date_id, 1) as string), 1, 11) as dcr_date_id,
    incremental_lead_value
  from insight.dealer_churn_reason dcr
  where dcr.date_id >= '2019-07-10'
    and dcr.time_period = 30
) dcr
  on da.customer_id = dcr.customer_id
  AND cast(dcr.dcr_date_id as string) = cast(da.date_id as string)
where da.date_id = '2019-07-10'
  and total_spend is not null
  and lower(da.customer_name) not like '%carvana%'
  and lower(da.customer_name) not like '%vroom%'
  and lower(da.customer_name) not like '%offleaseonly%'
  and lower(da.customer_name) not like '%carmax%'
  and lower(da.customer_name) not like '%hertz%'
  and lower(da.customer_name) not like '%tred private%'
  and lower(da.customer_name) not like '%tred (private%'
  and lower(da.major_account_name) not like '%vroom%'
  and lower(da.major_account_name) not like '%shift technologies%'
  and lower(da.major_account_name) not like '%blinker%'
  and lower(da.major_account_name) not like '%drivetime%'
  and lower(da.customer_name) not like '%avis%'
limit 10 
