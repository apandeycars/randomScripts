select
  dp.filedate,
  dp.dealer_legacy_id,
  dp.total_spend_30_day,
  da.total_spend,
  da.total_spend - da.total_non_subscription_spend as total_spend_subs,
  da.active_product_suite,
  da.days_until_cancellation,
  da.active_product_cnt,
  dp.roi_value_30_day,
  dp.franchise_independent,
  da.unique_vin,
  da.legit_inventory_used + da.legit_inventory_new n_listings_legit,
  da.total_leads_30day - da.other_leads_30day as leads_30_days,
  da.total_leads - da.other_leads as leads,
  da.customer_name,
  da.billing_city,
  da.billing_postal_code,
  da.customer_id
from insight.dealer_predictions dp
join insight.dealer_activity da
  on dp.dealer_legacy_id = da.dealer_legacy_id
  and dp.filedate = da.filedate
where (dp.roi_value_30_day is null or dp.roi_value_30_day = 0 or dp.roi_value_30_day = '')
  and dp.filedate = '2020-10-05'
-- non cancelled dealers only
  and cast(da.active_product_cnt as float) > 0
-- are not online dealers
  and cast(dp.total_spend_30_day as float) > 0
  and (lower(da.active_product_suite) like '%uvs%' or lower(da.active_product_suite) like '%nvs%')
;
