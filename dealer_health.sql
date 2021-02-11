select
  da.customer_name,
  da.dealer_legacy_id,
  da.sales_director_name,
  da.total_spend,
  dp.total_spend_30_day,
  dp.roi_value_30_day,
  dp.marginal_revenue_acquisition_30_day,
  da.total_leads_30day - da.other_leads_30day leads_30_day,
  da.legit_inventory_new legit_new,
  da.inventory_new,
  da.legit_inventory_used + da.legit_inventory_cpo legit_used,
  da.inventory_used + da.inventory_cpo,
  100.00*(da.legit_inventory_new + da.legit_inventory_used + da.legit_inventory_cpo)/(da.inventory_used + da.inventory_cpo + da.inventory_new)  per_legit,
  da.avg_best_match_score avg_bm_score,
  dsa.avg_raw_display_pos_cur,
  dsa.avg_raw_display_pos_week,
  dsa.avg_srp2vdp_cur,
  dsa.avg_srp2vdp_week
from insight_prod.dealer_activity da
left join insight_prod.dealer_predictions dp
  on da.customer_id = dp.customer_id
  and da.filedate = dp.filedate
left join insight_prod.dealer_site_activity_agg dsa
  on da.customer_id = dsa.customer_id
  and da.filedate = dsa.filedate
where da.dealer_legacy_id = '14812'
and da.filedate = '2020-12-10' ;
