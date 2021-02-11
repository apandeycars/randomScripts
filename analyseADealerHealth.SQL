select
  da.filedate,
  da.customer_name,
  dp.total_spend_30_day,
  dp.roi_value_30_day,
  da.total_leads_30day - da.other_leads_30day rolling_website_30_day,
  da.phone_new_30day + da.phone_used_30day n_phone_30_day,
  da.email_new_30day email_new_30_day,
  da.email_used_30day email_used_30_day,
  da.email_new,
  da.email_used,
  da.phone_new,
  da.phone_used,
  da.inventory_new,
  da.inventory_used,
  da.inventory_cpo,
  da.avg_inventory_age,
  da.inv_age_10_days,
  da.inv_age_11_25_days,
  da.inv_age_26_40_days,
  da.inv_age_40_60_days,
  da.inv_age_over_60_days,
  dsa.n_paid_srp_view_week,
  dsa.n_organic_srp_view_week,
  dsa.n_paid_leads_week,
  dsa.n_organic_leads_week,
  100.00*(da.legit_inventory_cpo + da.legit_inventory_used + da.legit_inventory_new) / (da.inventory_cpo + da.inventory_new + da.inventory_used) per_legit,
  100.00*(dp.n_leads_very_good_7_day + dp.n_leads_good_7_day)/(dp.n_leads_bad_7_day + dp.n_leads_fair_7_day + dp.n_leads_very_bad_7_day + dp.n_leads_very_good_7_day + dp.n_leads_good_7_day) per_good_7_day,
  da.cost_per_lead,
  da.avg_best_match_score avg_bm_score,
  dsa.avg_raw_display_pos_cur,
  dsa.avg_raw_display_pos_week,
  dsa.n_top_10_srp_views_cur,
  dsa.n_top_10_srp_views_week
from insight.dealer_activity da
join insight.dealer_predictions dp
  on da.customer_id = dp.customer_id
  and da.filedate =  dp.filedate
join insight.dealer_site_activity_agg dsa
  on dsa.customer_id = dp.customer_id
  and dsa.filedate =  dp.filedate
where dp.dealer_legacy_id in ('147')
  and dp.filedate >= '2020-07-01'
order by da.filedate ;
