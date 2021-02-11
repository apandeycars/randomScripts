select
  ia.filedate,
  dp.avg_lead_quality_cur,
  dp.per_good_leads,
  count(*) n_vehicles,
  sum(case when new_used_ind = 'Used' then 1 else 0 end) n_used_vehicles,
  sum(case when new_used_ind = 'New' then 1 else 0 end) n_new_vehicles,
  1.000*sum(case when new_used_ind = 'Used' and "30_day_lead_rolling_sum" > 0 then 1 else 0 end)/sum(case when new_used_ind = 'Used' then 1 else 0 end) per_used_vehicles_with_lead_30_days,
  1.000*sum(case when new_used_ind = 'New' and "30_day_lead_rolling_sum" > 0 then 1 else 0 end)/sum(case when new_used_ind = 'New' then 1 else 0 end) per_new_vehicles_with_lead_30_days,
  1.000*sum(case when new_used_ind = 'Used' and "7_day_lead_rolling_sum" > 0 then 1 else 0 end)/sum(case when new_used_ind = 'Used' then 1 else 0 end) per_used_vehicles_with_lead_7_days,
  1.000*sum(case when new_used_ind = 'New' and "7_day_lead_rolling_sum" > 0 then 1 else 0 end)/sum(case when new_used_ind = 'New' then 1 else 0 end) per_new_vehicles_with_lead_7_days,
  1.000*sum(case when ia."7_day_lead_rolling_sum" > 0 then ia."7_day_lead_rolling_sum" else null end)/sum(case when ia."7_day_lead_rolling_sum" > 0 then 1 else 0 end) avg_lead_per_listing,
  1.000*sum(case when ia."7_day_lead_rolling_sum" > 0 and ia.new_used_ind = 'Used' then ia."7_day_lead_rolling_sum" else null end)/sum(case when ia."7_day_lead_rolling_sum" > 0 and ia.new_used_ind = 'Used' then 1 else 0 end) avg_lead_per_used_listing,
  1.000*sum(case when ia."7_day_lead_rolling_sum" > 0 and ia.new_used_ind = 'New' then ia."7_day_lead_rolling_sum" else null end)/sum(case when ia."7_day_lead_rolling_sum" > 0 and ia.new_used_ind = 'New' then 1 else null end) avg_lead_per_new_listing,
  100.000*sum(case when ia.price is null or cast(ia.price as float) < 200 or ia.photo_count < 2 or (ia.mileage < 100 and new_used_ind = 'Used') then 1 else 0 end)/count(*) per_poor_merchandized,
  100.000*sum(case when new_used_ind = 'Used' and (ia.price is null or cast(ia.price as float) < 200 or ia.photo_count < 2 or ia.mileage < 100) then 1 else 0 end)/sum(case when new_used_ind = 'Used' then 1 else 0 end) per_poor_merchandized_used,
  100.000*sum(case when new_used_ind = 'New' and (ia.price is null or cast(ia.price as float) < 200 or ia.photo_count < 2) then 1 else 0 end)/sum(case when new_used_ind = 'New' then 1 else 0 end) per_poor_merchandized_new
from insight.inventory_activity ia
join insight.dealer_activity da
  on ia.customer_id = da.customer_id
  and ia.filedate = da.filedate
  and lower(da.major_account_name) like '%autonation%'
join (
    select
           dp.filedate,
           avg(dp.email_lead_quality_cur)  avg_lead_quality_cur,
           avg(100.000*(dp.n_leads_very_good_cur + dp.n_leads_good_cur) /
               (dp.n_leads_bad_cur + dp.n_leads_fair_cur + dp.n_leads_good_cur + dp.n_leads_very_bad_cur +
                dp.n_leads_very_good_cur)) per_good_leads
    from insight.dealer_predictions dp
    join insight.dealer_activity da
      on dp.customer_id = da.customer_id
      and dp.filedate = da.filedate
      and lower(da.major_account_name) like '%autonation%'
    group by 1
) dp
  on ia.filedate = dp.filedate
where ia.filedate >= '2020-01-01'
  and classified_ad_status_id = '0'
group by 1, 2, 3 ;
