select
  filedate,

  100.000*sum(case when source_category = 'Paid' and lower(sort_field) like '%searchrank%' then 1 else 0 end)/count(*) p_paid_bm,
  100.000*sum(case when source_category = 'Paid' and lower(sort_field) not like '%searchrank%' then 1 else 0 end)/count(*) p_paid_others,
  100.000*sum(case when source_category <> 'Paid' and lower(sort_field) like '%searchrank%' then 1 else 0 end)/count(*) p_organic_bm,
  100.000*sum(case when source_category <> 'Paid' and lower(sort_field) not like '%searchrank%' then 1 else 0 end)/count(*) p_organic_others,

  avg(case when source_category = 'Paid' and lower(sort_field) like '%searchrank%' then dp.roi_value_30_day else null end) paid_bm_avg_roi

from insight.search_activity_raw sar
join insight.dealer_predictions dp
  on sar.filedate = dp.filedate
  and sar.customer_id = dp.customer_id
where sar.filedate = '2020-08-05'
  and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' )
group by 1
order by 1
;

select
  sum(case when roi_value_30_day is not null then 1 else 0 end) n_dealers,
  sum(case when roi_value_30_day >= 10 then 1 else 0 end) n_dealers_10,
  sum(case when roi_value_30_day <= 3 then 1 else 0 end) n_dealers_3,
  sum(case when roi_value_30_day is not null then da.unique_vin else null end) n_veh,
  sum(case when dp.roi_value_30_day >= 10 then da.unique_vin else null end) n_veh_10,
  sum(case when dp.roi_value_30_day <=3 then da.unique_vin else null end) n_veh_3,
  sum(case when roi_value_30_day is not null then da.inventory_used else null end) n_used_veh,
  sum(case when dp.roi_value_30_day >= 10 then da.inventory_used else null end) n_used_veh_10,
  sum(case when dp.roi_value_30_day <= 3 then da.inventory_used else null end) n_used_veh_3
from insight.dealer_predictions dp
join insight.dealer_activity da
  on dp.filedate = da.filedate
  and dp.customer_id = da.customer_id
where dp.filedate = '2020-08-08'
;
