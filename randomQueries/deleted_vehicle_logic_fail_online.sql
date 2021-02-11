select
  dvd.date_id,
  sum(case when dvd.infer_sale_ind = 'Yes' and (
    lower(da.major_account_name) like '%carvana%'
    or lower(da.major_account_name) like '%vroom%'
    or lower(da.major_account_name) like '%offlease%'
    ) then 1 else 0 end
  ) n_removed_cars_ol,
  sum(case when dvd.infer_sale_ind = 'Yes' and (
    lower(da.major_account_name) not like '%carvana%'
    and lower(da.major_account_name) not like '%vroom%'
    and lower(da.major_account_name) not like '%offlease%'
    ) then 1 else 0 end
  ) n_removed_cars_local,
  avg(case when dvd.infer_sale_ind = 'Yes' and (
    lower(da.major_account_name) like '%carvana%'
    or lower(da.major_account_name) like '%vroom%'
    or lower(da.major_account_name) like '%offlease%'
    ) then datediff(vehicle_remove_date, vehicle_add_date) else NULL end
  ) avg_vehicle_age_ol,
  avg(case when dvd.infer_sale_ind = 'Yes' and (
    lower(da.major_account_name) not like '%carvana%'
    and lower(da.major_account_name) not like '%vroom%'
    and lower(da.major_account_name) not like '%offlease%'
    ) then datediff(vehicle_remove_date, vehicle_add_date) else NULL end
  ) avg_vehicle_age_local
from fact_data.deleted_vehicle_daily dvd
join insight.dealer_activity da
  on dvd.customer_id = da.customer_id
  and dvd.date_id = da.date_id
where dvd.date_id = '2020-05-05'
group by dvd.date_id ;
