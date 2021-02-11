select
  ia_today.filedate,
  count(*) n_listings,
  count(distinct ia_today.vin) n_vins,
  sum(case when ia_today.photo_count = 0 then 1 else 0 end) n_0_photo_vehicles,
  sum(case when ia_today.photo_count = 0 and ia_nextday.photo_count > 0 then 1 else 0 end) n_vehicles_fixed_next_day
from (
    select classified_ad_id, filedate, photo_count, vin
    from insight.inventory_activity ia_today
    where lower(ia_today.customer_name) like '%carvana%'
      and ia_today.filedate >= '2020-01-01'
) ia_today
join (
    select
      classified_ad_id,
      CAST(DATEADD(day, -1, CAST(ia_nextday.filedate AS DATE)) AS DATE) filedate,
      photo_count
    from insight.inventory_activity ia_nextday
    where lower(ia_nextday.customer_name) like '%carvana%'
      and ia_nextday.filedate >= '2020-01-01'
) ia_nextday
  on ia_today.classified_ad_id  = ia_nextday.classified_ad_id
  and ia_today.filedate = ia_nextday.filedate
group by 1 ;
