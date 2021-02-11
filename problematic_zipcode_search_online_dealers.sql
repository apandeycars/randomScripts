select
  to_postal_cd,
  make_name,
  model_name,
  sum(n_vehicles) n_vehicles,
  sum(n_carvana_listings) n_carvana_listings,
  sum(n_vroom_listings) n_vroom_listings,
  100.00*sum(n_carvana_listings)/sum(n_vehicles) per_carvana_listings,
  100.00*sum(n_vroom_listings)/sum(n_vehicles) per_vroom_listings,
  100.00*(sum(n_vroom_listings) + sum(n_carvana_listings))/sum(n_vehicles) per_ol_listings
from (
  select
    ia.filedate,
    pcd.from_postal_cd,
    pcd.to_postal_cd,
    pcd.distance,
    ia.make_name,
    ia.model_name,
    count(*) n_vehicles,
    sum(case when lower(customer_name) like '%carvana%' then 1 else 0 end) n_carvana_listings,
    sum(case when lower(customer_name) like '%vroom%' then 1 else 0 end) n_vroom_listings
  from insight.inventory_activity ia
  left outer join master_data.postal_code_distance pcd
    on ia.zipcode = pcd.from_postal_cd
    and pcd.distance <= 30
  join (
    select * from (
      select
        ia.filedate,
        ia.zipcode,
        ia.make_name,
        ia.model_name,
        count(*) n_listings,
        sum(case when lower(customer_name) like '%carvana%' then 1 else 0 end) n_carvana_listings,
        sum(case when lower(customer_name) like '%vroom%' then 1 else 0 end) n_vroom_listings
      from insight.inventory_activity ia
      where filedate = '2021-01-18'
      group by 1, 2, 3, 4
    ) a where (a.n_carvana_listings + a.n_vroom_listings) >= 3
  ) base on base.zipcode = pcd.to_postal_cd
    and base.make_name = ia.make_name
    and base.model_name = ia.model_name
  where ia.filedate = '2021-01-18'
group by 1, 2, 3, 4, 5, 6
) a
group by 1, 2, 3
having per_ol_listings >= 30 ;
