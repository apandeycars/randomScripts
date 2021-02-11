-- My search activities.

select filedate, count(*) from als_search_enrich ase
where src_user_id = '1355990432631149449652814648859417'
and filedate > '2018-08-01'
  and ase.search_type_cat not like '%Similar Vehicles%'
  and ase.search_type_cat <> 'Car Chemistry'
 group by 1


-- My recommendations.

select
  filedate,
  count(*) cnt
from recommended_email
where filedate > '2018-08-01'
and user_access_id = 'apandey@cars.com'
group by 1 order by 1

select * from recommended_email
where filedate > '2018-08-24'
and user_access_id = 'apandey@cars.com';
