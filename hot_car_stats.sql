# Hot Car Stats
select
  sar.filedate,
  sum(cast(sar.sublead as float))/count(*) avg_sublead,
  sum(case when hc.pred_hot = '1' then cast(sar.sublead as float) else 0 end)/sum(case when hc.pred_hot = '1' then 1 else 0 end) avg_sublead_hot,
  count(distinct case when sublead = '1' then user_id else null end)/count(distinct user_id) avg_conv,
  count(distinct case when hc.pred_hot = '1' and sublead = '1' then user_id else null end)/count(distinct case when hc.pred_hot = '1' then user_id else null end) avg_conv_hot
from insight.search_activity_raw sar
join insight.hot_car_badgingv2 hc
  on sar.classified_ad_id = hc.classified_ad_id
  and CAST(sar.filedate AS DATE) = DATEADD(day, 1, CAST(hc.filedate AS DATE))
where CAST(sar.filedate AS DATE) >= '2020-08-01'
group by 1
order by 1
;


select
  a.new_used_ind,
  count(*) n_vehicles,
  avg(days_infer_sale) avg_sale_period,
  median(days_infer_sale) med_sale_period
from (
         select hc.filedate,
                hc.classified_ad_id,
                inf_sale.days_infer_sale,
                hc.new_used_ind
         from insight.hot_car_badgingv2 hc
                  join dw_vw.infer_sale_vehicle_vw inf_sale
                       on hc.classified_ad_id = trim('O' FROM inf_sale.vehicle_id)
         where hc.filedate >= '2020-07-01'
           and hc.pred_hot = 1
           and hc.days_live = 0
           and inf_sale.date_id >= '2020-07-01'
) a
where a.filedate between '2020-07-01' and '2020-08-01'
group by 1
;

select
  new_used_ind,
  count(*) n_vehicles,
  avg(days_infer_sale) avg_sale_period,
  median(days_infer_sale) med_sale_period
from dw_vw.infer_sale_vehicle_vw a
join (select distinct vehicle_id, new_used_ind from insight.inventory_activity where filedate between '2020-07-01' and '2020-08-01') ia
on ia.vehicle_id = a.vehicle_id
where a.date_id between '2020-07-01' and '2020-08-01'
group by 1
