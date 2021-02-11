select
  ele.filedate date_id,
  ele.src_lead_id lead_id,
  ele.src_classified_ad_id,
  ele.src_search_id,
  case
    when ele.src_user_id is null or ele.src_user_id = '' then 0
    else 1
  end as has_user_id,
  case
    when ele.message like '%Within a day or two%' then 'within a day or two'
    when ele.message like '%Within a month%' then 'within a month'
    when ele.message like '%Within a few months%' then 'within a few months'
    when ele.message like '%Within a week%' then 'within a week'
    when ele.message like '%Unsure%' then 'unsure'
    else 'unknown'
  end as purchase_timeframe,
  da.customer_id,
  da.shipping_city,
  da.customer_name,
  da.major_account_name,
  substr(ele.search_zip_code_id, 1, 5) as search_zipcode,
  substr(ele.consumer_zip_code_id, 1, 5) as consumer_zip_code,
  substr(ele.customer_zip_code_id, 1, 5) as customer_zip_code,
  ele.src_device_type,
  ele.src_vehicle_bodystyle_dscrp,
  ele.customer_state,
  da.franchise_independent,
  fdc.source_category,
  fdc.source_type,
  da.total_spend,
  da.total_listing_spend_used total_listing_spend_used,
  da.total_listing_spend_new total_listing_spend_new,
  ele.make_name,
  ele.price,
  case when ele.stock_type_id = 1 then 'New' else 'Used' end as stock_type,
  ce.preds ce_preds,
  ce.intent_score intent_score,
  ce.`class` ce_class,
  pcd.distance distance_user_inventory
from activity_enrich_dr.email_lead_enrich ele
left join (select * from master_data_dr.front_door_category where current_ind = 'Yes') fdc
  on ele.front_door_affiliate_id = fdc.affiliate_id
left join insight_dr.dealer_activity da
  on ele.customer_id = da.customer_id
  and ele.filedate = da.date_id
join insight_dr.consumer_intent ce
  on ele.src_lead_id = cast(ce.lead_id as string)
  and ele.filedate = ce.date_id
left join master_data_dr.postal_code_distance pcd
    on substr(ele.consumer_zip_code_id, 1, 5) = pcd.from_postal_cd and substr(ele.customer_zip_code_id, 1, 5) = pcd.to_postal_cd
where ce.date_id >= '2019-05-01'
