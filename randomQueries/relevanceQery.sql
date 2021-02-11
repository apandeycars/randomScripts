  SELECT
    srch.filedate as date_id,
    coalesce(trim(srch.src_search_id), trim(srch.src_sid)) as src_search_id,
    coalesce(srch.src_user_id,srch.src_public_device_id) as user_id,
    srch.src_public_device_id as public_device_id,
    concat(translate(srch.date_id,'-',''),' ',t.hh_mm_ss_24_hr_tm) as search_time,
    imp.src_classified_ad_id classified_ad_id,
    imp.dealer_customer_id customer_id,
    imp.src_srp_page_number,
    imp.src_srp_display_position,
    imp.raw_srp_display_position,
    imp.src_channel_id,
    imp.make_name,
    imp.model_name,
    imp.bodystyle_name,
    imp.price vehicle_price,
    imp.mileage vehicle_mileage,
    imp.customer_zip_code customer_zip_code,
    case
      when lower(srch.search_type_name) like '%used%' then 'Used'
      when lower(srch.search_type_name) like '%new%' then 'New'
      when lower(srch.search_type_name) like '%blended%' then 'Blended'
      else 'Unknown'
    end as search_type,
    cast(srch.src_rsltcnt as float) as src_rsltcnt,
    srch.cpo_search cpo_search,
    srch.sort_field,
    srch.sort_order,
    srch.src_devicetype src_devicetype,
    srch.price_range_max,
    srch.price_range_min,
    substr(srch.zip_code_id, 1, 5) as search_zipcode,
    zc.msa_name search_msa_name,
    c.mobile_ind mobile_ind,
    fdc.source_category,
    fdc.source_type,
    srch.search_type_cat,
    pcd.distance distance_user_inventory,
    case
      when srch.src_page_url like 'http://www.cars.com/for-sale/listings/?%'
        then replace(concat('https://www.cars.com/for-sale/searchresults.action/?', substr(srch.src_page_url, 40, char_length(srch.src_page_url) - 39)),'amp;','')
      else replace(srch.src_page_url,'amp;','')
    end as src_page_fe_url,
    srch.src_radius src_radius,
    srch.authenticated_consumer authenticated_consumer,
    srch.mileage_range_max mileage_range_max,
    srch.mileage_range_min mileage_range_min,
    srch.vehicle_dtl_nm_list vehicle_dtl_nm_list,
    srch.front_door_apn front_door_apn,
    srch.src_keyword src_keyword,
    srch.dma_name dma_name,
    srch.dma_code dma_code,
    srch.front_door_apn front_door_apn,
    lower(split_part(split_part(srch.src_page_url, 'kw=', 2), '&', 1)) raw_text_keyword,
    imp.vdp_imp_time vdp_imp_time,
    imp.srp2vdp srp2vdp,
    imp.leadsub_time leadsub_time,
    imp.subLead subLead,
    imp.lead_type_category lead_type_category,
    imp.lead_type_name lead_type_name
  from activity_enrich.als_search_enrich srch
  inner join master_data.time_dim t
       on srch.time_id = t.time_id
  inner join (select * from master_data.front_door_category where current_ind = 'Yes') fdc
    on srch.front_door_affiliate_id = fdc.affiliate_id
  inner join master_data.channel c
    on srch.channel_id = c.channel_id
  inner join (
    select
      imp.filedate filedate,
      coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) src_search_id,
      imp.src_channel_id src_channel_id,
      concat(translate(imp.date_id,'-',''),' ',t.hh_mm_ss_24_hr_tm) as vdp_imp_time,
      cast(imp.src_srp_page_number as int) as src_srp_page_number,
      cast(imp.src_srp_display_position as int) src_srp_display_position,
      cast(src_srp_display_position as int) + (cast(src_srp_page_number as int)-1)*20 as raw_srp_display_position,
      imp.src_classified_ad_id src_classified_ad_id,
      imp.make_name make_name,
      imp.model_name as model_name,
      imp.price as price,
      imp.bodystyle_name as bodystyle_name,
      imp.mileage as mileage,
      imp.dealer_customer_id as dealer_customer_id,
      substr(imp.customer_zip_code_id, 1, 5) as customer_zip_code,
      CASE WHEN click.src_classified_ad_id IS NOT NULL THEN 1 ELSE 0 END AS srp2vdp,
      CASE WHEN dwl.vehicle_id IS NOT NULL THEN 1 ELSE 0 END AS subLead,
      concat(translate(dwl.date_id,'-',''),' ',td.hh_mm_ss_24_hr_tm) as leadsub_time,
      coalesce(lt.lead_type_category, '') lead_type_category,
      coalesce(lt.lead_type_name, '') lead_type_name
    from activity_enrich.als_impression_enrich imp
    inner join master_data.time_dim t
       on imp.time_id = t.time_id
    inner join (
      select
        filedate,
        coalesce(trim(search_instance_id), trim(src_sid)) src_search_id,
        src_classified_ad_id
      from activity_enrich.als_impression_enrich
      where web_page_type_name = 'SRP Listing Viewability'
        and filedate = '2019-04-10'
      group by 1, 2, 3
    ) srv
      on imp.filedate = srv.filedate and coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) = srv.src_search_id and imp.src_classified_ad_id = srv.src_classified_ad_id
    left join activity_enrich.als_clickthru_enrich click
      on coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) = coalesce(trim(click.search_instance_id), trim(click.src_sid))
      and imp.filedate = click.filedate
      and imp.src_classified_ad_id = click.src_classified_ad_id
    left join dw.lead dwl
      on coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) = dwl.search_instance_id
      and imp.filedate = dwl.filedate
      and imp.src_classified_ad_id = substr(dwl.vehicle_id, 1, 9)
    left join master_data.lead_type lt
      on cast(lt.lead_type_id as string) = dwl.lead_type_id
    left join master_data.time_dim td
       on dwl.time_id = td.time_id
    where imp.filedate = '2019-04-10'
      and imp.web_page_type_category = 'Product Search Results Page'
      and imp.bot_flag = 'No'
      and imp.src_isbot = 'false'
      and coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) <> ''
      and coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) is not null
      and coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) <> '0'
      and imp.src_public_device_id <> ''
      and imp.src_classified_ad_id <> ''
      and imp.src_srp_display_position is not null
      and imp.src_srp_display_position <> ''
      and imp.recordstatus = 'Accepted'
      and coalesce(trim(imp.search_instance_id), trim(imp.src_sid)) = '5823934589636728477404132381861798955'
    group by 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
  ) imp on coalesce(trim(srch.src_search_id), trim(srch.src_sid)) = imp.src_search_id
    and srch.filedate = imp.filedate
  left join (select zip_code_id, msa_name from work.zip_code group by 1, 2) zc
    on srch.zip_code_id = zc.zip_code_id
  left join master_data.postal_code_distance pcd
    on substr(srch.zip_code_id, 1, 5) = pcd.from_postal_cd and imp.customer_zip_code = pcd.to_postal_cd
  where srch.filedate = '2019-04-10'
    and imp.filedate = '2019-04-10'
    and srch.search_type_cat not in (
          'For Sale by Owner'
          , 'Dealer Search'
          , 'Premier Used Search','Premier New Search', 'Premier CPO Search'
          , 'New Similar Vehicles', 'Used Similar Vehicles')
    and coalesce(trim(srch.src_search_id), trim(srch.src_sid)) <> ''
    and coalesce(trim(srch.src_search_id), trim(srch.src_sid)) is not null
    and coalesce(trim(srch.src_search_id), trim(srch.src_sid)) <> '0'
    and substr(srch.zip_code_id, 1, 5) <> '0'
    and coalesce(trim(srch.src_search_id), trim(srch.src_sid)) = '5823934589636728477404132381861798955'
