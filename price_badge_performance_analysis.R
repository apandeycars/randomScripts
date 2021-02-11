select
  sar.filedate,
  sum(case when pb.price_badge = 'FAIR' then cast(srp2vdp as float) else 0 end)/sum(case when pb.price_badge = 'FAIR' then 1 else 0 end) avg_fair_srp2vdp,
  sum(case when pb.price_badge = 'GOOD' then cast(srp2vdp as float) else 0 end)/sum(case when pb.price_badge = 'GOOD' then 1 else 0 end) avg_good_srp2vdp,
  sum(case when pb.price_badge = 'GREAT' then cast(srp2vdp as float) else 0 end)/sum(case when pb.price_badge = 'GREAT' then 1 else 0 end) avg_great_srp2vdp,
  sum(case when pb.price_badge is null then cast(srp2vdp as float) else 0 end)/sum(case when pb.price_badge is null then 1 else 0 end) avg_null_srp2vdp,

  sum(case when pb.price_badge = 'FAIR' then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'FAIR' then cast(srp2vdp as float) else 0 end) avg_fair_connect_per_vdp,
  sum(case when pb.price_badge = 'GOOD' then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GOOD' then cast(srp2vdp as float) else 0 end) avg_good_connect_per_vdp,
  sum(case when pb.price_badge = 'GREAT' then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GREAT' then cast(srp2vdp as float) else 0 end) avg_great_connect_per_vdp,
  sum(case when pb.price_badge is null then cast(sublead as float) else 0 end)/sum(case when pb.price_badge is null then cast(srp2vdp as float) else 0 end) avg_null_connect_per_vdp,

  sum(case when pb.price_badge = 'FAIR' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'FAIR' then cast(srp2vdp as float) else 0 end) avg_fair_lead_per_vdp,
  sum(case when pb.price_badge = 'GOOD' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GOOD' then cast(srp2vdp as float) else 0 end) avg_good_lead_per_vdp,
  sum(case when pb.price_badge = 'GREAT' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GREAT' then cast(srp2vdp as float) else 0 end) avg_great_lead_per_vdp,
  sum(case when pb.price_badge is null and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge is null then cast(srp2vdp as float) else 0 end) avg_null_lead_per_vdp,

  sum(case when pb.price_badge = 'FAIR' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'FAIR' then 1 else 0 end) avg_fair_lead_per_view,
  sum(case when pb.price_badge = 'GOOD' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GOOD' then 1 else 0 end) avg_good_lead_per_view,
  sum(case when pb.price_badge = 'GREAT' and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge = 'GREAT' then 1 else 0 end) avg_great_lead_per_view,
  sum(case when pb.price_badge is null and ( lower(sar.lead_type_category) like '%e-mail%' or lower(sar.lead_type_category) like '%toll%' ) then cast(sublead as float) else 0 end)/sum(case when pb.price_badge is null then 1 else 0 end) avg_null_lead_per_view


from insight.search_activity_raw sar
join insight.price_badge_v2 pb
  on sar.classified_ad_id = pb.classified_ad_id
  and sar.filedate = pb.filedate
where sar.src_channel_id = 'wired'
  and sar.filedate = '2020-08-01'
group by 1
;

select
  ia.filedate,

  count(*) n_listings,

  sum(case when pb.price_badge = 'FAIR' then 1 else 0 end) n_fair,
  sum(case when pb.price_badge = 'GOOD' then 1 else 0 end) n_good,
  sum(case when pb.price_badge = 'GREAT' then 1 else 0 end) n_great,
  sum(case when pb.price_badge is NULL then 1 else 0 end) n_null,

  sum(case when pb.price_badge = 'FAIR' then ia.current_lead_wired else 0 end) n_fair_leads,
  sum(case when pb.price_badge = 'GOOD' then ia.current_lead_wired else 0 end) n_good_leads,
  sum(case when pb.price_badge = 'GREAT' then ia.current_lead_wired else 0 end) n_great_leads,
  sum(case when pb.price_badge is NULL then ia.current_lead_wired else 0 end) n_null_leads,

  1.000*sum(case when pb.price_badge = 'FAIR' then ia.current_vdp_wired else 0 end)/sum(case when pb.price_badge = 'FAIR' then ia.srp_viewability else 0 end) avg_fair_srp2vdp,
  1.000*sum(case when pb.price_badge = 'GOOD' then ia.current_vdp_wired else 0 end)/sum(case when pb.price_badge = 'GOOD' then ia.srp_viewability else 0 end) avg_good_srp2vdp,
  1.000*sum(case when pb.price_badge = 'GREAT' then ia.current_vdp_wired else 0 end)/sum(case when pb.price_badge = 'GREAT' then ia.srp_viewability else 0 end) avg_great_srp2vdp,
  1.000*sum(case when pb.price_badge is null then ia.current_vdp_wired else 0 end)/sum(case when pb.price_badge is null then ia.srp_viewability else 0 end) avg_null_srp2vdp,

  1.000*sum(case when pb.price_badge = 'FAIR' then ia.current_lead_wired else 0 end)/sum(case when pb.price_badge = 'FAIR' then ia.current_vdp_wired else 0 end) avg_fair_vdp2lead,
  1.000*sum(case when pb.price_badge = 'GOOD' then ia.current_lead_wired else 0 end)/sum(case when pb.price_badge = 'GOOD' then ia.current_vdp_wired else 0 end) avg_good_vdp2lead,
  1.000*sum(case when pb.price_badge = 'GREAT' then ia.current_lead_wired else 0 end)/sum(case when pb.price_badge = 'GREAT' then ia.current_vdp_wired else 0 end) avg_great_vdp2lead,
  1.000*sum(case when pb.price_badge is null then ia.current_lead_wired else 0 end)/sum(case when pb.price_badge is null then ia.current_vdp_wired else 0 end) avg_null_vdp2lead

from insight.inventory_activity ia
join insight.price_badge_v2 pb
  on pb.classified_ad_id = ia.classified_ad_id
  and pb.filedate = ia.filedate
where ia.filedate = '2019-06-01'
  and ia.classified_ad_status_id = 0
  and ia.new_used_ind = 'Used'
  and ia.siy_ind = 'No'
group by 1
order by 1
;
