select
  d.max_date_id run_date,
  da.dealer_legacy_id,
  billing_city,
  franchise_name_list,
  lt.min_date joining_date,
  datediff(date_id, lt.min_date) days_since_customer,
  lt.n_lifetime_connections + walkin_lt.n_lt_walkin_leads n_one_year_connections,
  lt_year.n_ytd_connections + walkin_ytd.n_ytd_walkin_leads connections_ytd,
  total_leads_mtd + walkin_mtd.n_mtd_walkin_leads connections_mtd,
  customer_name,
  100*(da.legit_inventory_new + da.legit_inventory_used )/(da.legit_inventory_new + da.inventory_used) merchandising_score
from insight_dr.dealer_activity da
join (
  select max(da.date_id) max_date_id from insight_dr.dealer_activity da where date_id >= '2020-01-01'
) d
left join (
  select
    da.dealer_legacy_id,
    min(da.commencement_date) min_date,
    sum(da.total_leads) n_lifetime_connections
  from insight_dr.dealer_activity da
  where customer_name in ('Bill Deluca Chevrolet Buick GMC', 'Mastria Mazda')
    and date_id >= '2019-01-01'
  group by da.dealer_legacy_id
) lt
  on da.dealer_legacy_id = lt.dealer_legacy_id
left join (
  select
    da.dealer_legacy_id,
    sum(da.total_leads) n_ytd_connections
  from insight_dr.dealer_activity da
  where customer_name in ('Bill Deluca Chevrolet Buick GMC', 'Mastria Mazda')
    and date_id >= '2020-01-01'
  group by da.dealer_legacy_id
) lt_year
  on da.dealer_legacy_id = lt_year.dealer_legacy_id
left join (
  select
    wl.customer_id,
    count(*) n_ytd_walkin_leads
  from fact_data_dr.lead_walkin wl
  where date_id >= '2020-01-01'
  group by customer_id
) walkin_ytd
  on walkin_ytd.customer_id = da.customer_id
left join (
  select
    wl.customer_id,
    count(*) n_mtd_walkin_leads
  from fact_data_dr.lead_walkin wl
  join (select concat(substr(max(date_id), 1, 7), '-01') mtd_date_id from fact_data_dr.lead_walkin where date_id >= '2020-01-01') d
  where date_id >= mtd_date_id
  group by customer_id
) walkin_mtd
  on walkin_mtd.customer_id = da.customer_id
left join (
  select
    wl.customer_id,
    count(*) n_lt_walkin_leads
  from fact_data_dr.lead_walkin wl
  where date_id >= '2019-01-01'
  group by customer_id
) walkin_lt
  on walkin_lt.customer_id = da.customer_id
where customer_name in ('Bill Deluca Chevrolet Buick GMC', 'Mastria Mazda')
  and date_id = '2020-01-15'
;
