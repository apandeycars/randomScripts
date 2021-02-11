select
  a.customer_name,
  min(run_date) run_date,
  min(dealer_legacy_id) dealer_legacy_id,
  min(billing_city) billing_city,
  min(franchise_name_list) franchise_name_list,
  min(joining_date) joining_date,
  min(days_since_customer) days_since_customer,
  min(n_one_year_connections) n_one_year_connections,
  min(connections_ytd) connections_ytd,
  min(connections_mtd) connections_mtd,
  max(merchandising_score) merchandising_score
from (
select
  d.max_date_id run_date,
  da.dealer_legacy_id,
  billing_city,
  franchise_name_list,
  lt.min_date joining_date,
  datediff(date_id, lt.min_date) days_since_customer,
  lt.n_lifetime_connections + (case when walkin_lt.n_lt_walkin_leads is null then 0 else walkin_lt.n_lt_walkin_leads end) n_one_year_connections,
  lt_year.n_ytd_connections + (case when walkin_ytd.n_ytd_walkin_leads is null then 0 else walkin_ytd.n_ytd_walkin_leads end) connections_ytd,
  total_leads_mtd + (case when walkin_mtd.n_mtd_walkin_leads is null then 0 else walkin_mtd.n_mtd_walkin_leads end) connections_mtd,
  customer_name,
  100*(da.legit_inventory_new + da.legit_inventory_used )/(da.legit_inventory_new + da.inventory_used) merchandising_score
from insight.dealer_activity da
join (
  select max(da.date_id) max_date_id from insight.dealer_activity da where date_id >= '2020-01-01'
) d
left join (
  select
    da.dealer_legacy_id,
    min(da.commencement_date) min_date,
    sum(da.total_leads) n_lifetime_connections
  from insight.dealer_activity da
  where customer_name in ('Brookdale Chrysler Jeep Dodge RAM', 'Luther Brookdale Chevrolet', 'Luther Brookdale Volkswagen',
  'Luther Hopkins Honda ', 'Luther Hudson Chevrolet GMC', 'Acura of Boston', 'Audi Natick', 'AutoFair Ford of Haverhill', 'AutoFair Subaru', 'Bernardi Honda of Natick', 'Bernardi Toyota', 'Bill Deluca Chevrolet Buick GMC', 'Bill Deluca Chrysler Jeep Dodge Ram', 'Cornerstone Mitsubishi', 'Framingham Ford', 'Honda North', 'Jaffarian Toyota', 'Empire Hyundai', 'First Hyundai', 'Grieco Ford', 'Elmwood Chrysler Dodge Jeep RAM', 'Empire Ford of New Bedford', 'Baystate Ford', 'BMW of Newport', 'Courtesy Mitsubishi', 'Bristol Toyota', 'Pride Hyundai of Seekonk', 'First Chrysler Dodge Jeep Ram', 'Grieco Honda', 'Brookdale Chrysler', 'Jaguar Land Rover Minneapolis', 'Luther Alfa Romeo of Minneapolis', 'Luther Bloomington Hyundai', 'Luther Bloomington Subaru', 'Luther Brookdale Chev', 'Luther Brookdale Honda', 'Luther Brookdale Mazda', 'Luther Brookdale Toyota', 'Luther Brookdale VW', 'Luther Cadillac', 'Luther Hopkins Honda', 'Luther Hudson Chev', 'Fitzgerald Toyota Chambersburg', 'Fitzgerald Toyota Gaithersburg', 'Fitzgerald Subaru', 'Fitzgerald Auto Mall Wheaton', 'Fitzgerald Buick GMC Rockville', 'Fitzgerald Chevrolet Frederick', 'Fitzgerald Auto Mall Germantown Used Cars', 'Fitzgerald Chrysler Dodge Jeep RAM Hagerstown', 'Fitzgerald Chevrolet Cadillac Hagerstown', 'Fitzgerald Mazda Frederick', 'Fitzgerald Volkswagen Frederick', 'Fitzgerald Nissan Chambersburg', 'Euro Motorcars Inc')
    and date_id >= '2019-01-01'
  group by da.dealer_legacy_id
) lt
  on da.dealer_legacy_id = lt.dealer_legacy_id
left join (
  select
    da.dealer_legacy_id,
    sum(da.total_leads) n_ytd_connections
  from insight.dealer_activity da
  where customer_name in ('Brookdale Chrysler Jeep Dodge RAM', 'Luther Brookdale Chevrolet', 'Luther Brookdale Volkswagen',
  'Luther Hopkins Honda ', 'Luther Hudson Chevrolet GMC', 'Acura of Boston', 'Audi Natick', 'AutoFair Ford of Haverhill', 'AutoFair Subaru', 'Bernardi Honda of Natick', 'Bernardi Toyota', 'Bill Deluca Chevrolet Buick GMC', 'Bill Deluca Chrysler Jeep Dodge Ram', 'Cornerstone Mitsubishi', 'Framingham Ford', 'Honda North', 'Jaffarian Toyota', 'Empire Hyundai', 'First Hyundai', 'Grieco Ford', 'Elmwood Chrysler Dodge Jeep RAM', 'Empire Ford of New Bedford', 'Baystate Ford', 'BMW of Newport', 'Courtesy Mitsubishi', 'Bristol Toyota', 'Pride Hyundai of Seekonk', 'First Chrysler Dodge Jeep Ram', 'Grieco Honda', 'Brookdale Chrysler', 'Jaguar Land Rover Minneapolis', 'Luther Alfa Romeo of Minneapolis', 'Luther Bloomington Hyundai', 'Luther Bloomington Subaru', 'Luther Brookdale Chev', 'Luther Brookdale Honda', 'Luther Brookdale Mazda', 'Luther Brookdale Toyota', 'Luther Brookdale VW', 'Luther Cadillac', 'Luther Hopkins Honda', 'Luther Hudson Chev', 'Fitzgerald Toyota Chambersburg', 'Fitzgerald Toyota Gaithersburg', 'Fitzgerald Subaru', 'Fitzgerald Auto Mall Wheaton', 'Fitzgerald Buick GMC Rockville', 'Fitzgerald Chevrolet Frederick', 'Fitzgerald Auto Mall Germantown Used Cars', 'Fitzgerald Chrysler Dodge Jeep RAM Hagerstown', 'Fitzgerald Chevrolet Cadillac Hagerstown', 'Fitzgerald Mazda Frederick', 'Fitzgerald Volkswagen Frederick', 'Fitzgerald Nissan Chambersburg', 'Euro Motorcars Inc')
    and date_id >= '2020-01-01'
  group by da.dealer_legacy_id
) lt_year
  on da.dealer_legacy_id = lt_year.dealer_legacy_id
left join (
  select
    wl.customer_id,
    count(*) n_ytd_walkin_leads
  from fact_data.lead_walkin wl
  where date_id >= '2020-01-01'
  group by customer_id
) walkin_ytd
  on walkin_ytd.customer_id = da.customer_id
left join (
  select
    wl.customer_id,
    count(*) n_mtd_walkin_leads
  from fact_data.lead_walkin wl
  join (select concat(substr(max(date_id), 1, 7), '-01') mtd_date_id from fact_data.lead_walkin where date_id >= '2020-01-01') d
  where date_id >= mtd_date_id
  group by customer_id
) walkin_mtd
  on walkin_mtd.customer_id = da.customer_id
left join (
  select
    wl.customer_id,
    count(*) n_lt_walkin_leads
  from fact_data.lead_walkin wl
  where date_id >= '2019-01-01'
  group by customer_id
) walkin_lt
  on walkin_lt.customer_id = da.customer_id
where customer_name in ('Brookdale Chrysler Jeep Dodge RAM', 'Luther Brookdale Chevrolet', 'Luther Brookdale Volkswagen',
  'Luther Hopkins Honda ', 'Luther Hudson Chevrolet GMC', 'Acura of Boston', 'Audi Natick', 'AutoFair Ford of Haverhill', 'AutoFair Subaru', 'Bernardi Honda of Natick', 'Bernardi Toyota', 'Bill Deluca Chevrolet Buick GMC', 'Bill Deluca Chrysler Jeep Dodge Ram', 'Cornerstone Mitsubishi', 'Framingham Ford', 'Honda North', 'Jaffarian Toyota', 'Empire Hyundai', 'First Hyundai', 'Grieco Ford', 'Elmwood Chrysler Dodge Jeep RAM', 'Empire Ford of New Bedford', 'Baystate Ford', 'BMW of Newport', 'Courtesy Mitsubishi', 'Bristol Toyota', 'Pride Hyundai of Seekonk', 'First Chrysler Dodge Jeep Ram', 'Grieco Honda', 'Brookdale Chrysler', 'Jaguar Land Rover Minneapolis', 'Luther Alfa Romeo of Minneapolis', 'Luther Bloomington Hyundai', 'Luther Bloomington Subaru', 'Luther Brookdale Chev', 'Luther Brookdale Honda', 'Luther Brookdale Mazda', 'Luther Brookdale Toyota', 'Luther Brookdale VW', 'Luther Cadillac', 'Luther Hopkins Honda', 'Luther Hudson Chev', 'Fitzgerald Toyota Chambersburg', 'Fitzgerald Toyota Gaithersburg', 'Fitzgerald Subaru', 'Fitzgerald Auto Mall Wheaton', 'Fitzgerald Buick GMC Rockville', 'Fitzgerald Chevrolet Frederick', 'Fitzgerald Auto Mall Germantown Used Cars', 'Fitzgerald Chrysler Dodge Jeep RAM Hagerstown', 'Fitzgerald Chevrolet Cadillac Hagerstown', 'Fitzgerald Mazda Frederick', 'Fitzgerald Volkswagen Frederick', 'Fitzgerald Nissan Chambersburg', 'Euro Motorcars Inc')
  and date_id = '2020-01-30'
) a
group by a.customer_name ;
