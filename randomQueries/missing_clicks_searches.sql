select distinct user_id,type, date_id, zip,activity_time,
case when type in ('search', 'impression') then web_page_type_cat else web_page_type_from_cat end as web_page_type_from_cat,
web_page_type_to_cat,
src_srp_page_number,
src_srp_display_position,
search_instance_id,
type_cat, type_name,
lead_user_id,
lead_time,
lead_id
from
(select user_id,type, date_id, zip,
case when type in ('click')
then hours_sub(from_timestamp(concat_ws('-', substr(activity_time,1,4), substr(activity_time,5,2), substr(activity_time,7) ), 'yyyy-MM-dd HH:mm:ss'), 6)
else
from_timestamp(concat_ws('-', substr(activity_time,1,4), substr(activity_time,5,2), substr(activity_time,7) ), 'yyyy-MM-dd HH:mm:ss')
end as activity_time,
web_page_type_cat,
web_page_type_name,
web_page_type_from_cat,
src_srp_page_number,
src_srp_display_position,
web_page_type_to_cat,
search_instance_id,
type_cat, type_name,
lead_user_id,
lead_time,
lead_id
from consumer_dev.consumer_activity cons
left join
(select distinct user_id as lead_user_id,
from_timestamp(concat_ws('-', substr(activity_time,1,4), substr(activity_time,5,2), substr(activity_time,7) ), 'yyyy-MM-dd HH:mm:ss') as lead_time,
id as lead_id
from consumer_dev.consumer_activity
where email is not null
) email_lead
on cons.user_id = email_lead.lead_user_id
where cons.user_id in ('1356307341524982348863090983587198', '1355911201296837889523292052384834', '2160616400000000000000001413229776421')
and cons.date_id <= date_add('2018-07-23', 1) and to_date(cons.date_id) >= '2018-07-01'
and type in ('search', 'lead', 'click', 'impression')
and lower(web_page_type_cat) not like '%similar%'
and logging_source = 'ALS')t
where (t.activity_time < t.lead_time)
order by lead_id, lead_user_id, activity_time;
