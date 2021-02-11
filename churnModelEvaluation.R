# select
#   dcr.filedate,
#   dcr.dealer_legacy_id,
#   case when dcr.reasoning = 'as is' then 0 else 1 end as churn_result,
#   dcr.incremental_lead_value,
#   dcr.label_pred_new churn_raw
# from insight.dealer_churn_reason dcr
# join insight.dealer_activity da
#   on dcr.dealer_legacy_id = da.dealer_legacy_id
#   and da.filedate = '2020-07-01'
# where dcr.filedate = '2020-07-01'
#   and dcr.time_period = 30
#   and dcr.dealer_legacy_id in ('158407','5393788','197221','4874','5387723','147444','5392442','203608','5357431','116','5388076','5392362','151623','21904','104265','5372427','185535','150651','184866','5378962','208611','5392755','185313','185725','5370647','10036','9450','107092','209665','110780','9746','5383852','181858','5383853','5363836','2749','5392997','193584','107882','4183','5366613','5391398','206165','5389237','147970','180959','159635','5343143','5367298','157291','5372258','5391680','5371441','207485','5391079','5385654','202764','5392268','5327789','5382247','200993','5342858','5393344','158030','202060','153349','124','5392240','157138','5368262','12716','3997','198851','5387144','5394037','195978','198347','5388565','5375501','5385767','165536','5370077','189379','5244220','181807','211606','158202','5355883','161121','204707','2401026','183661','5354866','25112','5369620','2442357','108464','5343550','5383957','5383957','5391767','5386230','23856','5391264','148640','185262','5394196','5362384','5393103','158212','5243326','5371509','5392854','5388302','183326','181716','181706','5392749','5388558','155242','5381077','5348421','5368312','152264','5243024','104579','5373066','5385900','1436','5393016','2351495','95321','5390726','157124','5391717','189609','5377665','5382034','5382121','5389691','158683','194393','6630','185247','5378','207676','13837','5373166','23466','15474','156931','5372835','5388169','5392368','5392702','5355853','2455626','5392109','92880','179770','5391375','162337','210843','193791','5368089','5376138','5389703','26571','5389260','83098','5385828','12137','150759','25470','5373513','5366227','87135','156755','5381694','157612','5393404','204333','2583192','192745','5393','154497','2183185','24406','5386','183362','5365121','188487','199444','5393652','13030','5394056','7888','2288574','198622','5394060','191345','5392893','5387175','5390197','179593','5391468','194682','5393417','157730','190503','5354878','5385637','148620','201579','5377590','13383','5392598','106142','185883','5373037','152308','5385235','5368346','202517','5391837','14472','6373','5242663','5242683','2479168','80866','5391217','2186601','5367305','201947','5362403','6377','6376','5385625','11827')
#   and da.active_product_cnt <> 0
# ;

library(data.table)
oldModel <- setDT(read.csv("oldModelOutput.csv"))
newModel <- setDT(read.csv("newModelOutput.csv"))
newestModel <- setDT(read.csv("newestModelOutput.csv"))

newData <- newModel[dealer_legacy_id %in% unique(oldModel$dealer_legacy_id)]
newData <- newData[, .(dealer_legacy_id, churn_result_30, prob_30)]
newData$churn_result_new <- ifelse(newData$churn_result_30 == 'as is', 0, 1)
newData$churn_result_30 <- NULL
names(newData) <- c("dealer_legacy_id", "churn_raw_new", "churn_result_new")
fullData <- merge(oldModel, newData, by = "dealer_legacy_id")

newestData <- newestModel[dealer_legacy_id %in% unique(oldModel$dealer_legacy_id)]
newestData <- newestData[, .(dealer_legacy_id, churn_result_30, prob_30)]
newestData$churn_result_newest <- ifelse(newestData$churn_result_30 == 'as is', 0, 1)
newestData$churn_result_30 <- NULL
names(newestData) <- c("dealer_legacy_id", "churn_raw_newest", "churn_result_newest")
fullData <- merge(fullData, newestData, by = "dealer_legacy_id")

newModel <- newModel[, .(dealer_legacy_id, churn_result_30, prob_30)]
newModel$churn_result_new <- ifelse(newModel$churn_result_30 == 'as is', 0, 1)
newModel$churn_result_30 <- NULL
names(newModel) <- c("dealer_legacy_id", "churn_raw_new", "churn_result_new")
newModel <- na.omit(newModel)
summary(newModel)

table(fullData$churn_result)
table(fullData$churn_result_new)
table(fullData$churn_result_newest)
