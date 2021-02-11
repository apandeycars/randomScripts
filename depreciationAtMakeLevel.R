options(width = 100)
library(arrow)
library(data.table)
input <- setDT(read_parquet(file = "depreciation.parq"))
summary(input)
'%ni%' <- Negate('%in%')
lb_drop_pct <- quantile(input$drop_pct, 0.05)
ub_drop_pct <- quantile(input$drop_pct, 0.97)

cleanData <- input[drop_pct >= lb_drop_pct & drop_pct <= ub_drop_pct]
cleanData <- cleanData[avg_start_price > 1000 & avg_start_price < 200000]
cleanData <- cleanData[n_dps >= 20]
cleanData <- cleanData[drop_pct > 0]
cleanData <- cleanData[trim_name %ni% c('<NA>', 'Unknown', 'NoName')]
cleanData <- cleanData[!(is.na(trim_name))]

agg_data_make_model = cleanData[, .(
    avg_drop_30days = sum(n_dps*drop_pct)/sum(n_dps),
    vehicle_analysed = sum(n_dps)
  ), by = list(make_name, model_name)
][order(-avg_drop_30days)][vehicle_analysed>5000]
agg_data_make_model
agg_data_make_model$year_dep_value = 100-100*(1-(0.01*agg_data_make_model$avg_drop_30days))^12
write.csv(agg_data_make_model, file = "agg_data_make_model.csv", row.names = F)

agg_data_make = cleanData[, .(
    avg_drop_30days = sum(n_dps*drop_pct)/sum(n_dps),
    vehicle_analysed = sum(n_dps)
  ), by = list(make_name)
][order(-avg_drop_30days)][vehicle_analysed>25000]
agg_data_make$year_dep_value = 100-100*(1-(0.01*agg_data_make$avg_drop_30days))^12
agg_data_make
write.csv(agg_data_make, file = "agg_data_make.csv", row.names = F)

cleanData <- input[drop_pct >= lb_drop_pct & drop_pct <= ub_drop_pct]
cleanData <- cleanData[avg_start_price > 1000 & avg_start_price < 200000]
cleanData <- cleanData[n_dps >= 100]
cleanData <- cleanData[model_year >= 2018]
names(cleanData)[which(names(cleanData) == "n_dps")] <- "vehicle_analysed"
names(cleanData)[which(names(cleanData) == "drop_pct")] <- "avg_drop_30days"
cleanData$avg_pd <- NULL
cleanData$avg_start_price <- NULL
write.csv(cleanData, file = "mmyt_depreciation.csv", row.names = F)
