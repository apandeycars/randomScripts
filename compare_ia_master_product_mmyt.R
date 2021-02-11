#compare_ia_master_product_mmyt.R
library(data.table)
library(arrow)
library(readr)
options(width = 150)
ia_mmyt <- setDT(read_csv("/Users/addhyanpandey/Desktop/randomRcodes/datasets/mmyt_ia.csv"))
master_mmyt <- setDT(read_parquet(file = "/Users/addhyanpandey/Desktop/randomRcodes/datasets/master_mmyt_2020-08-26.snappy.parquet"))
master_mmyt <- master_mmyt[product_typ_cd == 'CarsMMYT']
master_mmyt <- master_mmyt[, .(product_id, product_nm, vehicle_make_id, vehicle_model_id, vehicle_year_id, vehicle_trim_id)]
vehicle_trim <- setDT(read_csv("/Users/addhyanpandey/Desktop/randomRcodes/datasets/vehicle_trim.csv"))
make_model <- setDT(read_csv("/Users/addhyanpandey/Desktop/randomRcodes/datasets/make_model.csv"))
model_year <- setDT(read_csv("/Users/addhyanpandey/Desktop/randomRcodes/datasets/model_year.csv"))

ia_mmyt <- ia_mmyt[, mmyt:=paste(make_name, model_name, model_year, trim_name, sep = " ")]
ia_mmyt$mmyt_l <- tolower(ia_mmyt$mmyt)
master_mmyt$product_nm_l <- tolower(master_mmyt$product_nm)
ia_mmyt$mmyt_l <- str_replace_all(ia_mmyt$mmyt_l , "[^[:alnum:]]", " ")
master_mmyt$product_nm_l <- str_replace_all(master_mmyt$product_nm_l, "[^[:alnum:]]", " ")
fullList <- merge(ia_mmyt, master_mmyt, by.x = 'mmyt_l', by.y = 'product_nm_l', all.x = T)
fullList <- fullList[is.na(vehicle_trim_id) & !is.na(trim_name) & trim_name != 'Unknown'][order(-n_rows)]
write.csv(fullList[, .(mmyt, make_name, model_name, model_year, trim_name, n_rows)], "/Users/addhyanpandey/Desktop/randomRcodes/datasets/MMYT2Add.csv", row.names = F)
