#merge all data
library(data.table)
library(stringr)
options(width = 150)
options(warn=-1)
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
load("fuzzy_match_input_processed_data.RData")
atlas_bu <- as.data.frame(atlas)
cars_bu <- as.data.frame(cars)
di_bu <- as.data.frame(di)
setwd("/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/datasets/")
state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
state_codes_usa <- str_sort(state_codes_usa)
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')

for (filename in list.files(pattern=".RData")) {
  load(filename)
}

add_columns <- function(input_data = NULL, cda = 'cars', add_data = NULL, columns_to_add = c('department_number', 'dealership_website'), ...) {
  if (!(cda %in% c("cars", "di", "atlas"))){
    stop("cda should be one of cars, di or atlas")
    return(NULL)
  }
  add_data <- add_data[, c(columns_to_add, 'row'), with = F]
  setDT(add_data)
  input_data <- merge(input_data, add_data, by.x = paste("row_", cda, sep = ""), by.y = 'row', all.x = T)
  for (i in columns_to_add) {
    names(input_data)[which(names(input_data) == i)] <- paste(i, "_", cda, sep = "")
  }
  return(input_data)
}

#CANADA
# CARS
output_cars_canada <- add_columns(input_data = output_cars_canada, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website')
)
output_cars_canada <- add_columns(input_data = output_cars_canada, cda = 'cars',
  add_data = cars, columns_to_add = c('phone', 'website')
)
output_cars_canada <- output_cars_canada[order(as.numeric(row_atlas))]
write.csv(output_cars_canada, file = "final/output_cars_canada.csv", row.names = F)

# DI
output_di_canada <- add_columns(input_data = output_di_canada, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website')
)
output_di_canada <- add_columns(input_data = output_di_canada, cda = 'di',
  add_data = di, columns_to_add = c('phone', 'website')
)
output_di_canada <- output_di_canada[order(as.numeric(row_atlas))]
write.csv(output_di_canada, file = "final/output_di_canada.csv", row.names = F)

# USA
# CARS
cars_usa_data <- do.call("rbind", list(output_cars_usa_post_ak, output_cars_usa_post_ca,
output_cars_usa_post_co, output_cars_usa_post_ga, output_cars_usa_post_il,
output_cars_usa_post_la, output_cars_usa_post_mi, output_cars_usa_post_nj,
output_cars_usa_post_ny, output_cars_usa_post_oh, output_cars_usa_post_pa,
output_cars_usa_post_ri, output_cars_usa_post_tn, output_cars_usa_post_tx,
output_cars_usa_post_ut, output_cars_usa_post_va, output_cars_usa_post_wa,
output_cars_usa))
output_cars_usa <- unique(cars_usa_data)
output_cars_usa <- output_cars_usa[, char_len:=nchar(as.character(sf_id_cars))]
"%ni%" <- Negate('%in%')
#output_cars_usa <- output_cars_usa[char_len == 18]
output_cars_usa <- add_columns(input_data = output_cars_usa, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_state')
)
output_cars_usa <- output_cars_usa[department_state_atlas %ni% c('FL', 'TX', 'CA', 'PA')]

#output_cars_usa$dealership_address_cars <- NULL
#output_cars_usa$dealership_name_cars <- NULL
#output_cars_usa$sf_id_cars <- NULL
#output_cars_usa <- output_cars_usa[, max_score:=as.numeric(as.character(max_score))]

# Remove TX, FL and CA
# Florida:
load("output_cars_usa_fl.RData")
load("output_cars_usa_tx.RData")
load("output_cars_usa_ca.RData")
load("output_cars_usa_pa.RData")

output_cars_usa_updated <- rbind(output_cars_usa_ca, output_cars_usa_fl)
output_cars_usa_updated <- rbind(output_cars_usa_updated, output_cars_usa_tx)
output_cars_usa_updated <- rbind(output_cars_usa_updated, output_cars_usa_pa)
output_cars_usa_updated <- output_cars_usa_updated[, char_len:=nchar(as.character(sf_id_cars))]

output_cars_usa <- output_cars_usa[, department_state_atlas:=NULL]
output_cars_usa <- rbind(output_cars_usa, output_cars_usa_updated)
output_cars_usa <- unique(output_cars_usa)

# output_cars_usa <- add_columns(input_data = output_cars_usa, cda = 'atlas',
#   add_data = atlas, columns_to_add = c('department_state')
# )

# output_cars_usa <- add_columns(input_data = output_cars_usa, cda = 'cars',
#   add_data = cars, columns_to_add = c('name', 'id', 'dealership_address', 'unique_all_info')
# )
#output_cars_usa$unique_all_info_cars <- NULL
output_cars_usa$char_len <- NULL
# names(output_cars_usa)[which(names(output_cars_usa) == 'id_cars')] <- 'sf_id_cars'
# names(output_cars_usa)[which(names(output_cars_usa) == 'name_cars')] <- 'dealership_name_cars'

output_cars_usa <- add_columns(input_data = output_cars_usa, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website')
)
output_cars_usa <- add_columns(input_data = output_cars_usa, cda = 'cars',
  add_data = cars, columns_to_add = c('phone', 'website')
)

output_cars_usa <- output_cars_usa[order(as.numeric(row_atlas))]
setcolorder(output_cars_usa, c("row_cars", "row_atlas", "max_score", "dealership_id_atlas", "sf_id_cars", "dealership_name_atlas", "dealership_name_cars", "department_address_atlas", "dealership_address_cars", "department_number_atlas", "dealership_website_atlas", "phone_cars", "website_cars"))
write.csv(output_cars_usa, file = "final/output_cars_usa.csv", row.names = F)

# DI
di_usa_data <- do.call("rbind", list(output_di_usa_post_az, output_di_usa_post_fl,
output_di_usa_post_il, output_di_usa_post_mi, output_di_usa_post_ny,
output_di_usa_post_oh, output_di_usa_post_tx, output_di_usa_post_ak
))
output_di_usa <- unique(di_usa_data)
#output_di_usa <- merge(output_di_usa, atlas[, .(row, department_state)], by.x = 'row_atlas', by.y = 'row', all.x = T)

output_di_usa <- output_di_usa[, char_len:=nchar(as.character(dealership_id_atlas))]
output_di_usa <- output_di_usa[char_len == 15]
output_di_usa <- output_di_usa[, char_len:=NULL]
output_di_usa <- output_di_usa[, max_score:=as.numeric(as.character(max_score))]
# output_di_usa <- output_di_usa[, dealership_name_di:=as.character(dealership_name_di)]
# output_di_usa <- output_di_usa[output_di_usa[, .I[max_score == max(max_score)], by=row_atlas]$V1]
# output_di_usa <- merge(output_di_usa, di[, .(name, row)], by.x='row_di', by.y='row', all.x=T)
# output_di_usa <- output_di_usa[, dealership_name_di:=as.character(name)]
# output_di_usa <- output_di_usa[, department_state:=NULL]
# output_di_usa <- output_di_usa[, name:=NULL]

output_di_usa <- add_columns(input_data = output_di_usa, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website')
)
output_di_usa <- add_columns(input_data = output_di_usa, cda = 'di',
  add_data = di, columns_to_add = c('phone', 'website')
)
output_di_usa <- output_di_usa[order(as.numeric(row_atlas))]
write.csv(output_di_usa, file = "final/output_di_usa.csv", row.names = F)
#data_to_merge <- unique(output_di_usa[, .(row_atlas, dealership_id_atlas)])
#a <- merge(atlas[department_country == 'United States'], data_to_merge, by.x = "row", by.y = "row_atlas", all.x = T)
#diff_state <- unique(a[is.na(dealership_id_atlas), department_state])
