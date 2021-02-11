#merge all data
library(wordVectors)
library(stringr)
library(readr)
library(runner)
library(dplyr)
library(tm)
library(lsa)
library(proxy)
library(dialr)
library(data.table)
library(stringdist)
options(width = 150)
options(warn=-1)
setwd("/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/datasets/")
source('/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/udf_fuzzy_match_names.R')
state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
state_codes_usa <- str_sort(state_codes_usa)
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')
'%ni%' <- Negate('%in%')

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

# CARS
cars_usa_data <- do.call("rbind", list(output_cars_usa, output_cars_usa_nd,
output_cars_usa_wy, output_cars_usa_other))
cars_canada_data <- do.call("rbind", list(output_cars_canada, output_cars_canada_other))
cars_all <- rbind(cars_usa_data, cars_canada_data)
cars_all <- rbind(cars_all, output_cars_other)
cars_all <- cars_all[, .(row_cars, row_atlas, sf_id_cars, dealership_id_atlas, max_score, dealership_name_cars, dealership_name_atlas, dealership_address_cars, department_address_atlas, notes)]
cars_all <- add_columns(input_data = cars_all, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website', 'manufacturer_name', 'ownership_group_legal_name', 'make_name')
)
cars_all <- add_columns(input_data = cars_all, cda = 'cars',
  add_data = cars, columns_to_add = c('phone', 'website', 'franchise_independent')
)

# cars_all <- add_columns(input_data = cars_all, cda = 'atlas',
#   add_data = atlas, columns_to_add = c('clean_website', 'unique_all_info')
# )
#
# cars_all <- add_columns(input_data = cars_all, cda = 'cars',
#   add_data = cars, columns_to_add = c('clean_website', 'unique_all_info')
# )

cars_all$max_score <- as.numeric(as.character(cars_all$max_score))
cars_all$max_score <- round(cars_all$max_score, 3)
cars_all$notes <- as.character(cars_all$notes)

# qa_data <- cars_all[max_score==1]
# qa_data <- qa_data[, max_score:=get_score(base_statement = unique_all_info_cars, atlas_statement = unique_all_info_atlas)]
#
# cars_all <- rbind(cars_all[row_cars %ni% qa_data$row_cars], qa_data)
# cars_all$notes <- ifelse(cars_all$clean_website_cars %in% c('', ' '), '', cars_all$notes)

cols_to_delete <- c('row_cars', 'row_atlas', 'clean_website_cars', 'clean_website_atlas',
  'unique_all_info_atlas', 'unique_all_info_cars'
)
cars_all$max_score <- as.numeric(as.character(cars_all$max_score))
cars_all$max_score <- round(cars_all$max_score, 3)
cars_all$notes <- as.character(cars_all$notes)
cars_all[is.na(cars_all)] <- ""

cars_all <- unique(cars_all)
dupData <- cars_all[, .N, sf_id_cars][order(-N)][N>1]$sf_id_cars
tempData <- cars_all[sf_id_cars %in% unique(dupData)]
tempData <- tempData[row_atlas!='']
cars_all <- rbind(cars_all [sf_id_cars %ni% dupData], tempData)
cars_all <- cars_all[, name_match := get_score(base_statement = tolower(dealership_name_cars), atlas_statement = tolower(dealership_name_atlas))]
cars_all <- cars_all[, name_match := ifelse(name_match >= 0.9, 'similar_name', '')]
cars_all <- cars_all[, address_match := get_score(base_statement = tolower(dealership_address_cars), atlas_statement = tolower(department_address_atlas))]
cars_all <- cars_all[, address_match := ifelse(address_match >= 0.9, 'similar_address', '')]

write.csv(cars_all, file = "cars_matched_output.csv", row.names = F)

#cars_all$notes <- ifelse(cars_all$notes == 'website_match' & cars_all$website_cars == ' ', 'name_or_address_match', cars_all$notes)

for (filename in list.files(pattern=".RData")) {
  load(filename)
}
di_usa_data <- do.call("rbind", list(output_di_usa, output_di_usa_nd,
output_di_usa_wy, output_di_usa_other))
di_canada_data <- do.call("rbind", list(output_di_canada, output_di_canada_other))
di_all <- rbind(di_usa_data, di_canada_data)
di_all <- rbind(di_all, output_di_other)
di_all <- di_all[, .(row_di, row_atlas, sf_id_di, dealership_id_atlas, max_score, dealership_name_di, dealership_name_atlas, dealership_address_di, department_address_atlas, notes)]
di_all <- add_columns(input_data = di_all, cda = 'atlas',
  add_data = atlas, columns_to_add = c('department_number', 'dealership_website', 'manufacturer_name', 'ownership_group_legal_name', 'make_name')
)
di_all <- add_columns(input_data = di_all, cda = 'di',
  add_data = di, columns_to_add = c('phone', 'website', 'current_customer')
)

#di_all[notes == 'website_match' & (website_di == ' ' | dealership_website_atlas == ' ')][order(-max_score)]
di_all <- add_columns(input_data = di_all, cda = 'atlas',
  add_data = atlas, columns_to_add = c('clean_website', 'unique_all_info')
)

di_all <- add_columns(input_data = di_all, cda = 'di',
  add_data = di, columns_to_add = c('clean_website', 'unique_all_info')
)

qa_data <- di_all[max_score==1]
qa_data <- qa_data[, max_score:=get_score(base_statement = unique_all_info_di, atlas_statement = unique_all_info_atlas)]

di_all <- rbind(di_all[row_di %ni% qa_data$row_di], qa_data)
di_all$notes <- as.character(di_all$notes)
di_all$notes <- ifelse(di_all$clean_website_di %in% c('', ' '), '', di_all$notes)

cols_to_delete <- c('row_di', 'row_atlas', 'clean_website_di', 'clean_website_atlas',
  'unique_all_info_atlas', 'unique_all_info_di'
)
di_all$max_score <- as.numeric(as.character(di_all$max_score))
di_all$max_score <- round(di_all$max_score, 3)
di_all$notes <- as.character(di_all$notes)
di_all[is.na(di_all)] <- ""

set(di_all, , cols_to_delete, NULL)
di_all$max_score <- as.numeric(as.character(di_all$max_score))
di_all$max_score <- round(di_all$max_score, 3)
di_all$notes <- as.character(di_all$notes)
di_all$notes <- ifelse(di_all$notes == 'website_match' & di_all$website_di == ' ', '', di_all$notes)
write.csv(di_all, file = "di_matched_output.csv", row.names = F)
