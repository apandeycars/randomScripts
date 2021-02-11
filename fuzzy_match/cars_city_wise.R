# install.packages(c('runner', 'lsa', 'tm', 'proxy', 'dialr', 'stringdist'))
# library("devtools")
# devtools::install_github("bmschmidt/wordVectors")
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
'%ni%' <- Negate('%in%')

# setwd("/app/workarea/apandey/datasets/")
setwd("/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/datasets/")
source('/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/udf_fuzzy_match_names.R')

all_state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
all_state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')

#state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
remaining <- c('AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
running <- c('AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
done <- c()
config <- c()
config$baseDataPath <- getwd()
state_codes_usa <- c('ND')
state_codes_usa <- str_sort(state_codes_usa)

load("fuzzy_match_input_processed_data.RData")
atlas_bu <- as.data.frame(atlas)
cars_bu <- as.data.frame(cars)

# atlas <- atlas[department_state == 'AK']
# cars <- cars[shippingstate == 'AK']

## USA
atlas <- as.data.table(atlas_bu)
cars <- as.data.table(cars_bu)
atlas <- atlas[department_country == "United States"]
cars <- cars[shippingcountry == "United States"]

state_codes_usa <- 'WI'
output <- c()
for (state_code in state_codes_usa) {

  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code),])
  cars <- as.data.table(cars_bu[which(cars_bu$shippingstate == state_code),])
  message("Rows in Atlas: ", nrow(atlas), " Rows in cars: ", nrow(cars), "\n")

# Check if cars has zero rows, in case that's true, go to next.
  if(nrow(cars) == 0) next ;

  if (nrow(atlas) == 0){
    message("This state doesn't have any data in ATLAS")
    output <- rbind(case_of_zero_atlas_record(cars, as.data.table(atlas_bu)), output)
    output_cars_usa <- as.data.frame(output)
    names(output_cars_usa) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars')
    setDT(output_cars_usa)
    save(output_cars_usa, file = "output_cars_usa.RData")
    next
  }

  cars <- cars[, normalised_city:=tolower(shippingcity)]
  top_cities <- cars[, .N, by = list(normalised_city)][order(-N)][N>=20]$normalised_city
  cars$normalised_city <- ifelse(cars$normalised_city %in% top_cities, cars$normalised_city, 'other')
  top_cities <- c(top_cities, 'other')

  message("All the top cities: ", top_cities, "\n")
  print(table(cars$normalised_city))

  for (nc in top_cities) {
    message("Running For: ", nc, "\n")
    initial_time <- Sys.time()
    cars <- as.data.table(cars_bu[which(cars_bu$shippingstate == state_code),])
    cars <- cars[, normalised_city:=tolower(shippingcity)]
    cars$normalised_city <- ifelse(cars$normalised_city %in% top_cities, cars$normalised_city, 'other')
    cars <- cars[normalised_city == nc]
    atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code),])
    atlas <- atlas[, normalised_city:=tolower(department_city)]

    cars[is.na(cars)] <- ""

    if (nrow(cars) == 0) next;

    if (nc == 'other') {
      atlas <- atlas[!(normalised_city %in% setdiff(top_cities, 'other'))]
    } else {
      atlas <- atlas[normalised_city == nc]
    }
    if (nrow(atlas) == 0){
      output <- rbind(case_of_zero_atlas_record(cars, as.data.table(atlas_bu)), output)
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate %in% state_codes_usa),]), 0), "%", "\n")
      output_cars_usa <- as.data.frame(output)
      names(output_cars_usa) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
      setDT(output_cars_usa)
      save(output_cars_usa, file = "output_cars_usa.RData")
      next
    }
    message("Rows in Atlas: ", nrow(atlas), " Rows in cars: ", nrow(cars), "\n")

    for (j in (1:nrow(cars))){
      max_score <- 0
      statement_cars <- as.character(cars[j, unique_all_info])
      row_cars <- as.character(cars[j,row])
      sf_id_cars = as.character(cars[j,id])
      dealership_name_cars <- as.character(cars[j, name])
      dealership_address_cars <- as.character(cars[j,dealership_address])
      clean_website_cars <- as.character(cars[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_cars, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score = total_score
            max_statement_atlas = statement_atlas
            row_atlas = atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_cars <- as.character(cars[j,address])
            if (clean_website_atlas == clean_website_cars & clean_website_atlas %ni% c('', ' ')) {
              notes <- 'website_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else if (address_atlas == address_cars & address_cars != '') {
              notes <- 'address_match'
              max_score <- 0.6 + 0.4*round(total_score, 2)
            }
            else notes <- ''
          }
        }
      tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars, clean_website_atlas, clean_website_cars, notes)
      output <- rbind(output, tmp)
      if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate %in% state_codes_usa),]), 0), "%", "\n")
    }
    city_end_time <- Sys.time()
    print(city_end_time - initial_time)
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate %in% state_codes_usa),]), 0), "%", "\n")
    output_cars_usa <- as.data.frame(output)
    names(output_cars_usa) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
    setDT(output_cars_usa)
    save(output_cars_usa, file = "output_cars_usa.RData")
  }
}

# Starting Others of the USA.
atlas <- as.data.table(atlas_bu)
cars <- as.data.table(cars_bu)
atlas <- atlas[department_country == "United States"]
cars <- cars[shippingcountry == "United States" & shippingstate == 'other']

output_cars_usa_other <- c()
output <- c()

for (j in (1:nrow(cars))){
  max_score <- 0
  address_flag <- 1
  atlas <- as.data.table(atlas_bu)[department_country == "United States"]
  if(cars[j, clean_website] == '' & nchar(cars[j, shippingcity]) == 1){
    tmp <- add_empty_match(cars[j,])
    output <- rbind(output, tmp)
    if ((nrow(output) %% 20) == 0) {
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate == "other" & cars_bu$shippingcountry == "United States"),]), 0), "%", "\n")
      output_cars_usa_other <- as.data.frame(output)
      names(output_cars_usa_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
      setDT(output_cars_usa_other)
      save(output_cars_usa_other, file = "output_cars_usa_other.RData")
    }
    next
  } else {
    tmp_shippingcity <- tolower(cars[j, shippingcity])
    message("City Found: ", tmp_shippingcity)
    atlas <- atlas[tolower(department_city) == tmp_shippingcity]
    if (nrow(atlas) == 0){
      message("Didn't find matching Atlas City")
      output <- rbind(output, add_empty_match(cars[j,]))
      next
    } else {
      message("Found matching atlas data with ", nrow(atlas), " rows in ", tmp_shippingcity, " city")
      statement_cars <- as.character(cars[j, unique_all_info])
      row_cars <- as.character(cars[j,row])
      notes <- ''
      sf_id_cars = as.character(cars[j,id])
      dealership_name_cars <- as.character(cars[j, name])
      dealership_address_cars <- as.character(cars[j,dealership_address])
      clean_website_cars <- as.character(cars[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_cars, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score = total_score
            max_statement_atlas = statement_atlas
            row_atlas = atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_cars <- as.character(cars[j,address])
            if (clean_website_atlas == clean_website_cars & clean_website_atlas != '') {
              notes <- 'website_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else if (address_atlas == address_cars & address_cars != '') {
              notes <- 'address_match'
              max_score <- 0.6 + 0.4*round(total_score, 2)
            }
            else notes <- ''
          }
        }
    }
    tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars, clean_website_atlas, clean_website_cars, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate == "other" & cars_bu$shippingcountry == "United States"),]), 0), "%", "\n")
    output_cars_usa_other <- as.data.frame(output)
    names(output_cars_usa_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
    setDT(output_cars_usa_other)
    save(output_cars_usa_other, file = "output_cars_usa_other.RData")
  }
}
output_cars_usa_other <- as.data.frame(output)
names(output_cars_usa_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
setDT(output_cars_usa_other)
save(output_cars_usa_other, file = "output_cars_usa_other.RData")

## Canada
atlas <- as.data.table(atlas_bu)
cars <- as.data.table(cars_bu)
atlas <- atlas[department_country == "Canada"]
cars <- cars[shippingcountry == "Canada"]

output_cars_canada <- c()
output <- c()
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')
for (state_code in state_codes_canada) {

  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code & atlas_bu$department_country == "Canada"),])
  cars <- as.data.table(cars_bu[which(cars_bu$shippingstate == state_code & cars_bu$shippingcountry == "Canada"),])
  message("Rows in Atlas: ", nrow(atlas), " Rows in cars: ", nrow(cars), "\n")

  if(nrow(cars) == 0) next ;

  if (nrow(atlas) == 0){
    message("This state doesn't have any data in ATLAS")
    output <- rbind(case_of_zero_atlas_record(cars, as.data.table(atlas_bu)), output)
    output_cars_canada <- as.data.frame(output)
    names(output_cars_canada) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
    setDT(output_cars_canada)
    save(output_cars_canada, file = "output_cars_canada.RData")
    next
  }

    for (j in (1:nrow(cars))){
      max_score = 0
      address_flag <- 0
      statement_cars <- as.character(cars[j, unique_all_info])
      row_cars <- as.character(cars[j,row])
      sf_id_cars = as.character(cars[j,id])
      dealership_name_cars <- as.character(cars[j, name])
      dealership_address_cars <- as.character(cars[j,dealership_address])
      clean_website_cars <- as.character(cars[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_cars, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score = total_score
            if(address_flag == 0) notes <- ''
            max_statement_atlas = statement_atlas
            row_atlas = atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_cars <- as.character(cars[j,address])
            if (clean_website_atlas == clean_website_cars & clean_website_atlas != '') {
              notes <- 'website_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else if (address_atlas == address_cars & address_cars != '') {
              notes <- 'address_match'
              max_score <- 0.6 + 0.4*round(total_score, 2)
            }
            else notes <- ''
          }
        }
#      }
    #message(max_score, " ATLAS:", statement_atlas, " cars:", max_statement_cars, "\n")
      tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars, clean_website_atlas, clean_website_cars, notes)
      output <- rbind(output, tmp)
      if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate %in% state_codes_canada),]), 0), "%", "\n")
    }

  message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate %in% state_codes_canada),]), 0), "%", "\n")
  output_cars_canada <- as.data.frame(output)
  names(output_cars_canada) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
  setDT(output_cars_canada)
  save(output_cars_canada, file = "output_cars_canada.RData")
}

#Others of Canada
atlas <- as.data.table(atlas_bu)
cars <- as.data.table(cars_bu)
atlas <- atlas[department_country == "Canada"]
cars <- cars[shippingcountry == "Canada" & shippingstate == 'other']

output_cars_canada_other <- c()
output <- c()

for (j in (1:nrow(cars))){
  max_score <- 0
  address_flag <- 0
  if(cars[j, clean_website] == '' & nchar(cars[j, dealership_address]) == 22){
    tmp <- add_empty_match(cars[j,])
    output <- rbind(output, tmp)
  } else {
    statement_cars <- as.character(cars[j, unique_all_info])
    row_cars <- as.character(cars[j,row])
    sf_id_cars = as.character(cars[j,id])
    dealership_name_cars <- as.character(cars[j, name])
    dealership_address_cars <- as.character(cars[j,dealership_address])
    clean_website_cars <- as.character(cars[j,clean_website])
      for (i in (1:nrow(atlas))){
        statement_atlas <- as.character(atlas[i, unique_all_info])
        total_score <- get_score(base_statement = statement_cars, atlas_statement = statement_atlas)
        if(total_score > max_score){
          max_score = total_score
          if (address_flag == 0) notes <- ''
          max_statement_atlas = statement_atlas
          row_atlas = atlas[i,row]
          dealership_id_atlas <- atlas[i, dealership_id]
          department_address_atlas <- as.character(atlas[i, department_address])
          dealership_name_atlas <- as.character(atlas[i,dealership_name])
          clean_website_atlas <- as.character(atlas[i,clean_website])
          address_atlas <- as.character(atlas[i,address])
          address_cars <- as.character(cars[j,address])
          if (clean_website_atlas == clean_website_cars & clean_website_atlas != '') {
            notes <- 'website_match'
            max_score <- 0.5 + 0.5*round(total_score, 2)
          }
          else if (address_atlas == address_cars & address_cars != '') {
            notes <- 'address_match'
            max_score <- 0.6 + 0.4*round(total_score, 2)
          }
          else notes <- ''
        }
      }
    tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars, clean_website_atlas, clean_website_cars, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingstate == "other" & cars_bu$shippingcountry == "Canada"),]), 0), "%", "\n")
    output_cars_canada_other <- as.data.frame(output)
    names(output_cars_canada_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
    setDT(output_cars_canada_other)
    save(output_cars_canada_other, file = "output_cars_canada_other.RData")
  }
}
output_cars_canada_other <- as.data.frame(output)
names(output_cars_canada_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
setDT(output_cars_canada_other)
save(output_cars_canada_other, file = "output_cars_canada_other.RData")

#Others all
atlas <- as.data.table(atlas_bu)
cars <- as.data.table(cars_bu)
cars <- cars[shippingcountry == "other"]

output_cars_other <- c()
output <- c()

for (j in (1:nrow(cars))){
  max_score = 0
  address_flag <- 0
  atlas <- as.data.table(atlas_bu)
  if(cars[j, clean_website] == '' & nchar(cars[j, dealership_address]) == 21){
    message("Place of bad data")
    tmp <- add_empty_match(cars[j,])
    output <- rbind(output, tmp)
    if ((nrow(output) %% 20) == 0) {
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingcountry == "other"),]), 0), "%", "\n")
      output_cars_other <- as.data.frame(output)
      names(output_cars_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
      setDT(output_cars_other)
      save(output_cars_other, file = "output_cars_other.RData")
    }
    next
  } else {
    tmp_shippingcity <- tolower(cars[j, shippingcity])
    message("City Found: ", tmp_shippingcity)
    atlas <- atlas[tolower(department_city) == tmp_shippingcity]
    if (nrow(atlas) == 0){
      message("carsdn't find matching Atlas City")
      output <- rbind(output, add_empty_match(cars[j,]))
      next
    } else {
      message("Found matching atlas data with ", nrow(atlas), " rows in ", tmp_shippingcity, " city")
      statement_cars <- as.character(cars[j, unique_all_info])
      row_cars <- as.character(cars[j,row])
      sf_id_cars = as.character(cars[j,id])
      dealership_name_cars <- as.character(cars[j, name])
      dealership_address_cars <- as.character(cars[j,dealership_address])
      clean_website_cars <- as.character(cars[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_cars, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score <- total_score
            max_statement_atlas <- statement_atlas
            row_atlas <- atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_cars <- as.character(cars[j,address])
            if (clean_website_atlas == clean_website_cars & clean_website_atlas != '') {
              notes <- 'website_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else if (address_atlas == address_cars & address_cars != '') {
              notes <- 'address_match'
              max_score <- 0.6 + 0.4*round(total_score, 2)
            }
            else notes <- ''
          }
        }
    }
    tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars, clean_website_atlas, clean_website_cars, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(cars_bu[which(cars_bu$shippingcountry == "other"),]), 0), "%", "\n")
    output_cars_other <- as.data.frame(output)
    names(output_cars_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
    setDT(output_cars_other)
    save(output_cars_other, file = "output_cars_other.RData")
  }
}
output_cars_other <- as.data.frame(output)
names(output_cars_other) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
setDT(output_cars_other)
save(output_cars_other, file = "output_cars_other.RData")
