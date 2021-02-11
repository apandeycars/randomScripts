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

config <- c()
config$baseDataPath <- getwd()
state_codes_usa <- "ND"
state_codes_usa <- str_sort(state_codes_usa)

load("fuzzy_match_input_processed_data.RData")
atlas_bu <- as.data.frame(atlas)
di_bu <- as.data.frame(di)

# atlas <- atlas[department_state == 'AK']
# di <- di[billingstate == 'AK']

## USA
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
atlas <- atlas[department_country == "United States"]
di <- di[billingcountry == "United States"]

output <- c()
for (state_code in state_codes_usa) {

  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code),])
  di <- as.data.table(di_bu[which(di_bu$billingstate == state_code),])
  message("Rows in Atlas: ", nrow(atlas), " Rows in di: ", nrow(di), "\n")

# Check if di has zero rows, in case that's true, go to next.
  if(nrow(di) == 0) next ;

  if (nrow(atlas) == 0){
    message("This state doesn't have any data in ATLAS")
    output <- rbind(case_of_zero_atlas_record(di, as.data.table(atlas_bu)), output)
    output_di_usa <- as.data.frame(output)
    names(output_di_usa) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di')
    setDT(output_di_usa)
    save(output_di_usa, file = "output_di_usa.RData")
    next
  }

  di <- di[, normalised_city:=tolower(billingcity)]
  top_cities <- di[, .N, by = list(normalised_city)][order(-N)][N>=15]$normalised_city
  di$normalised_city <- ifelse(di$normalised_city %in% top_cities, di$normalised_city, 'other')
  top_cities <- c(top_cities, 'other')

  message("All the top cities: ", top_cities, "\n")
  print(table(di$normalised_city))

  for (nc in top_cities) {
    message("Running For: ", nc, "\n")
    initial_time <- Sys.time()
    di <- as.data.table(di_bu[which(di_bu$billingstate == state_code),])
    di <- di[, normalised_city:=tolower(billingcity)]
    di$normalised_city <- ifelse(di$normalised_city %in% top_cities, di$normalised_city, 'other')
    di <- di[normalised_city == nc]
    atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code),])
    atlas <- atlas[, normalised_city:=tolower(department_city)]

    if (nc == 'other') {
      atlas <- atlas[!(normalised_city %in% setdiff(top_cities, 'other'))]
    } else {
      atlas <- atlas[normalised_city == nc]
    }
    if (nrow(atlas) == 0){
      output <- rbind(case_of_zero_atlas_record(di, as.data.table(atlas_bu)), output)
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_usa),]), 0), "%", "\n")
      output_di_usa <- as.data.frame(output)
      names(output_di_usa) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
      setDT(output_di_usa)
      save(output_di_usa, file = "output_di_usa.RData")
      next
    }
    message("Rows in Atlas: ", nrow(atlas), " Rows in di: ", nrow(di), "\n")

    for (j in (1:nrow(di))){
      max_score <- 0
      statement_di <- as.character(di[j, unique_all_info])
      row_di <- as.character(di[j,row])
      sf_id_di = as.character(di[j,id])
      dealership_name_di <- as.character(di[j, name])
      dealership_address_di <- as.character(di[j,dealership_address])
      clean_website_di <- as.character(di[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_di, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score <- total_score
            max_statement_atlas <- statement_atlas
            row_atlas <- atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_di <- as.character(di[j,address])
            if (clean_website_atlas == clean_website_di & clean_website_atlas %ni% c('', ' ')) {
              notes <- 'website_match'
              max_score <- 0.7 + 0.3*round(total_score, 2)
            }
            else if (address_atlas == address_di & address_di != '') {
              notes <- 'address_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else notes <- ''
          }
        }
      tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di, clean_website_atlas, clean_website_di, notes)
      output <- rbind(output, tmp)
      if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_usa),]), 0), "%", "\n")
    }
    city_end_time <- Sys.time()
    print(city_end_time - initial_time)
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_usa),]), 0), "%", "\n")
    output_di_usa <- as.data.frame(output)
    names(output_di_usa) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
    setDT(output_di_usa)
    save(output_di_usa, file = "output_di_usa.RData")
  }
}

# Starting Others of the USA.
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
atlas <- atlas[department_country == "United States"]
di <- di[billingcountry == "United States" & billingstate == 'other']

output_di_usa_other <- c()
output <- c()

for (j in (1:nrow(di))){
  max_score <- 0
  address_flag <- 1
  atlas <- as.data.table(atlas_bu)[department_country == "United States"]
  if(di[j, clean_website] == '' & nchar(di[j, billingcity]) == 1){
    tmp <- add_empty_match(di[j,])
    output <- rbind(output, tmp)
    if ((nrow(output) %% 20) == 0) {
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate == "other" & di_bu$billingcountry == "United States"),]), 0), "%", "\n")
      output_di_usa_other <- as.data.frame(output)
      names(output_di_usa_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
      setDT(output_di_usa_other)
      save(output_di_usa_other, file = "output_di_usa_other.RData")
    }
    next
  } else {
    tmp_billingcity <- tolower(di[j, billingcity])
    message("City Found: ", tmp_billingcity)
    atlas <- atlas[tolower(department_city) == tmp_billingcity]
    if (nrow(atlas) == 0){
      message("Didn't find matching Atlas City")
      output <- rbind(output, add_empty_match(di[j,]))
      next
    } else {
      message("Found matching atlas data with ", nrow(atlas), " rows in ", tmp_billingcity, " city")
      statement_di <- as.character(di[j, unique_all_info])
      row_di <- as.character(di[j,row])
      notes <- ''
      sf_id_di = as.character(di[j,id])
      dealership_name_di <- as.character(di[j, name])
      dealership_address_di <- as.character(di[j,dealership_address])
      clean_website_di <- as.character(di[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_di, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score = total_score
            max_statement_atlas = statement_atlas
            row_atlas = atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_di <- as.character(di[j,address])
            if (clean_website_atlas == clean_website_di & clean_website_atlas %ni% c('', ' ')) {
              notes <- 'website_match'
              max_score <- 0.7 + 0.3*round(total_score, 2)
            }
            else if (address_atlas == address_di & address_di != '') {
              notes <- 'address_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else notes <- ''
          }
        }
    }
    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di, clean_website_atlas, clean_website_di, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate == "other" & di_bu$billingcountry == "United States"),]), 0), "%", "\n")
    output_di_usa_other <- as.data.frame(output)
    names(output_di_usa_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
    setDT(output_di_usa_other)
    save(output_di_usa_other, file = "output_di_usa_other.RData")
  }
}
output_di_usa_other <- as.data.frame(output)
names(output_di_usa_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
setDT(output_di_usa_other)
save(output_di_usa_other, file = "output_di_usa_other.RData")

## Canada
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
atlas <- atlas[department_country == "Canada"]
di <- di[billingcountry == "Canada"]

output_di_canada <- c()
output <- c()
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')
for (state_code in state_codes_canada) {

  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code & atlas_bu$department_country == "Canada"),])
  di <- as.data.table(di_bu[which(di_bu$billingstate == state_code & di_bu$billingcountry == "Canada"),])
  message("Rows in Atlas: ", nrow(atlas), " Rows in di: ", nrow(di), "\n")

  if(nrow(di) == 0) next ;

  if (nrow(atlas) == 0){
    message("This state doesn't have any data in ATLAS")
    output <- rbind(case_of_zero_atlas_record(di, as.data.table(atlas_bu)), output)
    output_di_canada <- as.data.frame(output)
    names(output_di_canada) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
    setDT(output_di_canada)
    save(output_di_canada, file = "output_di_canada.RData")
    next
  }

    for (j in (1:nrow(di))){
      max_score = 0
      address_flag <- 0
      statement_di <- as.character(di[j, unique_all_info])
      row_di <- as.character(di[j,row])
      sf_id_di = as.character(di[j,id])
      dealership_name_di <- as.character(di[j, name])
      dealership_address_di <- as.character(di[j,dealership_address])
      clean_website_di <- as.character(di[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_di, atlas_statement = statement_atlas)
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
            address_di <- as.character(di[j,address])
            if (clean_website_atlas == clean_website_di & clean_website_atlas %ni% c('', ' ')) {
              notes <- 'website_match'
              max_score <- 0.7 + 0.3*round(total_score, 2)
            }
            else if (address_atlas == address_di & address_di != '') {
              notes <- 'address_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else notes <- ''
          }
        }
#      }
    #message(max_score, " ATLAS:", statement_atlas, " di:", max_statement_di, "\n")
      tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di, clean_website_atlas, clean_website_di, notes)
      output <- rbind(output, tmp)
      if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_canada),]), 0), "%", "\n")
    }

  message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_canada),]), 0), "%", "\n")
  output_di_canada <- as.data.frame(output)
  names(output_di_canada) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
  setDT(output_di_canada)
  save(output_di_canada, file = "output_di_canada.RData")
}

#Others of Canada
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
atlas <- atlas[department_country == "Canada"]
di <- di[billingcountry == "Canada" & billingstate == 'other']

output_di_canada_other <- c()
output <- c()

for (j in (1:nrow(di))){
  max_score <- 0
  address_flag <- 0
  if(di[j, clean_website] == '' & nchar(di[j, dealership_address]) == 22){
    tmp <- add_empty_match(di[j,])
    output <- rbind(output, tmp)
  } else {
    statement_di <- as.character(di[j, unique_all_info])
    row_di <- as.character(di[j,row])
    sf_id_di = as.character(di[j,id])
    dealership_name_di <- as.character(di[j, name])
    dealership_address_di <- as.character(di[j,dealership_address])
    clean_website_di <- as.character(di[j,clean_website])
      for (i in (1:nrow(atlas))){
        statement_atlas <- as.character(atlas[i, unique_all_info])
        total_score <- get_score(base_statement = statement_di, atlas_statement = statement_atlas)
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
          address_di <- as.character(di[j,address])
          if (clean_website_atlas == clean_website_di & clean_website_atlas %ni% c('', ' ')) {
            notes <- 'website_match'
            max_score <- 0.7 + 0.3*round(total_score, 2)
          }
          else if (address_atlas == address_di & address_di != '') {
            notes <- 'address_match'
            max_score <- 0.5 + 0.5*round(total_score, 2)
          }
          else notes <- ''
        }
      }

    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di, clean_website_atlas, clean_website_di, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate == "other" & di_bu$billingcountry == "Canada"),]), 0), "%", "\n")
    output_di_canada_other <- as.data.frame(output)
    names(output_di_canada_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
    setDT(output_di_canada_other)
    save(output_di_canada_other, file = "output_di_canada_other.RData")
  }
}
output_di_canada_other <- as.data.frame(output)
names(output_di_canada_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
setDT(output_di_canada_other)
save(output_di_canada_other, file = "output_di_canada_other.RData")

#Others all
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
di <- di[billingcountry == "other"]

output_di_other <- c()
output <- c()

for (j in (1:nrow(di))){
  max_score = 0
  address_flag <- 0
  atlas <- as.data.table(atlas_bu)
  if(di[j, clean_website] == '' & nchar(di[j, dealership_address]) == 21){
    message("Place of bad data")
    tmp <- add_empty_match(di[j,])
    output <- rbind(output, tmp)
    if ((nrow(output) %% 20) == 0) {
      message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingcountry == "other"),]), 0), "%", "\n")
      output_di_other <- as.data.frame(output)
      names(output_di_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
      setDT(output_di_other)
      save(output_di_other, file = "output_di_other.RData")
    }
    next
  } else {
    tmp_billingcity <- tolower(di[j, billingcity])
    message("City Found: ", tmp_billingcity)
    atlas <- atlas[tolower(department_city) == tmp_billingcity]
    if (nrow(atlas) == 0){
      message("Didn't find matching Atlas City")
      output <- rbind(output, add_empty_match(di[j,]))
      next
    } else {
      message("Found matching atlas data with ", nrow(atlas), " rows in ", tmp_billingcity, " city")
      statement_di <- as.character(di[j, unique_all_info])
      row_di <- as.character(di[j,row])
      sf_id_di = as.character(di[j,id])
      dealership_name_di <- as.character(di[j, name])
      dealership_address_di <- as.character(di[j,dealership_address])
      clean_website_di <- as.character(di[j,clean_website])
        for (i in (1:nrow(atlas))){
          statement_atlas <- as.character(atlas[i, unique_all_info])
          total_score <- get_score(base_statement = statement_di, atlas_statement = statement_atlas)
          if(total_score > max_score){
            max_score <- total_score
            max_statement_atlas <- statement_atlas
            row_atlas <- atlas[i,row]
            dealership_id_atlas <- atlas[i, dealership_id]
            department_address_atlas <- as.character(atlas[i, department_address])
            dealership_name_atlas <- as.character(atlas[i,dealership_name])
            clean_website_atlas <- as.character(atlas[i,clean_website])
            address_atlas <- as.character(atlas[i,address])
            address_di <- as.character(di[j,address])
            if (clean_website_atlas == clean_website_di & clean_website_atlas %ni% c('', ' ')) {
              notes <- 'website_match'
              max_score <- 0.7 + 0.3*round(total_score, 2)
            }
            else if (address_atlas == address_di & address_di != '') {
              notes <- 'address_match'
              max_score <- 0.5 + 0.5*round(total_score, 2)
            }
            else notes <- ''
          }
        }
    }
    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di, clean_website_atlas, clean_website_di, notes)
    output <- rbind(output, tmp)
  }
  if ((nrow(output) %% 20) == 0) {
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingcountry == "other"),]), 0), "%", "\n")
    output_di_other <- as.data.frame(output)
    names(output_di_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
    setDT(output_di_other)
    save(output_di_other, file = "output_di_other.RData")
  }
}
output_di_other <- as.data.frame(output)
names(output_di_other) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di', 'clean_website_atlas', 'clean_website_di', 'notes')
setDT(output_di_other)
save(output_di_other, file = "output_di_other.RData")
