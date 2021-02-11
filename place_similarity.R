# install.packages(c('runner', 'lsa', 'tm', 'proxy', 'dialr', 'stringdist', 'stringdist'))
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
# setwd("/app/workarea/apandey/datasets/")
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
source('/Users/addhyanpandey/Desktop/randomScripts/udf_fuzzy_match_names.R')

config <- c()
config$baseDataPath <- getwd()
state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')

atlas <- setDT(read_csv('atlas.csv'))
cars <- setDT(read_csv('cars_account_data.csv'))
names(cars) <- tolower(names(cars))
cars <- cars[!(is.na(billingcity)) & billingcity != '']
di <- setDT(read_csv('di_account_data.csv'))
names(di) <- tolower(names(di))
di <- di[!(is.na(billingcity)) & billingcity != '']

cars$billingstate <- ifelse(is.na(state.abb[match(cars$billingstate,state.name)]), cars$billingstate, state.abb[match(cars$billingstate,state.name)])
cars <- cars[, billingstate:=gsub("[[:punct:]]", "", toupper(billingstate))]
cars$billingcountry <- ifelse(cars$billingcountry %in% c('USA', 'us', 'US', 'United State', 'United', 'united states', 'usa'), 'United States', cars$billingcountry)
cars <- cars[billingcountry %in% c('United States', 'Canada')]
cars <- cars[(billingcountry == 'United States' & billingstate %in% state_codes_usa) | (billingcountry == 'Canada' & billingstate %in% state_codes_canada)]

di$billingcountry <- ifelse(di$billingcountry %in% c('USA', 'us', 'US', 'United State', 'United', 'united states', 'usa'), 'United States', di$billingcountry)
di <- di[billingcountry %in% c('United States', 'Canada')]
di$billingstate <- ifelse(is.na(state.abb[match(di$billingstate,state.name)]), di$billingstate, state.abb[match(di$billingstate,state.name)])
di <- di[, billingstate:=gsub("[[:punct:]]", "", toupper(billingstate))]
di <- di[(billingcountry == 'United States' & billingstate %in% state_codes_usa) | (billingcountry == 'Canada' & billingstate %in% state_codes_canada)]

#Let's work on small dataset first, starting with zipcode 60647.
split_address <- as.data.frame(str_match(atlas$department_address,"(.+), (.+), (.+) (.+), (.+)")[ ,-1])
names(split_address) <- c('department_address_line_1', 'department_city', 'department_state', 'department_zipcode', 'department_country')
setDT(split_address)
atlas <- cbind(atlas, split_address)
#top_zcs <- head(atlas[, .N, by = list(department_zipcode)][order(-N)], 5)
#top_zc <- as.character(top_zcs[1,department_zipcode])

atlas[is.na(atlas)] <- " "
atlas$row <- row.names(atlas)
atlas <- atlas[, clean_number:=format(phone(department_number, region='US'))]
atlas <- clean_website(atlas, "dealership_website")
# removing duplicates from all_info
atlas <- atlas[, department_state:=as.character(department_state)]
atlas$department_state <- ifelse(atlas$department_country == 'Canada', substr(atlas$department_state, 0, 2), atlas$department_state)
atlas <- atlas[, department_country:=as.character(department_country)]
atlas$department_country <- ifelse(atlas$department_country == 'USA', 'United States', atlas$department_country)
atlas <- atlas[(department_country == 'United States' & department_state %in% state_codes_usa) | (department_country == 'Canada' & department_state %in% state_codes_canada)]
atlas <- atlas[, all_info:=gsub("[[:punct:]]", "", tolower(paste(dealership_name, dealership_legal_name,
  clean_website, department_address_line_1, department_zipcode, department_city, ownership_group_name, ownership_group_legal_name,
  make_name, manufacturer_name, clean_number)))]
atlas <- atlas[, unique_all_info:=rem_dup.vector(all_info)]

atlas <- atlas[department_state == 'TX']
cars <- cars[billingstate == 'TX']
di <- di[billingstate == 'TX']

processedData <- atlas$unique_all_info
processedData <- Corpus(VectorSource(processedData))
processedData <- tm_map(processedData, stripWhitespace)
#Using skip gram (instead of cbow) because there isn't a lot of training data.
wordToVecObj <- createLSA(inputData = processedData, trashDataPath = config$baseDataPath,
    vectors = 100, min_count = 1, window = 20, threads = 2, cbow = 1)

cars[is.na(cars)] <- ""
cars$row <- row.names(cars)
cars <- cars[, clean_number:=format(phone(phone, region='US'))]
cars <- clean_website(cars, "website")
cars <- cars[, all_info:=gsub("[[:punct:]]", "", tolower(paste(name,
  clean_website, billingstreet, billingpostalcode, billingcity, clean_number)))]
cars <- cars[, all_info:=gsub("\\n", " ", all_info)]
cars <- cars[, all_info:=gsub("\\r\\n", " ", all_info)]
cars <- cars[, unique_all_info:=rem_dup.vector(all_info)]
cars <- cars[, dealership_address:=paste(billingstreet, billingcity, billingstate, billingpostalcode, billingcountry, sep = ", ")]

di[is.na(di)] <- " "
di$row <- row.names(di)
di <- di[, clean_number:=format(phone(phone, region='US'))]
di <- clean_website(di, "website")
di <- di[, all_info:=gsub("[[:punct:]]", "", tolower(paste(name,
  clean_website, billingstreet, billingpostalcode, billingcity, clean_number)))]
di <- di[, all_info:=gsub("\\n", " ", all_info)]
di <- di[, all_info:=gsub("\\r\\n", " ", all_info)]
di <- di[, unique_all_info:=rem_dup.vector(all_info)]
di <- di[, dealership_address:=paste(billingstreet, billingcity, billingstate, billingpostalcode, billingcountry, sep = ", ")]

#total_runs <- round(nrow(atlas)/500, 0)
total_runs <- nrow(atlas)
output <- c()
for (j in (1:total_runs)){
  max_score = 0
  statement_atlas <- atlas[j, unique_all_info]
  row_atlas <- atlas[j, row]
  dealership_id_atlas <- atlas[j, dealership_id]
  dealership_name_atlas <- atlas[j, dealership_name]
  department_address_atlas <- atlas[j, department_address]
  for (i in (1:nrow(cars))){
    statement_cars <- cars[i, unique_all_info]
    a <- as.numeric(get_vector_statement(wordToVecObj, statement_atlas))
    b <- as.numeric(get_vector_statement(wordToVecObj, statement_cars))
    if(is.na(b)[1]) next
    l <- list(a, b)
    sim_score <- as.numeric(simil(l, method="cosine"))
    osa <- stringsim(statement_cars, statement_atlas, method = "osa")
    lcs <- stringsim(statement_cars, statement_atlas, method = "lcs")
    qgram <- stringsim(statement_cars, statement_atlas, method = "qgram")
    cosine <- stringsim(statement_cars, statement_atlas, method = "cosine")
    jaccard <- stringsim(statement_cars, statement_atlas, method = "jaccard")
    jw <- stringsim(statement_cars, statement_atlas, method = "jw")
    soundex <- stringsim(statement_cars, statement_atlas, method = "soundex")
    total_score <- (sim_score + osa + lcs + qgram + cosine + jaccard + jw + soundex)/8
    if(total_score > max_score){
      #message(total_score, " Statement:", statement_cars, "\n")
      max_score = total_score
      max_statement_cars = statement_cars
      row_cars = cars[i,row]
      sf_id_cars = cars[i,id]
      dealership_address_cars = cars[i,dealership_address]
      dealership_name_cars = cars[i,name]
    }
  }
  message(max_score, " ATLAS:", statement_atlas, " CARS:", max_statement_cars, "\n")
  tmp <- c(row_atlas, row_cars, max_score, dealership_id_atlas, sf_id_cars, dealership_name_atlas, dealership_name_cars, department_address_atlas, dealership_address_cars)
  output <- rbind(output, tmp)
}
output <- as.data.table(output)
names(output) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars')

#UDF


#0.700197439337271 ATLAS:serra bmw champaign of serrabmwchampaign 100 burwash ave suite b 61874 savoy automotive llc group 12173560303 CARS:honda  bmw of champaign drivechampaign 100 burwash ave 61874 savoy 12173560303
#Good Example
