# install.packages(c('runner', 'lsa', 'tm', 'proxy', 'dialr', 'stringdist', 'urltools'))
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
library('urltools')
options(width = 150)
'%ni%' <- Negate('%in%')
# setwd("/app/workarea/apandey/datasets/")
setwd("/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/datasets/")
source('/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/udf_fuzzy_match_names.R')

config <- c()
config$baseDataPath <- getwd()

state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')

cars <- setDT(read_csv('cars_account_data.csv'))
names(cars) <- tolower(names(cars))
billing_colnames <- names(cars)[which(names(cars) %like% 'billing')]
selected_cols <- setdiff(names(cars), billing_colnames)
cars <- setDT(cars[, selected_cols, with = F])
names(cars)[which(names(cars) == 'current customer?')] <- 'current_customer'
names(cars)[which(names(cars) == 'franchise_or_independent__c')] <- 'franchise_independent'
req_cols <- c("id", "name", "phone", "website", "shippingcity",
  "shippingcountry", "shippingpostalcode", "shippingstate",
  "shippingstreet", "current_customer", "franchise_independent"
)
cars <- setDT(cars[, req_cols, with = F])
cars <- cars[, shippingcountry:=str_replace_all(as.character(shippingcountry), "[^[:graph:]]", " ")]
cars <- cars[, shippingstate:=str_replace_all(as.character(shippingstate), "[^[:graph:]]", " ")]
cars <- cars[, shippingcity:=str_replace_all(as.character(shippingcity), "[^[:graph:]]", " ")]
cars <- cars[, shippingstreet:=str_replace_all(as.character(shippingstreet), "[^[:graph:]]", " ")]
cars <- cars[, shippingpostalcode:=str_replace_all(as.character(shippingpostalcode), "[^[:graph:]]", " ")]

cars$shippingcountry <- ifelse(cars$shippingcountry %in% c('USA', 'us', 'US', 'United State', 'United', 'united states', 'usa'), 'United States', cars$shippingcountry)
cars$shippingcountry <- ifelse(cars$shippingcountry %in% c('United States', 'Canada'), cars$shippingcountry, 'other')
#cars <- cars[shippingcountry %in% c('United States', 'Canada')]
cars$shippingstate <- ifelse(is.na(state.abb[match(cars$shippingstate,state.name)]), cars$shippingstate, state.abb[match(cars$shippingstate,state.name)])
cars$shippingstate <- ifelse(cars$shippingstate == 'Alberta', 'AB', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'British Columbia', 'BC', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Manitoba', 'MB', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'New Brunswick', 'NB', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Newfoundland and Labrador', 'NL', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Nova Scotia', 'NS', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Ontario', 'ON', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Quebec', 'QC', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingstate == 'Saskatchewan', 'SK', cars$shippingstate)
cars <- cars[, shippingstate:=gsub("[[:punct:]]", "", toupper(shippingstate))]
cars$shippingstate <- ifelse(cars$shippingcountry == "United States" & cars$shippingstate %ni% state_codes_usa, 'other', cars$shippingstate)
cars$shippingstate <- ifelse(cars$shippingcountry == "Canada" & cars$shippingstate %ni% state_codes_canada, 'other', cars$shippingstate)
cars$current_customer <- ifelse(is.na(cars$current_customer), 'No', cars$current_customer)
cars$current_customer <- ifelse(cars$current_customer == 'No', 0, 1)
cars$shippingstate <- ifelse(cars$shippingstate %ni% c(state_codes_usa, state_codes_canada), 'other', cars$shippingstate)

cars[is.na(cars)] <- " "
cars$row <- row.names(cars)
cars <- cars[, clean_number:=format(phone(phone, region='US'))]
cars <- clean_website(cars, "website")
cars <- cars[, name:=str_replace_all(as.character(name), "[^[:graph:]]", " ")]
cars <- cars[, shippingcity:=str_replace_all(as.character(shippingcity), "[^[:graph:]]", " ")]
cars <- cars[, shippingstreet:=str_replace_all(as.character(shippingstreet), "[^[:graph:]]", " ")]
cars <- cars[, shippingpostalcode:=gsub("-.*","",shippingpostalcode)]
cars$shippingpostalcode <- ifelse(nchar(cars$shippingpostalcode) == 4, paste("0", cars$shippingpostalcode, sep = ""), cars$shippingpostalcode)
cars <- cars[, all_info:=gsub("[[:punct:]]", "", tolower(paste(name,
  clean_website, enc2native(as.character(shippingstreet)), shippingpostalcode, shippingcity, clean_number)))]
cars <- cars[, all_info:=gsub("\\n", " ", all_info)]
cars <- cars[, all_info:=gsub("\\r\\n", " ", all_info)]
cars <- cars[, all_info:=gsub(" na", " ", all_info)]
cars <- cars[, unique_all_info:=rem_dup.vector(all_info)]
# Adding website twice and name once to give them more weightage.
cars <- cars[, unique_all_info:=paste(unique_all_info, clean_website, name, clean_website)]
cars <- cars[, dealership_address:=paste(shippingstreet, shippingcity, shippingstate, shippingpostalcode, shippingcountry, sep = ", ")]

cars <- cars[, address:=gsub("[[:punct:]]", "", tolower(paste(enc2native(as.character(shippingstreet)), shippingpostalcode)))]
cars <- cars[, address:=gsub("\\n", " ", address)]
cars <- cars[, address:=gsub("\\r\\n", " ", address)]
cars <- cars[, address:=gsub(" na", " ", address)]

cars$shippingcountry <- ifelse(cars$shippingcountry == 'other' & cars$shippingstate %in% state_codes_usa, 'United States', cars$shippingcountry)
cars$shippingcountry <- ifelse(cars$shippingcountry == 'other' & cars$shippingstate %in% state_codes_canada, 'Canada', cars$shippingcountry)
#Let's work on small dataset first, starting with zipcode 60647.
atlas <- setDT(read_csv('atlas.csv'))
split_address <- as.data.frame(str_match(atlas$department_address,"(.+), (.+), (.+) (.+), (.+)")[ ,-1])
names(split_address) <- c('department_address_line_1', 'department_city', 'department_state', 'department_zipcode', 'department_country')
setDT(split_address)
atlas <- cbind(atlas, split_address)
#top_zcs <- head(atlas[, .N, by = list(department_zipcode)][order(-N)], 5)
#top_zc <- as.character(top_zcs[1,department_zipcode])
# atlas <- atlas[department_country == 'United States']
# di <- di[shippingcountry == 'United States']

atlas[is.na(atlas)] <- " "
atlas$row <- row.names(atlas)
atlas <- atlas[, clean_number:=format(phone(department_number, region='US'))]
atlas <- clean_website(atlas, "dealership_website")
atlas$clean_website <- ifelse(atlas$clean_website == 'dakotachryslercenterutmsourcegoogleutmmediumorganicutmcampaigngooglemybusiness', 'dakotachryslercenter', atlas$clean_website)
atlas$clean_website <- ifelse(atlas$clean_website == 'carolinacarco2004pontiacgrand20prixgreenwoodsc176903251veh', 'carolinacarco', atlas$clean_website)
# removing duplicates from all_info
atlas <- atlas[, department_state:=as.character(department_state)]
atlas$department_state <- ifelse(atlas$department_country == 'Canada', substr(atlas$department_state, 0, 2), atlas$department_state)
atlas <- atlas[, department_zipcode:=gsub("-.*","",department_zipcode)]
atlas$department_zipcode <- ifelse(nchar(atlas$department_zipcode) == 4, paste("0", atlas$department_zipcode, sep = ""), atlas$department_zipcode)
atlas <- atlas[, department_country:=as.character(department_country)]
atlas$department_country <- ifelse(atlas$department_country == 'USA', 'United States', atlas$department_country)
atlas <- atlas[(department_country == 'United States' & department_state %in% state_codes_usa) | (department_country == 'Canada' & department_state %in% state_codes_canada)]
atlas <- atlas[, all_info:=gsub("[[:punct:]]", "", tolower(paste(dealership_name,
  clean_website, department_address_line_1, department_zipcode, department_city, clean_number, dealership_legal_name, ownership_group_name, ownership_group_legal_name,
  make_name, manufacturer_name)))]
atlas <- atlas[, unique_all_info:=rem_dup.vector(all_info)]
atlas <- atlas[, unique_all_info:=paste(unique_all_info, clean_website, dealership_name, clean_website)]

atlas <- atlas[, address:=gsub("[[:punct:]]", "", tolower(paste(enc2native(as.character(department_address_line_1)), department_zipcode)))]
atlas <- atlas[, address:=gsub("\\n", " ", address)]
atlas <- atlas[, address:=gsub("\\r\\n", " ", address)]
atlas <- atlas[, address:=gsub(" na", " ", address)]

di <- setDT(read_csv('di_account_data.csv'))
names(di) <- tolower(names(di))
shipping_colnames <- names(di)[which(names(di) %like% 'shipping')]
selected_cols <- setdiff(names(di), shipping_colnames)
di <- setDT(di[, selected_cols, with = F])
names(di)[which(names(di) == 'current di customer')] <- 'current_customer'
req_cols <- c("id", "name", "phone", "website", "billingcity",
  "billingcountry", "billingpostalcode", "billingstate",
  "billingstreet", "current_customer"
)
di <- setDT(di[, req_cols, with = F])
di <- di[, billingcountry:=str_replace_all(as.character(billingcountry), "[^[:graph:]]", " ")]
di <- di[, billingstate:=str_replace_all(as.character(billingstate), "[^[:graph:]]", " ")]
di <- di[, billingcity:=str_replace_all(as.character(billingcity), "[^[:graph:]]", " ")]
di <- di[, billingstreet:=str_replace_all(as.character(billingstreet), "[^[:graph:]]", " ")]
di <- di[, billingpostalcode:=str_replace_all(as.character(billingpostalcode), "[^[:graph:]]", " ")]

di$billingcountry <- ifelse(di$billingcountry %in% c('USA', 'us', 'US', 'United State', 'United', 'united states', 'usa'), 'United States', di$billingcountry)
di$billingcountry <- ifelse(di$billingcountry %in% c('United States', 'Canada'), di$billingcountry, 'other')
#di <- di[billingcountry %in% c('United States', 'Canada')]
di$billingstate <- ifelse(is.na(state.abb[match(di$billingstate,state.name)]), di$billingstate, state.abb[match(di$billingstate,state.name)])
di$billingstate <- ifelse(di$billingstate == 'Alberta', 'AB', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'British Columbia', 'BC', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Manitoba', 'MB', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'New Brunswick', 'NB', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Newfoundland and Labrador', 'NL', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Nova Scotia', 'NS', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Ontario', 'ON', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Quebec', 'QC', di$billingstate)
di$billingstate <- ifelse(di$billingstate == 'Saskatchewan', 'SK', di$billingstate)
di <- di[, billingstate:=gsub("[[:punct:]]", "", toupper(billingstate))]
di$billingstate <- ifelse(di$billingcountry == "United States" & di$billingstate %ni% state_codes_usa, 'other', di$billingstate)
di$billingstate <- ifelse(di$billingcountry == "Canada" & di$billingstate %ni% state_codes_canada, 'other', di$billingstate)
di$current_customer <- ifelse(is.na(di$current_customer), 'No', di$current_customer)
di$current_customer <- ifelse(di$current_customer == 'No', 0, 1)
di$billingstate <- ifelse(di$billingstate %ni% c(state_codes_usa, state_codes_canada), 'other', di$billingstate)

di[is.na(di)] <- " "
di$row <- row.names(di)
di <- di[, clean_number:=format(phone(phone, region='US'))]
di <- clean_website(di, "website")
di <- di[, name:=str_replace_all(as.character(name), "[^[:graph:]]", " ")]
di <- di[, billingcity:=str_replace_all(as.character(billingcity), "[^[:graph:]]", " ")]
di <- di[, billingstreet:=str_replace_all(as.character(billingstreet), "[^[:graph:]]", " ")]
di <- di[, billingpostalcode:=gsub("-.*","",billingpostalcode)]
di$billingpostalcode <- ifelse(nchar(di$billingpostalcode) == 4, paste("0", di$billingpostalcode, sep = ""), di$billingpostalcode)
di <- di[, all_info:=gsub("[[:punct:]]", "", tolower(paste(name,
  clean_website, enc2native(as.character(billingstreet)), billingpostalcode, billingcity, clean_number)))]
di <- di[, all_info:=gsub("\\n", " ", all_info)]
di <- di[, all_info:=gsub("\\r\\n", " ", all_info)]
di <- di[, all_info:=gsub(" na", " ", all_info)]
di <- di[, unique_all_info:=rem_dup.vector(all_info)]
# Adding website twice and name once to give them more weightage.
di <- di[, unique_all_info:=paste(unique_all_info, clean_website, name, clean_website)]
di <- di[, dealership_address:=paste(billingstreet, billingcity, billingstate, billingpostalcode, billingcountry, sep = ", ")]

di <- di[, address:=gsub("[[:punct:]]", "", tolower(paste(enc2native(as.character(billingstreet)), billingpostalcode)))]
di <- di[, address:=gsub("\\n", " ", address)]
di <- di[, address:=gsub("\\r\\n", " ", address)]
di <- di[, address:=gsub(" na", " ", address)]

di$billingcountry <- ifelse(di$billingcountry == 'other' & di$billingstate %in% state_codes_usa, 'United States', di$billingcountry)
di$billingcountry <- ifelse(di$billingcountry == 'other' & di$billingstate %in% state_codes_canada, 'Canada', di$billingcountry)
di[is.na(di)] <- " "

save(cars, atlas, di, file = "fuzzy_match_input_processed_data.RData")

atlas <- atlas[, clean_website_v1:=clean_website]

atlas <- atlas[, dealership_website:=str_replace_all(as.character(dealership_website), "http:", "https:")]
atlas <- atlas[, dealership_website:=str_replace_all(as.character(dealership_website), "m.facebook.com", "www.facebook.com")]
atlas <- atlas[, dealership_website:=str_replace_all(as.character(dealership_website), "https://facebook.com", "https://www.facebook.com")]

atlas <- atlas[, clean_website:=suffix_extract(domain(dealership_website))$host]
atlas[is.na(atlas)] <- ""
atlas <- atlas[, clean_website:=ifelse(clean_website == 'tbd', '', clean_website)]

no_change_website <- c("www.facebook.com",
               "www.byrider.com", "bit.ly", "www.carhop.com", "goo.gl", "fb.me",
               "www.communitycars.com", "www.hertzcarsales.com", "sites.google.com")

atlas <- atlas[, clean_website:=ifelse(clean_website == 'tbd', '', clean_website)]
atlas <- atlas[, clean_website:=ifelse(clean_website %in% no_change_website, dealership_website, clean_website)]

#atlas_bu <- as.data.frame(atlas)

di <- di[, clean_website_v1:=clean_website]
di <- di[, clean_website:=suffix_extract(domain(website))$host]
di[is.na(di)] <- ""
di <- di[, clean_website:=ifelse(clean_website == 'tbd', '', clean_website)]

di_bu <- as.data.frame(di)

cars <- cars[, clean_website_v1:=clean_website]

cars <- cars[, website:=str_replace_all(as.character(website), "http:", "https:")]
cars <- cars[, website:=str_replace_all(as.character(website), "m.facebook.com", "www.facebook.com")]
cars <- cars[, website:=str_replace_all(as.character(website), "https://facebook.com", "https://www.facebook.com")]

cars <- cars[, clean_website:=suffix_extract(domain(website))$host]
cars[is.na(cars)] <- ""

#cars <- cars[, clean_website:=ifelse(clean_website == 'tbd', '', clean_website)]
bad_sites <- c('oob', 'n', 'tbd', 'none', 'oob.com', 'hss.hsselite.com', 'www.cars.com', 'ad.doubleclick.net',
              'no site', 'http', 'www.cargurus.com', 'https', 'no website', 'bhph', 'none.com', 'gmail.com', ' ',
              'www.oob.com')
cars <- cars[, clean_website:=ifelse(clean_website %in% bad_sites, '', clean_website)]

cars <- cars[, clean_website:=ifelse(clean_website == "drivetime.com", "www.drivetime.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "jdbyrider.com", "www.jdbyrider.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "car-mart.com", "www.car-mart.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "https://www.car-mart.com/", "www.car-mart.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "carmart.com", "www.car-mart.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "carbiz.com", "www.carbiz.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "carvana.com", "www.carvana.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "carmax.com", "www.carmax.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "baxterauto.com", "www.baxterauto.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "gatewayclassiccars.com", "www.gatewayclassiccars.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "carite.com", "www.carite.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "gojdb.com", "www.gojdb.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "shift.com", "www.shift.com", clean_website)]
cars <- cars[, clean_website:=ifelse(clean_website == "autotrader.com", "www.autotrader.com", clean_website)]

no_change_website <- c("www.facebook.com",
               "www.byrider.com", "bit.ly", "www.carhop.com", "goo.gl", "fb.me",
               "www.communitycars.com", "www.hertzcarsales.com", "sites.google.com")

cars <- cars[, clean_website:=ifelse(clean_website %in% no_change_website, website, clean_website)]


save(cars, atlas, di, file = "fuzzy_match_input_processed_data.RData")
