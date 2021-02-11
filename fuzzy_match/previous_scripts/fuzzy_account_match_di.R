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
source('/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/udf_fuzzy_match_names.R')

config <- c()
config$baseDataPath <- getwd()
#state_codes_usa <- c('OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
#state_codes_usa <- c('PA','RI','SC','SD','TN')
#state_codes_usa <- c('TX','UT','VT','VA','WA','WV','WI','WY')
state_codes_usa <- c('UT','VT','VA','WA','WV','WI','WY')
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')
remaining <- c('OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
running <- c('OH','OK','OR')
load("fuzzy_match_input_processed_data.RData")
atlas_bu <- as.data.frame(atlas)
di_bu <- as.data.frame(di)

# atlas <- atlas[department_state == 'AK']
# di <- di[billingstate == 'AK']

## USA
output <- c()
atlas <- as.data.table(atlas_bu)
di <- as.data.table(di_bu)
atlas <- atlas[department_country == "United States"]
di <- di[billingcountry == "United States"]

for (state_code in setdiff(state_codes_usa, c('DC'))) {
  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code),])
  di <- as.data.table(di_bu[which(di$billingstate == state_code),])

  processedData <- atlas$unique_all_info
  processedData <- Corpus(VectorSource(processedData))
  processedData <- tm_map(processedData, stripWhitespace)
#Using skip gram (instead of cbow) because there isn't a lot of training data.
  wordToVecObj <- createLSA(inputData = processedData, trashDataPath = config$baseDataPath,
    vectors = 100, min_count = 1, window = 20, threads = 2, cbow = 1)

  for (j in (1:nrow(atlas))){
    max_score = 0
    statement_atlas <- as.character(atlas[j, unique_all_info])
    row_atlas <- atlas[j, row]
    dealership_id_atlas <- atlas[j, dealership_id]
    dealership_name_atlas <- as.character(atlas[j, dealership_name])
    department_address_atlas <- as.character(atlas[j, department_address])
    for (i in (1:nrow(di))){
      statement_di <- as.character(di[i, unique_all_info])
      a <- as.numeric(get_vector_statement(wordToVecObj, statement_atlas))
      b <- as.numeric(get_vector_statement(wordToVecObj, statement_di))
      if(is.na(b)[1]) next
      l <- list(a, b)
      sim_score <- as.numeric(simil(l, method="cosine"))
      osa <- stringsim(statement_di, statement_atlas, method = "osa")
      lcs <- stringsim(statement_di, statement_atlas, method = "lcs")
      qgram <- stringsim(statement_di, statement_atlas, method = "qgram")
      cosine <- stringsim(statement_di, statement_atlas, method = "cosine")
      jaccard <- stringsim(statement_di, statement_atlas, method = "jaccard")
      jw <- stringsim(statement_di, statement_atlas, method = "jw")
      soundex <- stringsim(statement_di, statement_atlas, method = "soundex")
      total_score <- (sim_score + osa + lcs + qgram + cosine + jaccard + jw + soundex)/8
      if(total_score > max_score){
    #message(total_score, " Statement:", statement_di, "\n")
        max_score = total_score
        max_statement_di = statement_di
        row_di = di[i,row]
        sf_id_di = di[i,id]
        dealership_address_di = di[i,dealership_address]
        dealership_name_di = di[i,name]
      }
    }
    #message(max_score, " ATLAS:", statement_atlas, " di:", max_statement_di, "\n")
    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
    output <- rbind(output, tmp)
    if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(atlas_bu[which(atlas_bu$department_state %in% state_codes_usa),]), 0), "%", "\n")
  }
  message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(atlas_bu), 0), "%", "\n")
  output_di_usa_post_ut <- as.data.frame(output)
  names(output_di_usa_post_ut) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
  setDT(output_di_usa_post_ut)
  save(output_di_usa_post_ut, file = "output_di_usa_post_ut.RData")
}

#http://172.28.195.169:8888/?token=dde71802386eb16d6a78043c6a1d60c694943ccc7dcb3a91
## Canada
output_canada <- c()

atlas <- atlas[department_country == "Canada"]
atlas <- atlas[, all_info:=gsub("[[:punct:]]", "", tolower(paste(dealership_name, dealership_legal_name,
  clean_website, department_address_line_1, department_zipcode, department_city, department_state, ownership_group_name, ownership_group_legal_name,
  make_name, manufacturer_name, clean_number)))]
atlas <- atlas[, unique_all_info:=rem_dup.vector(all_info)]

di <- di[billingcountry == "Canada"]
di <- di[, all_info:=gsub("[[:punct:]]", "", tolower(paste(name,
  clean_website, billingstreet, billingpostalcode, billingcity, billingstate, clean_number)))]
di <- di[, all_info:=gsub("\\n", " ", all_info)]
di <- di[, all_info:=gsub("\\r\\n", " ", all_info)]
di <- di[, unique_all_info:=rem_dup.vector(all_info)]

processedData <- atlas$unique_all_info
processedData <- Corpus(VectorSource(processedData))
processedData <- tm_map(processedData, stripWhitespace)
#Using skip gram (instead of cbow) because there isn't a lot of training data.
wordToVecObj <- createLSA(inputData = processedData, trashDataPath = config$baseDataPath,
    vectors = 100, min_count = 1, window = 20, threads = 2, cbow = 1)

  for (j in (1:nrow(atlas))){
    max_score = 0
    statement_atlas <- atlas[j, unique_all_info]
    row_atlas <- atlas[j, row]
    dealership_id_atlas <- atlas[j, dealership_id]
    dealership_name_atlas <- atlas[j, dealership_name]
    department_address_atlas <- atlas[j, department_address]
    for (i in (1:nrow(di))){
      statement_di <- di[i, unique_all_info]
      a <- as.numeric(get_vector_statement(wordToVecObj, statement_atlas))
      b <- as.numeric(get_vector_statement(wordToVecObj, statement_di))
      if(is.na(b)[1]) next
      l <- list(a, b)
      sim_score <- as.numeric(simil(l, method="cosine"))
      osa <- stringsim(statement_di, statement_atlas, method = "osa")
      lcs <- stringsim(statement_di, statement_atlas, method = "lcs")
      qgram <- stringsim(statement_di, statement_atlas, method = "qgram")
      cosine <- stringsim(statement_di, statement_atlas, method = "cosine")
      jaccard <- stringsim(statement_di, statement_atlas, method = "jaccard")
      jw <- stringsim(statement_di, statement_atlas, method = "jw")
      soundex <- stringsim(statement_di, statement_atlas, method = "soundex")
      total_score <- (sim_score + osa + lcs + qgram + cosine + jaccard + jw + soundex)/8
      if(total_score > max_score){
    #message(total_score, " Statement:", statement_di, "\n")
        max_score = total_score
        max_statement_di = statement_di
        row_di = di[i,row]
        sf_id_di = di[i,id]
        dealership_address_di = di[i,dealership_address]
        dealership_name_di = di[i,name]
      }
    }
    #message(max_score, " ATLAS:", statement_atlas, " di:", max_statement_di, "\n")
    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
    output_canada <- rbind(output_canada, tmp)
    output_canada <- as.data.table(output_canada)
    names(output_canada) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
    save(output_canada, file = "output_di_canada.RData")
    if ((nrow(output_canada) %% 20) == 0) message("So Far Completed: ", nrow(output_canada), " Rows or ", round(100.00*nrow(output_canada)/nrow(atlas_bu[which(atlas_bu$department_country == "Canada"),]), 0), "%", "\n")
  }
