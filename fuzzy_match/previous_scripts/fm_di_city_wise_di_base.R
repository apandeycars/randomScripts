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
options(warn=-1)


# setwd("/app/workarea/apandey/datasets/")
setwd("/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/fm_di_base/")
source('/Users/addhyanpandey/Desktop/randomScripts/fuzzy_match/udf_fuzzy_match_names.R')

#state_codes_usa <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
remaining <- c('AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
running <- c('AZ','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
done <- c()
config <- c()
config$baseDataPath <- getwd()
state_codes_usa <- c('NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD')
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

  if(nrow(di) == 0) next ;

  if (nrow(atlas) == 0){
    for (j in (1:nrow(di))){
      row_atlas <- ''
      row_di <- di[j, row]
      max_score <- 0
      dealership_id_atlas <- ''
      sf_id_di <- di[j, id]
      dealership_name_atlas <- ''
      dealership_name_di <- as.character(di[j, name])
      department_address_atlas <- ''
      dealership_address_di <- as.character(di[j, billingaddress])
      tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
      output <- rbind(output, tmp)
    }
    output_di_usa_post_fl <- as.data.frame(output)
    names(output_di_usa_post_fl) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
    setDT(output_di_usa_post_fl)
    save(output_di_usa_post_fl, file = "output_di_usa_post_fl.RData")
    next
  }

  di <- di[, normalised_city:=tolower(billingcity)]
  top_cities <- di[, .N, by = list(normalised_city)][order(-N)][N>=20]$normalised_city
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
      for (j in (1:nrow(di))){
        row_atlas <- ''
        row_di <- di[j, row]
        max_score <- 0
        dealership_id_atlas <- ''
        sf_id_di <- di[j, id]
        dealership_name_atlas <- ''
        dealership_name_di <- as.character(di[j, name])
        department_address_atlas <- ''
        dealership_address_di <- as.character(di[j, billingaddress])
        tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
        output <- rbind(output, tmp)
      }
      #message("\n", "\n", "\n", "Hello I am here", "\n", "\n", "\n")
      next
    }
    message("Rows in Atlas: ", nrow(atlas), " Rows in di: ", nrow(di), "\n")

#     processedData <- di$unique_all_info
#     processedData <- Corpus(VectorSource(processedData))
#     processedData <- tm_map(processedData, stripWhitespace)
# #Using skip gram (instead of cbow) because there isn't a lot of training data.
#     wordToVecObj <- createLSA(inputData = processedData, trashDataPath = config$baseDataPath,
#       vectors = 100, min_count = 1, window = 20, threads = 2, cbow = 1)

    for (j in (1:nrow(di))){
      max_score = 0
      statement_di <- as.character(di[j, unique_all_info])
      row_di <- as.character(di[j,row])
      sf_id_di = as.character(di[j,id])
      dealership_name_di <- as.character(di[j, name])
      dealership_address_di = as.character(di[j,dealership_address])
      for (i in (1:nrow(atlas))){
        statement_atlas <- as.character(atlas[i, unique_all_info])
        # a <- as.numeric(get_vector_statement(wordToVecObj, statement_atlas))
        # b <- as.numeric(get_vector_statement(wordToVecObj, statement_di))
        # if(is.na(b)[1] | is.na(a)[1]) {
        #   sim_score <- 0
        # } else {
        #   l <- list(a, b)
        #   sim_score <- as.numeric(simil(l, method="cosine"))
        # }
        osa <- stringsim(statement_di, statement_atlas, method = "osa")
        lcs <- stringsim(statement_di, statement_atlas, method = "lcs")
        qgram <- stringsim(statement_di, statement_atlas, method = "qgram")
        cosine <- stringsim(statement_di, statement_atlas, method = "cosine")
        jaccard <- stringsim(statement_di, statement_atlas, method = "jaccard")
        jw <- stringsim(statement_di, statement_atlas, method = "jw")
        soundex <- stringsim(statement_di, statement_atlas, method = "soundex")
#        total_score <- (sim_score + osa + lcs + qgram + cosine + jaccard + jw + soundex)/8
        total_score <- (osa + lcs + qgram + cosine + jaccard + jw + soundex)/7
        if(total_score > max_score){
          max_score = total_score
          max_statement_atlas = statement_atlas
          row_atlas = atlas[i,row]
          dealership_id_atlas <- atlas[i, dealership_id]
          department_address_atlas <- as.character(atlas[i, department_address])
          dealership_name_atlas = as.character(atlas[i,dealership_name])
        }
      }
    #message(max_score, " ATLAS:", statement_atlas, " di:", max_statement_di, "\n")
      tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
      output <- rbind(output, tmp)
      if ((nrow(output) %% 20) == 0) message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_usa),]), 0), "%", "\n")
    }
    city_end_time <- Sys.time()
    print(city_end_time - initial_time)
    message("So Far Completed: ", nrow(output), " Rows or ", round(100.00*nrow(output)/nrow(atlas_bu[which(atlas_bu$department_state %in% state_codes_usa),]), 0), "%", "\n")
    output_di_usa_post_fl <- as.data.frame(output)
    names(output_di_usa_post_fl) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
    setDT(output_di_usa_post_fl)
    save(output_di_usa_post_fl, file = "output_di_usa_post_fl.RData")
  }
}

#output <- as.data.table(output)
#names(output) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
#output_di_usa <- output
#save(output_di_usa, file = "output_di_usa.RData")

#http://172.28.195.169:8888/?token=dde71802386eb16d6a78043c6a1d60c694943ccc7dcb3a91
## Canada

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

# processedData <- atlas$unique_all_info
# processedData <- Corpus(VectorSource(processedData))
# processedData <- tm_map(processedData, stripWhitespace)
# #Using skip gram (instead of cbow) because there isn't a lot of training data.
# wordToVecObj <- createLSA(inputData = processedData, trashDataPath = config$baseDataPath,
#     vectors = 100, min_count = 1, window = 20, threads = 2, cbow = 1)
output_canada <- c()
state_codes_canada <- c('NL','PE','NS','NB','QC','ON','MB','SK','AB','BC','YT','NT','NU')
for (state_code in state_codes_canada) {

  message("Running For: ", state_code, "\n")
  atlas <- as.data.table(atlas_bu[which(atlas_bu$department_state == state_code & atlas_bu$department_country == "Canada"),])
  di <- as.data.table(di_bu[which(di_bu$billingstate == state_code & di_bu$billingcountry == "Canada"),])
  message("Rows in Atlas: ", nrow(atlas), " Rows in di: ", nrow(di), "\n")

  if(nrow(di) == 0) next ;

  for (j in (1:nrow(di))){
    max_score = 0
    statement_di <- as.character(di[j, unique_all_info])
    row_di <- as.character(di[j,row])
    sf_id_di = as.character(di[j,id])
    dealership_name_di <- as.character(di[j, name])
    dealership_address_di = as.character(di[j,dealership_address])

    for (i in (1:nrow(atlas))){
        statement_atlas <- as.character(atlas[i, unique_all_info])
        # a <- as.numeric(get_vector_statement(wordToVecObj, statement_atlas))
        # b <- as.numeric(get_vector_statement(wordToVecObj, statement_di))
        # if(is.na(b)[1] | is.na(a)[1]) {
        #   sim_score <- 0
        # } else {
        #   l <- list(a, b)
        #   sim_score <- as.numeric(simil(l, method="cosine"))
        # }
        osa <- stringsim(statement_di, statement_atlas, method = "osa")
        lcs <- stringsim(statement_di, statement_atlas, method = "lcs")
        qgram <- stringsim(statement_di, statement_atlas, method = "qgram")
        cosine <- stringsim(statement_di, statement_atlas, method = "cosine")
        jaccard <- stringsim(statement_di, statement_atlas, method = "jaccard")
        jw <- stringsim(statement_di, statement_atlas, method = "jw")
        soundex <- stringsim(statement_di, statement_atlas, method = "soundex")
#        total_score <- (sim_score + osa + lcs + qgram + cosine + jaccard + jw + soundex)/8
        total_score <- (osa + lcs + qgram + cosine + jaccard + jw + soundex)/7
        if(total_score > max_score){
          max_score = total_score
          max_statement_atlas = statement_atlas
          row_atlas = atlas[i,row]
          dealership_id_atlas <- atlas[i, dealership_id]
          department_address_atlas <- as.character(atlas[i, department_address])
          dealership_name_atlas = as.character(atlas[i,dealership_name])
        }
    }
    #message(max_score, " ATLAS:", statement_atlas, " di:", max_statement_di, "\n")
    tmp <- c(row_atlas, row_di, max_score, dealership_id_atlas, sf_id_di, dealership_name_atlas, dealership_name_di, department_address_atlas, dealership_address_di)
    tmp <- as.data.table(t(tmp))
    names(tmp) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
    output_canada <- rbind(output_canada, tmp)
    if ((nrow(output_canada) %% 20) == 0) message("So Far Completed: ", nrow(output_canada), " Rows or ", round(100.00*nrow(output_canada)/nrow(di_bu[which(di_bu$billingstate %in% state_codes_canada),]), 0), "%", "\n")
  }
  output_canada <- as.data.table(output_canada)
  names(output_canada) <- c('row_atlas', 'row_di', 'max_score', 'dealership_id_atlas', 'sf_id_di', 'dealership_name_atlas', 'dealership_name_di', 'department_address_atlas', 'dealership_address_di')
  output_di_canada <- output_canada
  save(output_di_canada, file = "output_di_canada.RData")
}
