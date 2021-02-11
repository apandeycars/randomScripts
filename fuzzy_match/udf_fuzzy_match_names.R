get_vector_statement <- function(lsaData = NULL, statement = NULL, ...){
  stop_en <- append(stopwords_en, '')
  tokens <- setdiff(unlist(strsplit(statement, ' ')),stop_en)
  vec <- rep(0, ncol(lsaData))
  vec <- lsaData[[tokens]]
  vec <- vec/length(tokens)
}

get_score <- function (base_statement, atlas_statement) {
  osa <- stringsim(base_statement, atlas_statement, method = "osa")
  lcs <- stringsim(base_statement, atlas_statement, method = "lcs")
  qgram <- stringsim(base_statement, atlas_statement, method = "qgram")
  cosine <- stringsim(base_statement, atlas_statement, method = "cosine")
  jaccard <- stringsim(base_statement, atlas_statement, method = "jaccard")
  jw <- stringsim(base_statement, atlas_statement, method = "jw")
  soundex <- stringsim(base_statement, atlas_statement, method = "soundex")
  total_score <- (osa + lcs + qgram + cosine + jaccard + jw + soundex)/7
}
#get_score(base_statement = statement_cars, atlas_statement = statement_atlas)

case_of_zero_atlas_record <- function (base_data = NULL, atlas_data = NULL, ...){
  output <- c()
  '%ni%' <- Negate('%in%')
  for (j in (1:nrow(base_data))){
    # conduct website check
    tmp_clean_website <- base_data[j, clean_website]
    tmp_atlas_data <- atlas_data[clean_website %ni% c('', ' ') & clean_website == tmp_clean_website]
    #message("Cars website: ", tmp_clean_website, " Atlas: ", tmp_atlas_data[1, clean_website])
    row_base_data <- base_data[j, row]
    sf_id_base_data <- base_data[j, id]
    dealership_name_base_data <- as.character(base_data[j, name])
    dealership_address_base_data <- as.character(base_data[j, dealership_address])
    if(nrow(tmp_atlas_data)==0) {
      row_atlas <- ''
      notes <- ''
      max_score <- 0
      dealership_id_atlas <- ''
      dealership_name_atlas <- ''
      department_address_atlas <- ''; clean_website_atlas <- ''
    } else {
      row_atlas <- tmp_atlas_data[1,row]
      notes <- 'website_match'
      statement_atlas_data <- tmp_atlas_data[1, unique_all_info]
      statement_base_data <- base_data[j, unique_all_info]
      max_score <- get_score(base_statement = statement_base_data, atlas_statement = statement_atlas_data)
      dealership_id_atlas <- tmp_atlas_data[1,dealership_id]
      dealership_name_atlas <- tmp_atlas_data[1,dealership_name]
      department_address_atlas <- tmp_atlas_data[1,department_address]; clean_website_atlas <- tmp_atlas_data[1,clean_website]
    }
    tmp <- c(row_atlas, row_base_data, max_score, dealership_id_atlas, sf_id_base_data, dealership_name_atlas, dealership_name_base_data, department_address_atlas, dealership_address_base_data, clean_website_atlas, tmp_clean_website, notes)
    output <- rbind(output, tmp)
  }
  #output <- as.data.frame(output)
  #names(output) <- c('row_atlas', 'row_cars', 'max_score', 'dealership_id_atlas', 'sf_id_cars', 'dealership_name_atlas', 'dealership_name_cars', 'department_address_atlas', 'dealership_address_cars', 'clean_website_atlas', 'clean_website_cars', 'notes')
  #return(setDT(output))
  return(output)
}

add_empty_match <- function(base_data = NULL){
  row_atlas <- ''
  notes <- ''
  max_score <- 0
  dealership_id_atlas <- ''
  dealership_name_atlas <- ''
  department_address_atlas <- ''
  clean_website_atlas <- ''
  tmp_clean_website <- base_data[, clean_website]
  row_base_data <- base_data[, row]
  sf_id_base_data <- base_data[, id]
  dealership_name_base_data <- as.character(base_data[, name])
  dealership_address_base_data <- as.character(base_data[, dealership_address])
  output <- c(row_atlas, row_base_data, max_score, dealership_id_atlas, sf_id_base_data, dealership_name_atlas, dealership_name_base_data, department_address_atlas, dealership_address_base_data, clean_website_atlas, tmp_clean_website, notes)
  return(output)
}

createLSA <- function(inputData = NULL, trashDataPath = c(), vectors = 50, min_count = 2,
  window = 3, threads = 4, cbow = FALSE, ...) {
  library("wordVectors")
  if (is.null(inputData)) {
    stop("Please provide the inputData")
  }
  dfFromCorpus <- data.table(get("content", inputData))
  write.csv(dfFromCorpus, paste(trashDataPath, "/tempfile.txt", sep = ""), row.names = F,
    quote = F)
  model = train_word2vec(paste(trashDataPath, "/tempfile.txt", sep = ""), vectors = vectors,
    min_count = min_count, force = TRUE, window = window, threads = threads, cbow = cbow)
  system(paste("rm -rf ", trashDataPath, "/tempfile.txt", sep = ""))
  return(model)
}

clean_website <- function(input_data, website_column_name = "website", ...){
  library(data.table)
  input_data <- setDT(input_data)
  input_data <- input_data[, clean_website:=str_replace_all(get(website_column_name), "facebook.com", "")]
  input_data <- input_data[, clean_website:=gsub(".com.*","",clean_website)]
  input_data <- input_data[, clean_website:=gsub(".net.*","",clean_website)]
  input_data <- input_data[, clean_website:=gsub(".html.*","",clean_website)]
  input_data <- input_data[, clean_website:=gsub(".io.*","",clean_website)]
  input_data <- input_data[, clean_website:=gsub(".ca.*","",clean_website)]
  input_data <- input_data[, clean_website:=str_replace_all(gsub("[[:punct:]]", " ", clean_website), "https", "")]
  input_data <- input_data[, clean_website:=tolower(clean_website)]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, "http ", "")]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, "www ", "")]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, " aspx", "")]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, " htm", "")]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, " ", "")]
  input_data <- input_data[, clean_website:=str_replace_all(clean_website, " ", "")]
  return(input_data)
}
#source: https://stackoverflow.com/questions/20283624/removing-duplicate-words-in-a-string-in-r
rem_dup.one <- function(x){
  paste(unique(tolower(trimws(unlist(strsplit(x,split="(?!')[ [:punct:]]",fixed=F,perl=T))))),collapse = " ")
}
rem_dup.vector <- Vectorize(rem_dup.one,USE.NAMES = F)
