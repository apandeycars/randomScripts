
library(readr)
library(curl)
library(data.table)
library(dplyr)
library(jsonlite)
allLeads <- read_csv('chat_leads.csv')
#fullData <- setDT(read_csv("fullData.csv"))
#fullData <- unique(fullData)
setDT(allLeads)
fullData <- c()
noResultsLeads <- c()

Sys.time()
allData <- c()
for (i in 150001:176368) {
  print(i)
  preURL <- "https://api.cars.com/dealer-inspire-service/cars/conversation//"
  postURL <- "?apikey=SWW1wfWvi7yRdw1GAGCDWEnVZ7K2flAz"
  chat_transcript_id <- allLeads[i,chat_transcript_id]
  url <- paste(preURL, chat_transcript_id, postURL, sep = "")
  chat_transcript <- curl_fetch_memory(url)
  if(length(grep("Server Error", chat_transcript)) | chat_transcript$status_code == 500 | chat_transcript$status_code == 404) {
    message("No result for ", allLeads[i, chat_transcript_id], "\n")
    noResultsLeads <- c(noResultsLeads, chat_transcript_id)
    next;
  }
  chat_transcript <- jsonlite::prettify(rawToChar(chat_transcript$content))
  chat_transcript <- stream_in(textConnection(gsub("\\n", "", chat_transcript)))
  chat_transcript <- as.data.frame(chat_transcript$transcript)
  chat_transcript$attachments <- NULL
  chat_transcript$readReceipts <- NULL
  chat_transcript$chat_transcript_id <- chat_transcript_id
  chat_transcript$lead_id <- allLeads[i, lead_id]
  chat_transcript$submitted_date_time <- allLeads[i, submitted_date_time]
  chat_transcript$date_id <- allLeads[i, date_id]
  chat_transcript$src_pdid <- allLeads[i, src_pdid]
  allData <- rbind(chat_transcript, allData)
  if (i%%50 == 0) Sys.sleep(3)
}

dim(allData)
fullData <- unique(rbind(fullData, allData))
write.csv(fullData, file = "conversationData.csv", row.names = F)
#write.csv(allData, file = "allData.csv", row.names = F)
