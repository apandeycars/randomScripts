library(RCurl)
library(RJSONIO)
library(plyr)

inputData <- read.csv("updated_records.csv")
inputData$origins <- paste(inputData$Rep.Latitude, ',', inputData$Rep.Longitude, sep = "")
inputData$destinations <- paste(inputData$Account.Latitude, ',', inputData$Account.Longitude, sep = "")

api_key = "AIzaSyC8YzyKXqSlSfq7hPuH47LQ2914zbHF6PI"

buildURL <- function(origins, destinations, apiKey = NULL) {
  root <- "https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins="
  u <- paste(root, origins, "&destinations=", destinations, "&key=", api_key, sep = "")
  return(URLencode(u))
}

nLoops <- nrow(inputData)
#nLoops <- 3
newData <- c()
startingValue <- 1
for (i in startingValue:nLoops) {
  if((i%%10) == 0) {
    print(i)
    Sys.sleep(0.5)
  }
  tempURL <- buildURL (origins = inputData$origins[i], destinations = inputData$destinations[i], apiKey = api_key)
  tempDoc <- getURL(tempURL)
  tempData <- fromJSON(tempDoc, simplify = FALSE)
  if(tempData$status=="OK" & tempData$rows[[1]]$elements[[1]]$status != "ZERO_RESULTS") {
    distance_in_miles <- tempData$rows[[1]]$elements[[1]]$distance$text
    distance_in_meter <- tempData$rows[[1]]$elements[[1]]$distance$value
    duration_in_hour <- tempData$rows[[1]]$elements[[1]]$duration$text
    duration_in_seconds <- tempData$rows[[1]]$elements[[1]]$duration$value
    newData <- rbind(newData, cbind(inputData[i,], distance_in_miles, distance_in_meter, duration_in_hour, duration_in_seconds))
    newData <- as.data.frame(newData)
  } else {
    next
  }
}
#newDataFirstRun
write.csv(newData, file = "newData.csv", row.names = F)
