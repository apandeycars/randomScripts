#Load Libraries
library(readxl)
library(data.table)
library(dplyr)

# This script combine files into one big file.
# renaming files to the date name
initialNames <- list.files()
for (i in initialNames) {
  if (length(grep("raw", tolower(i))) != 0) {
    to <- as.Date(substr(i, 1, gregexpr(pattern ='raw',tolower(i))[[1]][1]-2), tryFormats="%m.%d.%y")
    to <- paste(to, ".xlsx", sep = "")
    message("Renaming: ", i, " to: ", to)
    #file.rename(from = i, to = to)
  }
}

#loading one at a time, create column to suggest date
newNames <- list.files()
fullData <- c()
filesToExclude <- c('2019-03-15.xlsx', '2019-03-15.csv')
#newNames <- setdiff(newNames, filesToExclude)
for (i in newNames){
  tmpData <- setDT(read_xlsx(i))
  if (length(names(tmpData)) < 25) tmpData <- setDT(read_xlsx(i, sheet = 'Account List'))
  #message(i, " ", length(names(tmpData)))

# Names alterations
  names(tmpData)[which(names(tmpData) == "Seat Number")] <- 'seat_number'
  names(tmpData)[which(names(tmpData) == "Provision Channel")] <- 'provision_channel'
  names(tmpData)[which(names(tmpData) == "R/ASM")] <- 'r_asm'
  names(tmpData)[which(names(tmpData) == "Director")] <- 'director'
  names(tmpData)[which(names(tmpData) == "Account Owner")] <- 'account_owner'
  names(tmpData)[which(names(tmpData) == "Account Name")] <- 'account_name'
  names(tmpData)[which(names(tmpData) == "Account ID")] <- 'sf_account_id'
  names(tmpData)[which(names(tmpData) == "Parent Account")] <- 'parent_account'
  names(tmpData)[which(names(tmpData) == "Major Accounts Group")] <- 'major_account_group'
  names(tmpData)[which(names(tmpData) == "Customer ID")] <- 'customer_id'
  names(tmpData)[which(names(tmpData) == "Account Record Type")] <- 'account_record_type'
  names(tmpData)[which(names(tmpData) == "Type")] <- 'account_type'
  names(tmpData)[which(names(tmpData) == "Franchise or Independent")] <- 'account_name'
  names(tmpData)[which(names(tmpData) == "Shipping Address Line 1")] <- 'shipping_addr_1'
  names(tmpData)[which(names(tmpData) == "Shipping City")] <- 'shipping_city'
  names(tmpData)[which(names(tmpData) == "Shipping State/Province")] <- 'shipping_state_code'
  names(tmpData)[which(names(tmpData) == "Shipping Zip/Postal Code")] <- 'shipping_postal_code'
  names(tmpData)[which(names(tmpData) == "Market Territory")] <- 'market_territory'
  names(tmpData)[which(names(tmpData) == "Ad Package")] <- 'ad_package'
  names(tmpData)[which(names(tmpData) == "Total Monthly Rate")] <- 'total_monthly_rate'
  names(tmpData)[which(names(tmpData) == "Cancellations Pending")] <- 'cancellations_pending'
  names(tmpData)[which(names(tmpData) == "AutoTrader Status")] <- 'autotrader_status'

  namesRequired <- c("seat_number", "provision_channel", "r_asm", "director",
    "account_owner", "account_name", "sf_account_id", "parent_account",
    "major_account_group", "customer_id", "account_record_type", "account_type",
    "shipping_addr_1", "shipping_city", "shipping_state_code", "shipping_postal_code",
    "market_territory", "ad_package", "total_monthly_rate", "cancellations_pending",
    "autotrader_status", "date_id"
  )
  tmpData <- tmpData[, date_id:=substr(i, 1, 10)]
  namesToAdd <- setdiff(namesRequired, names(tmpData))
  for (j in namesToAdd) {
    tmpData <- tmpData[, paste0(j):="NULL"]
  }
  tmpData <- tmpData[, namesRequired, with = FALSE]
  tmpData <- tmpData %>% mutate_all(as.character)
  message(i, " ", length(names(tmpData)))
  fullData <- rbind(tmpData, fullData)
  print(names(tmpData))
}
