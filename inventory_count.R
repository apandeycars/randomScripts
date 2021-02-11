#inventory_count.Rlibrary(data.table)
library(readr)
library(data.table)
options(width = 150)
inventory_count <- setDT(read_csv('inventory_count.csv'))

head(inventory_count)

#added
added <- inventory_count[, .N, by = list(min_filedate)][order(min_filedate)]
names(added)[1] <- 'date'
names(added)[2] <- 'vehicles_added'
#removed
removed <- inventory_count[, .N, by = list(max_filedate)][order(max_filedate)]
names(removed)[1] <- 'date'
names(removed)[2] <- 'vehicles_removed'

allStats <- merge(added, removed, by = 'date')
allStats <- allStats[date != '2020-08-13' & date!= '2020-08-18']
write.csv(allStats, file = "allStats.csv", row.names = F)

inventory_count <- setDT(read_csv('inventory_count_vin.csv'))
#added
added <- inventory_count[, .N, by = list(min_filedate)][order(min_filedate)]
names(added)[1] <- 'date'
names(added)[2] <- 'vins_added'
#removed
removed <- inventory_count[, .N, by = list(max_filedate)][order(max_filedate)]
names(removed)[1] <- 'date'
names(removed)[2] <- 'vins_removed'
allStats <- merge(added, removed, by = 'date')
allStats <- allStats[date != '2020-08-13' & date!= '2020-08-18']
write.csv(allStats, file = "allStats_vin.csv", row.names = F)
