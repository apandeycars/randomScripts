#priceAgeDist.R

# select a.price_range_max, a.price_range_min from (
# select
#   sar.src_search_id,
#   min(cast(price_range_max as float)) price_range_max,
#   min(cast(price_range_min as float)) price_range_min
# from insight.search_activity_raw sar
# where sar.filedate = '2020-08-01'
# and sar.price_range_max is not null
# and sar.price_range_min is not null
# and cast(price_range_min as float) > 0
# and sar.source_type <> 'Paid'
# group by sar.src_search_id
#     ) a ;

library(data.table)
library(plotly)
library(readr)
options(width = 150)

supplyData <- setDT(read_csv("inputData.csv"))

supplyData$age_car <- ifelse(supplyData$age_car <= 0, 0, supplyData$age_car)
supplyData$age_car <- ifelse(supplyData$age_car >= 20, 20, supplyData$age_car)
supplyData <- supplyData[price >= 100 & price <= 1000000]
supplyData$price <- ifelse(supplyData$price >= 100000, 100000, supplyData$price)
hist(supplyData[, price], xlab = "Price (USD)", main = "Histogram of Price [All Used Vehicles]")
hist(supplyData[,age_car], xlab = "Age [2020 - Model Year]", main = "Distribution of the Vehicle Age")

# Gathering Demand Data
demandData <- setDT(read_csv("price_range_data.csv"))
demandData$avg_price <- (demandData$price_range_min + demandData$price_range_max)/2
demandData <- demandData[avg_price >= 100 & avg_price <= 1000000]
demandData$avg_price <- ifelse(demandData$avg_price >= 100000, 100000, demandData$avg_price)
demandData$price_range_max <- ifelse(demandData$price_range_max >= 100000, 100000, demandData$price_range_max)
demandData$price_range_min <- ifelse(demandData$price_range_min >= 100000, 100000, demandData$price_range_min)
hist(demandData[, avg_price], xlab = "Avg Price (USD)", main = "Histogram of Price [All Searches with Max & Min Filled Out] \n Organic Traffic Only")
hist(demandData[, price_range_max], xlab = "Avg Price (USD)", main = "Histogram of Max Price [All Searches with Max & Min Filled Out] \n Organic Traffic Only")
plot(density(supplyData[, price]))
lines(density(demandData[, avg_price]))

qdemandData <- setDT(read_csv("demandData.csv"))
qdemandData <- qdemandData[avg_price_viewed >= 100 & avg_price_viewed <= 1000000]
qdemandData$avg_price_viewed <- ifelse(qdemandData$avg_price_viewed >= 100000, 100000, qdemandData$avg_price_viewed)
qdemandData$avg_price_clicked <- ifelse(qdemandData$avg_price_clicked >= 100000, 100000, qdemandData$avg_price_clicked)
qdemandData$avg_price_connected <- ifelse(qdemandData$avg_price_connected >= 100000, 100000, qdemandData$avg_price_connected)
hist(qdemandData[, avg_price_viewed], xlab = "Avg Price (USD)", main = "Histogram of Price of Vehicles Viewed \n Organic Traffic Only")
hist(qdemandData[, avg_price_clicked], xlab = "Avg Price (USD)", main = "Histogram of Price of Vehicles Viewed (VDP) \n Organic Traffic Only")
hist(qdemandData[, avg_price_connected], xlab = "Avg Price (USD)", main = "Histogram of Price of Vehicles (Lead Submission) \n Organic Traffic Only")



supplyData$age_group <- NULL
supplyData$age_group <- "20Y+"
supplyData$age_group <- ifelse(supplyData$age_car <= 1, "0Y-1Y", supplyData$age_group)
supplyData$age_group <- ifelse(supplyData$age_car > 1 & supplyData$age_car <= 3, "2Y-3Y", supplyData$age_group)
supplyData$age_group <- ifelse(supplyData$age_car > 3 & supplyData$age_car <= 5, "4Y-5Y", supplyData$age_group)
supplyData$age_group <- ifelse(supplyData$age_car > 5 & supplyData$age_car <= 10, "6Y-10Y", supplyData$age_group)
supplyData$age_group <- ifelse(supplyData$age_car > 10 & supplyData$age_car <= 19, "11Y-19Y", supplyData$age_group)
supplyData$age_group <- ordered(supplyData$age_group, levels = c("0Y-1Y", "2Y-3Y", "4Y-5Y", "6Y-10Y", "11Y-19Y", "20Y+"))
supplyData[, .(n_vins = .N, per_vins = round(.N/nrow(supplyData)*100, 2)), by = 'age_group'][order(age_group)]

supplyData$price_group <- "50K+"
supplyData$price_group <- ifelse(supplyData$price <= 5000, "0K-5K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 5000 & supplyData$price <= 10000, "5K-10K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 10000 & supplyData$price <= 15000, "10K-15K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 15000 & supplyData$price <= 20000, "15K-20K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 20000 & supplyData$price <= 30000, "20K-30K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 30000 & supplyData$price <= 40000, "30K-40K", supplyData$price_group)
supplyData$price_group <- ifelse(supplyData$price > 40000 & supplyData$price <= 50000, "40K-50K", supplyData$price_group)
supplyData$price_group <- ordered(supplyData$price_group, levels = c("0K-5K", "5K-10K", "10K-15K", "15K-20K", "20K-30K", "30K-40K", "40K-50K", "50K+"))
supplyData[, .(n_vins = .N, per_vins = round(.N/nrow(supplyData)*100, 2)), by = 'price_group'][order(price_group)]
aggData <- supplyData[, .(n_vins = .N, per_vins = round(.N/nrow(supplyData)*100, 2)), by = list(age_group, price_group)][order(age_group, price_group)]
write.csv(aggData, file = "aggData.csv", row.names = F)

p <- ggplot(supplyData, aes(x=price_group, y=age_group) ) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + labs(title="Population Desnity of Price Group v/s Age Group ",
        x ="Price Group", y = "Age Group")

p <- ggplot(supplyData, aes(x=price, y=age_group) ) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + labs(title="Population Desnity of Price v/s Age Group ",
        x ="Price", y = "Age Group")

p <- ggplot(supplyData, aes(x=price_group, y=age_car) ) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + labs(title="Population Desnity of Price Group v/s Age",
        x ="Price Group", y = "Age")

## Online Only



# select
#   vin,
#   case
#       when lower(customer_name) like '%carvana%' or lower(customer_name) like '%vroom%' then 'online'
#       else 'local'
#   end ol_or_not,
#   min(cast (price as float)) price,
#   2020 - min(cast(model_year as int)) age_car
# from insight.inventory_activity
# where new_used_ind = 'Used'
# and filedate = '2020-08-02'
# and price is not null
# and model_year is not null
# group by 1, 2 ;
