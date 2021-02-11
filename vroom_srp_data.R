library(data.table)
library(plotly)
library(readr)
library(scales)
options(width = 150)

setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
vroom_srp_data <- setDT(read_csv("vroom_srp_data.csv"))
vroom_srp_data$per_vroom_listings <- 100*vroom_srp_data$n_vroom_listings/vroom_srp_data$n_vehicles_seen
head(vroom_srp_data[, .(search_zipcode, n_vehicles_seen, n_vroom_listings, per_vroom_listings)])
