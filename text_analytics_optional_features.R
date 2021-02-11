library(data.table)
library(dplyr)
library(readr)
library(tidytext)

setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
optional_features <- setDT(read_csv("optional_features.csv"))
setDF(optional_features)
setattr(optional_features, "class", c("tbl", "tbl_df", "data.frame"))
#optional_features <- tibble(optional_features)
word_level <- optional_features %>%
  unnest_tokens(word, optional_feature)
data(stop_words)
word_level <- word_level %>%
  anti_join(stop_words)
word_level %>%
  count(word, sort = TRUE)




# select
#   vin,
#   optional_feature,
#   cast(price as float) price
# from insight_prod.inventory_activity
# where filedate = '2020-10-10'
#   and new_used_ind = 'Used'
#   and make_name = 'Ford'
#   and price is not null
#   and price <> ''
#   and cast(price as float) > 100
#   and cast(price as float) < 100000
#   and "30_day_vdp_rolling_wired" > 1
#   and mileage is not null
# group by 1, 2, 3
# ;
