library(data.table)
library(plotly)
library(readr)
library(ggplot2)
library(scales)
options(width = 150)
library(ggplot2)
theme_set(theme_bw())
library(ggridges)
# devtools::install_github("dgrtwo/gganimate")
library(gganimate)

## define function to reverse date time scale
# source: https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot#43626186
library(scales)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)

  name <- paste(a$name, b$name, sep = "-")

  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))

  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)

}
rev_date <- c_trans("reverse", "date")
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
roi_data <- setDT(read_csv("changing_roi_data.csv"))
head(roi_data)
roi_data$filedate <- as.Date(roi_data$filedate)
roi_data <- roi_data[!is.na(roi_value_30_day)]
roi_data$roi_value_30_day <- ifelse(roi_data$roi_value_30_day >= 20, 20, roi_data$roi_value_30_day)
#hist(roi_data[,roi_value_30_day], xlab = "ROI", main = "Distribution of ROI", breaks=25, ylab = 'No. of Dealers')
data_to_plot <- roi_data

gg_shares <- ggplot(data_to_plot,
    aes(x = roi_value_30_day, y = filedate, group = filedate, # basic aesthetics
      fill = ifelse(..x..>9, 1-..ecdf.., NA), # "cut-off" gradient
      cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.5, rel_min_height = 0.1, calc_ecdf=TRUE) +
  scale_fill_gradient2(
    name     = "ROI",
    midpoint = -.2, high = "steelblue", na.value = "whitesmoke") +
  geom_vline(xintercept = 3, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "X")) +
  scale_y_continuous(trans  = rev_date) +
  xlab("ROI Value (Rolling 30 Days) for paying customers") + ylab("") +
  theme(legend.position = "bottom")
gg_shares

data_to_plot$less_than_1 <- ifelse(data_to_plot$roi_value_30_day < 1, 1, 0)
data_to_plot$value_1_3 <- ifelse(data_to_plot$roi_value_30_day >= 1 & data_to_plot$roi_value_30_day < 3, 1, 0)
data_to_plot$value_3_6 <- ifelse(data_to_plot$roi_value_30_day >= 3 & data_to_plot$roi_value_30_day < 6, 1, 0)
data_to_plot$value_6_9 <- ifelse(data_to_plot$roi_value_30_day >= 6 & data_to_plot$roi_value_30_day < 9, 1, 0)
data_to_plot$more_than_9 <- ifelse(data_to_plot$roi_value_30_day > 9, 1, 0)
agg_data_to_plot <- data_to_plot[, .(
  median_roi_value = round(median(roi_value_30_day), 2),
  per_below_1 = round(100.00*mean(less_than_1), 2),
  per_below_3 = round(100.00*mean(less_than_1 + value_1_3), 2),
  per_1_3 = round(100.00*mean(value_1_3), 2),
  per_3_6 = round(100.00*mean(value_3_6), 2),
  per_6_9 = round(100.00*mean(value_6_9), 2),
  per_above_9 = round(100.00*mean(more_than_9), 2)
), by = list(filedate)][order(filedate)]

p <- plot_ly(agg_data_to_plot) %>%
  add_lines(x = ~filedate, y = ~per_below_1, type = 'scatter', mode = 'lines', name = '0-1') %>%
  add_lines(x = ~filedate, y = ~per_1_3, type = 'scatter', mode = 'lines', name = '1-3') %>%
  add_lines(x = ~filedate, y = ~per_3_6, type = 'scatter', mode = 'lines', name = '3-6') %>%
  add_lines(x = ~filedate, y = ~per_6_9, type = 'scatter', mode = 'lines', name = '6-9') %>%
  add_lines(x = ~filedate, y = ~per_above_9, type = 'scatter', mode = 'lines', name = '9+') %>%
  layout(xaxis = list(title = "Date", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "ROI Value"),
         title = "ROI V2 with Time",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "ROI V2 - With P2B Inbuilt",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p

p <- plot_ly(agg_data_to_plot) %>%
  add_lines(x = ~filedate, y = ~per_below_3, type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "ROI", range = c(5, 40)),
         title = "% Dealers with ROI Under 3X",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0, text = "ROI V2 - With P2B Inbuilt",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p


combined_density_data <- roi_data[filedate %in% c('2020-07-01', '2020-10-01'), .(filedate, roi_value_30_day)]

par(bg = 'black', fg = 'dark grey')
plot(density(roi_data[filedate == '2020-10-01', roi_value_30_day]), col = "purple", col.axis = "grey", main = "ROI on July & Oct 1st", col.main = "grey", xlab="ROI Value", ylab="% Population", col.lab="grey")
lines(density(roi_data[filedate == '2020-07-01', roi_value_30_day]), col = "beige", col.axis = "grey")
legend(15, 0.15, legend=c("October 1", "July 1"), col=c("purple", "beige"), lty=1:1, cex=0.8)

x <- roi_data[filedate == '2020-07-01', .(dealer_legacy_id, roi_value_30_day)]
names(x)[2] <- 'pre_roi'
x$roi_group_july <- "9+"
x <- x[, roi_group_july := ifelse(pre_roi < 1, "-1", roi_group_july)]
x$roi_group_july <- ifelse(x$pre_roi >= 1 & x$pre_roi < 3, "1-3", x$roi_group_july)
x$roi_group_july <- ifelse(x$pre_roi >= 3 & x$pre_roi < 6, "3-6", x$roi_group_july)
x$roi_group_july <- ifelse(x$pre_roi >= 6 & x$pre_roi < 9, "6-9", x$roi_group_july)
y <- roi_data[filedate == '2020-10-01', .(dealer_legacy_id, roi_value_30_day)]
names(y)[2] <- 'post_roi'
y$roi_group_oct <- "9+"
y$roi_group_oct <- ifelse(y$post_roi < 1, "-1", y$roi_group_oct)
y$roi_group_oct <- ifelse(y$post_roi >= 1 & y$post_roi < 3, "1-3", y$roi_group_oct)
y$roi_group_oct <- ifelse(y$post_roi >= 3 & y$post_roi < 6, "3-6", y$roi_group_oct)
y$roi_group_oct <- ifelse(y$post_roi >= 6 & y$post_roi < 9, "6-9", y$roi_group_oct)

data_to_compare <- merge(x, y, by = 'dealer_legacy_id')
data_to_compare$increase <- ifelse(data_to_compare$post_roi >= data_to_compare$pre_roi, 1, 0)

p <- ggplot(data_to_compare, aes(x = roi_group_july, y = roi_group_oct)) +
    geom_bin2d(bins=5) +
    theme_bw() + labs(title="Population Desnity of ROI in July v/s October",
        x ="July", y = "October")

# select
#   dp.filedate,
#   dp.dealer_legacy_id,
#   dp.total_spend_30_day,
#   da.active_product_suite,
#   da.days_until_cancellation,
#   da.active_product_cnt,
#   case when dp.roi_value_30_day is null then 0 else dp.roi_value_30_day end as roi_value_30_day,
#   dp.franchise_independent,
#   da.unique_vin,
#   da.legit_inventory_used + da.legit_inventory_new n_listings_legit,
#   da.customer_name
# from insight.dealer_predictions dp
# join insight.dealer_activity da
#   on dp.dealer_legacy_id = da.dealer_legacy_id
#   and dp.filedate = da.filedate
# where dp.filedate = '2020-07-01'
# -- non cancelled dealers only
#   and cast(da.active_product_cnt as float) > 0
# -- are not online dealers
#   and cast(dp.total_spend_30_day as float) > 0
#   and (lower(da.active_product_suite) like '%uvs%' or lower(da.active_product_suite) like '%nvs%')
# ;
#
#
