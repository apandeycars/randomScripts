library(data.table)
library(plotly)
library(readr)
library(ggplot2)
library(scales)
options(width = 150)
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
roi_data <- setDT(read_csv("roi_data.csv"))
head(roi_data)
roi_data <- roi_data[!is.na(roi_value_30_day)]
roi_data$roi_value_30_day <- ifelse(roi_data$roi_value_30_day >= 50, 50, roi_data$roi_value_30_day)
hist(roi_data[,roi_value_30_day], xlab = "ROI", main = "Distribution of ROI", breaks=25, ylab = 'No. of Dealers')

tot_obs <- nrow(roi_data)
p <- ggplot(data = roi_data, mapping = aes(x = roi_value_30_day)) +
  geom_histogram(bins = 50, color = 'black', fill = 'grey') +
  scale_y_continuous(
    sec.axis = sec_axis(trans = ~./tot_obs, labels = percent,
                        name = "Proportion (in %)")) +
  xlab("Rolling 30 Days ROI") +
  ylab("Count of Dealers") + ggtitle("Distribution of ROI (Rolling 30 Days)")

library(plyr)
cdat <- ddply(roi_data, "franchise_independent", summarise, rating.mean=mean(roi_value_30_day))
cdat
ggplot(roi_data, aes(x=roi_value_30_day, fill=franchise_independent)) +
    geom_histogram(color = 'black', binwidth=.5, alpha=.5, position="identity") +
    geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=franchise_independent),
               linetype="dashed", size=1)

roi_data$roi_30_group <- NULL
roi_data$roi_30_group <- "50X+"
roi_data$roi_30_group <- ifelse(roi_data$roi_value_30_day <= 1, "0X-1X", roi_data$roi_30_group)
roi_data$roi_30_group <- ifelse(roi_data$roi_value_30_day > 1 & roi_data$roi_value_30_day <= 3, "1X-3X", roi_data$roi_30_group)
roi_data$roi_30_group <- ifelse(roi_data$roi_value_30_day > 3 & roi_data$roi_value_30_day <= 5, "3X-5X", roi_data$roi_30_group)
roi_data$roi_30_group <- ifelse(roi_data$roi_value_30_day > 5 & roi_data$roi_value_30_day <= 10, "5X-10X", roi_data$roi_30_group)
roi_data$roi_30_group <- ifelse(roi_data$roi_value_30_day > 10 & roi_data$roi_value_30_day < 50, "10X-50X", roi_data$roi_30_group)
roi_data$roi_30_group <- ordered(roi_data$roi_30_group, levels = c("0X-1X", "1X-3X", "3X-5X", "5X-10X", "10X-50X", "50X+"))

aggData <- roi_data[, .(n_dealers = .N, per_dealers = round(.N/nrow(roi_data)*100, 2)), by = list(roi_30_group, franchise_independent)][order(roi_30_group, franchise_independent)]

p <- ggplot(roi_data, aes(x=roi_30_group, y=inventory_new) ) +
  geom_bin2d(bins = 100) +
  scale_fill_continuous(type = "viridis") +
  theme_bw() + labs(title="Population Desnity of Price Group v/s Age Group ",
        x ="ROI Group", y = "New Inventory")
