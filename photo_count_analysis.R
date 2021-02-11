library(data.table)
library(readr)
library(ggeffects)
library(ggiraphExtra)

options(width = 150)
'%ni%' <- Negate('%in%')
setwd("/Users/addhyanpandey/Desktop/randomRcodes/datasets/")
photo_count_data <- setDT(read_csv("photo_count.csv"))
agg_vin <- photo_count_data[, .N, vin]
photo_count_data <- photo_count_data [vin %ni% agg_vin[N>1, vin]]
summary(photo_count_data)

ggplot(photo_count_data,aes(photo_count,fill=new_used_ind))+
   scale_fill_manual(values=c("red","blue"))+
   geom_histogram(alpha=0.5,binwidth=0.1,position="identity")
a <- glm(data = photo_count_data, srp2vdp~new_used_ind+photo_count)
agg_pc <- photo_count_data[, .(.N, mean_srp2vdp = 100.00*mean(srp2vdp)), by = list(photo_count, new_used_ind)]
agg_pc <- agg_pc[N>10000 & photo_count > 1][order(-mean_srp2vdp)]
usedData <- agg_pc[new_used_ind == 'Used']
newData <- agg_pc[new_used_ind == 'New']
names(agg_pc) <- c('photo_count', 'new_used_ind', 'N', 'conversion_rate_wired')
fit1=lm(conversion_rate_wired~photo_count+new_used_ind,data=agg_pc)
ggPredict(fit1)

agg_pc$pc_group <- NULL
agg_pc$pc_group <- "30+"
agg_pc$pc_group <- ifelse(agg_pc$photo_count <= 2, "0-2", agg_pc$pc_group)
agg_pc$pc_group <- ifelse(agg_pc$photo_count > 2 & agg_pc$photo_count <= 5, "3-5", agg_pc$pc_group)
agg_pc$pc_group <- ifelse(agg_pc$photo_count > 5 & agg_pc$photo_count <= 14, "6-14", agg_pc$pc_group)
agg_pc$pc_group <- ifelse(agg_pc$photo_count > 14 & agg_pc$photo_count <= 30, "15-30", agg_pc$pc_group)
new_agg_pc <- agg_pc[new_used_ind == 'New', .(n_vehicles = sum(N)), by = list(pc_group)][order(pc_group)]
used_agg_pc <- agg_pc[new_used_ind == 'Used', .(n_vehicles = sum(N)), by = list(pc_group)][order(pc_group)]
write.csv(new_agg_pc, file = "new_agg_pc.csv", row.names=F)
write.csv(used_agg_pc, file = "used_agg_pc.csv", row.names=F)
