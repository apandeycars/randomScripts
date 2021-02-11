library(data.table)
library(readr)
options(width = 150)
data_80 <- setDT(read_csv('/Users/addhyanpandey/Downloads/test-results-80.csv'))
100*nrow(data_80[actual >= pred_lb & actual <= pred_ub])/nrow(data_80)
100*median(data_80[,(pred_ub-pred_lb)/actual])

data_70 <- setDT(read_csv('/Users/addhyanpandey/Downloads/test-results-70.csv'))
100*nrow(data_70[actual >= pred_lb & actual <= pred_ub])/nrow(data_70)
100*median(data_70[,(pred_ub-pred_lb)/actual])

data_60 <- setDT(read_csv('/Users/addhyanpandey/Downloads/test-results-60.csv'))
100*nrow(data_60[actual >= pred_lb & actual <= pred_ub])/nrow(data_60)
100*median(data_60[,(pred_ub-pred_lb)/actual])
