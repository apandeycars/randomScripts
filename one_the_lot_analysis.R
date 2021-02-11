# one_the_lot_analysis.R

library(data.table)
library(plotly)

inputData <- setDT(read.csv("/Users/addhyanpandey/Desktop/on_the_lot_data.csv"))
inputData$date_id <- as.Date(inputData$date_id, tryFormats = "%m/%d/%y")

dataToPlot <- inputData

p <- plot_ly(dataToPlot , colors = viridis_pal(option = "D")(2),
  x = ~date_id, y = ~n_leads, color = ~on_the_lot_ind,
  type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Connections"),
         title = "On the lot traffic with time",
         margin = list(b = 100),
         barmode = 'group')
p
