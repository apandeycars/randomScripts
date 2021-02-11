library(data.table)
library(plotly)
options(width = 150)
library(readr)
library(runner)
library(dplyr)
options(width = 150)
setwd("/Users/addhyanpandey/Desktop/randomScripts/datasets/")
conversion <- setDT(read.csv("conversion_post_price_badge_update.csv"))

conversion <- conversion[, filedate:=as.Date(filedate)]
conversion <- conversion[, avg_srp2vdp:=100.00*avg_srp2vdp]

conversion <- conversion %>%
  mutate(
    avg_srp2vdp_7 = mean_run(
      x = conversion$avg_srp2vdp,
      k = 7,
      idx = as.Date(conversion$filedate))
  )

conversion <- conversion %>%
  mutate(
    conversion_rate_7 = mean_run(
      x = conversion$conversion_rate,
      k = 7,
      idx = as.Date(conversion$filedate))
  )
setDT(conversion)
conversion <- conversion[, avg_srp2vdp:=round(avg_srp2vdp, 2)]
conversion <- conversion[, conversion_rate:=round(conversion_rate, 2)]
conversion <- conversion[, avg_srp2vdp_7:=round(avg_srp2vdp_7, 2)]
conversion <- conversion[, conversion_rate_7:=round(conversion_rate_7, 2)]
dataToPlot <- conversion[filedate >= '2020-11-01']
p <- plot_ly(dataToPlot) %>%
  add_lines(x = ~filedate, y = ~conversion_rate_7, type = 'scatter', mode = 'lines', name = 'Rolling 7 Days Conversion Rate') %>%
  add_lines(x = ~filedate, y = ~conversion_rate, type = 'scatter', mode = 'lines', name = 'Conversion Rate') %>%
  add_segments(x = '2020-11-19', xend = '2020-11-19', y = 5.8, yend = 7.6, name = 'Launch Date') %>%
  layout(xaxis = list(title = "Date", tickangle = -45),
         yaxis = list(title = "Conversion Rate"),
         title = "Rolling 7 Days Conversion Rate",
         margin = list(b = 100),
         barmode = 'group',
         annotations = list(x = 1, y = -0.2, text = "Only Direct and SEO Traffic on Wired",
           showarrow = F, xref='paper', yref='paper',
           xanchor='right', yanchor='auto', xshift=0, yshift=0,
           font=list(size=10, color="black")
         )
  )
p %>% layout (
annotations = list(x = '2020-11-19', y = 6, text = "New Price Badges Rolled Out on\n 2020-11-19, 1:00 AM",
  showarrow = F, xref='x', yref='y',
  xanchor='center', yanchor='auto', xshift=0, yshift=0,
  font=list(size=10, color="black")
))
