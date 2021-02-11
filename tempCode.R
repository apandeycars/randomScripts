library(jsonlite)
library(data.table)
pop_file <- "https://datausa.io/api/data?drilldowns=State&measures=Population&year=latest"
pop_data <- jsonlite::fromJSON(pop_file, flatten = TRUE)
pop_data <- setDT(pop_data$data)

book <- read.csv("book.csv")
book$date_id <- as.Date(as.character(book$Date), tryFormats   = "%Y%m%d", origin = "20200101")
book$Positive <- as.numeric(as.character(book$Positive))
book$Negative <- as.numeric(as.character(book$Negative))
book$Pending <- as.numeric(as.character(book$Pending))
book$Hospitalized <- as.numeric(as.character(book$Hospitalized))
book$Death <- as.numeric(as.character(book$Death))
setDT(book)

tmp_testing_start <- book[Total >= 1, .(date = min(date_id)), by = list(State)][order(State)]
names(tmp_testing_start)[2] <- 'date_testing_start'
tmp_pos <- book[Positive >= 1, .(date = min(date_id)), by = list(State)][order(State)]
names(tmp_pos)[2] <- 'date_first_positive'
tmp_hospitalized <- book[Hospitalized >= 1, .(date = min(date_id)), by = list(State)][order(State)]
names(tmp_hospitalized)[2] <- 'date_first_hospitalized'
tmp_death <- book[Death >= 1, .(date = min(date_id)), by = list(State)][order(State)]
names(tmp_death)[2] <- 'date_first_death'
tmp <- merge(tmp_testing_start, tmp_pos, by = 'State', all.x = T)
tmp <- merge(tmp, tmp_hospitalized, by = 'State', all.x = T)
tmp <- merge(tmp, tmp_death, by = 'State', all.x = T)

a$date <- as.Date(gsub(" ", "", substr(as.character(a$date), 0, 9)), tryFormats   = "%m/%d/%y", origin = "1/1/20")

b$date <- as.Date(b$date, tryFormats   = "%Y-%M-%D", origin = "2020-01-01")
b$date <- format(b$date, "%Y%m%d")



scoredData <- read_csv('/Users/addhyanpandey/Downloads/sum_dt_jan_mar.csv')
tempData <- scoredData[date_id >= '2020-02-21']
tempData <- tempData[, churn_result_30:=ifelse(churn_result_30=="as is", 0, 1)]
latest <- tempData[date_id == "2020-03-21", .(historical_CPL_30, churn_result_30, prob_30, current_customer_value_30), by = list(dealer_legacy_id, billing_state_code)]
cumSumData <- tempData[, .(.N, avg_risk_score_rolling_30 = mean(churn_result_30), avg_prob_rolling_30 = mean(prob_30)), by = list(dealer_legacy_id, billing_state_code)]
cumSumData <- merge(cumSumData, latest, by = c('dealer_legacy_id', 'billing_state_code'))
cumSumData <- merge(cumSumData, latest, by = c('dealer_legacy_id', 'billing_state_code'))
#cumSumData <- merge(cumSumData, dls[, dealer_legacy_id], by = c('dealer_legacy_id'), all.x = T)
cancel_cases <- setDT(read.csv('cancel_cases.csv'))
cancel_cases$cancel <- 1
cumSumData <- merge(cumSumData, cancel_cases[, .(dealer_legacy_id, cancel)], by = c('dealer_legacy_id'), all.x = T)
cumSumData$cancel <- ifelse(is.na(cumSumData$cancel), 0, 1)
risk_model_output <- cumSumData[, .(n_dealers = .N, n_risky_30 = sum(avg_risk_score_rolling_30),
                        n_risky = sum(churn_result_30),
                        avg_CPL_30 = mean(historical_CPL_30),
                        revenue_at_risk = sum(ifelse(churn_result_30 == 1, current_customer_value_30, 0)),
                        revenue_at_risk_rolling_30 = sum(ifelse(avg_risk_score_rolling_30 == 1, current_customer_value_30, 0)),
                        revenue_from_pre_cancels = sum(ifelse(cancel == 1, current_customer_value_30, 0))
                      ), by = list(billing_state_code)][order(billing_state_code)]
risk_model_output <- merge(state_map, risk_model_output, by.x = 'state_abbrev', by.y = 'billing_state_code')
risk_model_output <- cumSumData[, .(n_dealers = .N, n_risky_30 = sum(avg_risk_score_rolling_30), n_risky = sum(churn_result_30), avg_CPL_30 = mean(historical_CPL_30)), by = list(billing_state_code)][order(billing_state_code)]
risk_model_output <- merge(state_map, risk_model_output, by.x = 'state_abbrev', by.y = 'billing_state_code')
risk_model_output <- risk_model_output[order(state_name)]
risk_model_output <- merge(risk_model_output[, .(state_abbrev, state_name, n_risky_30, n_risky, avg_CPL_30)], cancel_by_state, by.x = 'state_name', by.y = 'state')
risk_model_output$net_new_exp_cancellations <- risk_model_output$n_risky_30 - risk_model_output$n_cancel_requests
summary(risk_model_output$net_new_exp_cancellations)
risk_model_output[net_new_exp_cancellations <= 0]
head(risk_model_output)
risk_model_output$net_new_exp_cancellations <- ifelse(risk_model_output$n_risky_30 - risk_model_output$n_cancel_requests <= 0, risk_model_output$n_risky - risk_model_output$n_cancel_requests, risk_model_output$n_risky_30 - risk_model_output$n_cancel_requests)
risk_model_output$net_new_exp_cancellations <- round(risk_model_output$net_new_exp_cancellations, 0)
write.csv(risk_model_output, file = "risk_model_output.csv", row.names=F)



us_covid_cases <- setDT(read.csv('us_covid_19_data.csv'))
us_covid_cases$date <- as.Date(as.character(us_covid_cases$date), tryFormats   = "%Y%m%d", origin = "20200101")

p <- plot_ly(us_covid_cases, x = ~date, y = ~positive,
             type = 'scatter', mode = 'lines') %>%
  layout(xaxis = list(title = "Days", tickangle = -45, rangeselector = list(buttons = list(list(count = 7,label = "7 D",step = "day",  stepmode = "todate"),list(count = 1,label = "1 M",step = "month",stepmode = "todate"),list(count = 3,label = "3 M",step = "month",stepmode = "todate"),list(count = 6,label = "6 M",step = "month",stepmode = "todate"),list(count = 1,label = "1 Y",step = "year", stepmode = "todate"),list(count = 1,label = "YTD",step = "year", stepmode = "todate"),list(step = "all")))),
         yaxis = list(title = "Total COVID-19 Positive Cases"),
         title = "Timeline of CARS reaction to COVID-19",
         margin = list(b = 100),
         barmode = 'group'
        )

p

cancel_cases <- setDT(read.csv('cancel_cases.csv'))
cancel_cases$date <- as.Date(gsub(" ", "", substr(as.character(cancel_cases$date), 0, 9)), tryFormats   = "%m/%d/%y", origin = "1/1/20")
daily_cancels <- cancel_cases[, .(cancel_suspension_request = .N), by = list(date)][order(date)]
daily_cancels[, n_cancels_since_march_1 := cumsum(cancel_suspension_request)]

us_covid_cases <- merge(us_covid_cases, daily_cancels, by = 'date')

ay <- list(
  tickfont = list(color = "red"),
  overlaying = "y",
  side = "right",
  title = "Cumulative Monthly Cancel\n Requests"
)
fig <- plot_ly()
fig <- fig %>% add_lines(data = us_covid_cases, x = ~date, y = ~positive, name = "Positive \nCOVID-19 Cases")
fig <- fig %>% add_lines(data = us_covid_cases, x = ~date, y = ~n_cancels_since_march_1,
    name = "Cumulative \nMonthly Cancel \nRequests", yaxis = "y2", line = list(color = 'red'))
fig <- fig %>% layout(
    title = "Timeline of CARS reaction to COVID-19", yaxis2 = ay,
    yaxis = list(title = "Total COVID-19 Positive Cases"),
    xaxis = list(title="Date")
  )

fig
