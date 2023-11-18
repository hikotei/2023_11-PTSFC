rm(list = ls())


library(tidyverse)
# library(fpp3)
# library(fable.prophet)
library(prophet)

set.seed(1103)

#setwd("/your/working_directory/")
source("dax_procs.R")
tau <- c(.025, .25, .5, .75, .975) # quantile levels

# forecast date
forecast_date <- "2023-11-08"

# load data, select rows up to forecast date
data_raw <- read.table("^GDAXI.csv", sep = ",", header = TRUE, 
                  na.strings = "null") %>%
  mutate(Date = ymd(Date)) %>% filter(Date <= forecast_date) %>%
  as_tibble()

# Check whether data contains latest observation
most_recent <- tail(data_raw$Date, 1)
if (most_recent < forecast_date){
  warning(paste0("Most recent observation is ", most_recent, 
                 ". Forecast date is ", forecast_date))
}


# calculate log-returns
data <- data_raw %>%
    mutate(
         ret1 = compute_return(Adj.Close, h = 1), 
         ret2 = compute_return(Adj.Close, h = 2),
         ret3 = compute_return(Adj.Close, h = 3),
         ret4 = compute_return(Adj.Close, h = 4),
         ret5 = compute_return(Adj.Close, h = 5)
    ) %>% 
    filter(Date >= "2016-11-08") %>%
    as_tibble()

## data exploration
View(data)

data %>%
    ggplot(aes(x=Date, y = ret1)) + 
    geom_line()


## simple forecast

# data_ts <- data %>% 
#     tsibble(index = Date, regular = TRUE) %>%
#     mutate(trading_day = row_number(),
#             weekday = ) %>%
#     tsibble(index = trading_day)

##### prophet
data_ts <- data.frame(
    ds = data$Date,
    y = data$ret1        
)

m <- prophet()
m <- add_country_holidays(m, country_name = 'DE')
m <- fit.prophet(m, data_ts)

m$train.holiday.names

future <- data.frame(
    ds = c("2023-11-09","2023-11-10", "2023-11-13","2023-11-14","2023-11-15")
)

make_future_dataframe

forecast <- predict(m, future)
prophet_plot_components(m, forecast)

View(forecast)

##### other approach

# Load necessary libraries
library(forecast)

# Load and prepare your time series data (replace with your own data)

data_clean <- data %>%
    filter(Date >= as.Date("2016-11-14")) %>%
    select(Date, ret1)


data_ts <- ts(data, frequency = 5)

# Fit an ARIMA model
model <- auto.arima(data_ts)

# Generate probabilistic forecasts (95% prediction intervals)
forecasts <- forecast(model, level = c(2.5, 97.5))

# Visualize the probabilistic forecast
plot(forecasts)

# Generate prediction intervals for a 90%, 95%, and 99% confidence level
tau <- c(.025, .25, .5, .75, .975) # quantile levels

# Generate probabilistic forecasts with custom confidence levels
forecasts_custom <- forecast(model, level = tau)

# Visualize the probabilistic forecast with multiple prediction intervals
plot(forecasts_custom)



#### fpp3
data_ts <- data %>% 
     tsibble(index = Date, regular = TRUE) #%>%
    #  mutate(trading_day = row_number()) %>%
    #  tsibble(index = trading_day)

data_ts <- data_ts %>% fill_gaps(ret1 = 0L)

data_ts %>% ggplot(aes(x = Date, y = ret1)) + geom_line()

fit <- data_ts %>%
    model(ARIMA(ret1))

levels <- c("2.5", "25", "50", "75", "97.5") # quantile levels

fc <- fit %>%
    forecast(h = 7, bootstrap = TRUE) %>%
    hilo(level = c(2.5, 25, 50, 75, 97.5))

fc %>% View()

fc %>% select(-1) %>% View()

fc %>% 
    select(Date, "2.5%": "97.5%")

q1 <- fc %>% 
    select(Date, "2.5%") %>%
    unpack_hilo("2.5%") %>% 
    mutate(mean = (`2.5%_lower` + `2.5%_upper`)/2) %>%
    filter(!Date %in% c("2023-11-11", "2023-11-12")) %>%
    rename(q0.025 = mean)

q2 <- fc %>% 
    select(Date, "25%") %>%
    unpack_hilo("25%") %>% 
    mutate(mean = (`25%_lower` + `25%_upper`)/2) %>%
    filter(!Date %in% c("2023-11-11", "2023-11-12")) %>%
    rename(q0.25 = mean)

q3 <- fc %>% 
    select(Date, "50%") %>%
    unpack_hilo("50%") %>% 
    mutate(mean = (`50%_lower` + `50%_upper`)/2) %>%
    filter(!Date %in% c("2023-11-11", "2023-11-12")) %>%
    rename(q0.5 = mean)

q4 <- fc %>% 
    select(Date, "75%") %>%
    unpack_hilo("75%") %>% 
    mutate(mean = (`75%_lower` + `75%_upper`)/2) %>%
    filter(!Date %in% c("2023-11-11", "2023-11-12")) %>%
    rename(q0.75 = mean)

q5 <- fc %>% 
    select(Date, "97.5%") %>%
    unpack_hilo("97.5%") %>% 
    mutate(mean = (`97.5%_lower` + `97.5%_upper`)/2) %>%
    filter(!Date %in% c("2023-11-11", "2023-11-12")) %>%
    rename(q0.975 = mean)

pred_df <- data.frame(forecast_date = forecast_date, target = "DAX", 
            horizon = paste(c(1, 2, 5:7), "day"),
             q0.025 = q1$q0.025, q0.25 = q2$q0.25, q0.5 = q3$q0.5, q0.75 = q4$q0.75, 
                      q0.975 = q5$q0.975)

forecast_date_v2 <- gsub("-", "", forecast_date)
flnm <- paste0("results", forecast_date_v2, "_DAX_benchmark.csv")
write.table(pred_df, flnm, sep = ",", row.names = FALSE, col.names = TRUE)
