# = = = = = = = = = = = = = = = = = = = = = = = = = =
#' 
#' v1 is stupid because what i tried to do was
#' 
#' fit quantile regression models for each cum log ret
#' ret1 = ret1_lag1 + ret1_lag2 + ret1_lag3
#' 
#' which is basically saying that tmrws return is always 
#' = intercep + 3 x yesterday + 4 x day before yesterday + etc
#' 
#' ... which doesnt make sense at all 
#' 
#' in v2 i try R_{t+1} is always equal to b * R_t^2
#' ie a measure of todays volatility
#' + some kind of intercept
#' 
#' this makes a little more sense, but it is still a large generalization
#' since tmrw's price obviously isnt = some factor times today's price
#' 
# = = = = = = = = = = = = = = = = = = = = = = = = = =

rm(list = ls())

library(dplyr)
library(lubridate)

library(tseries)
library(zoo)

library(quantreg)

set.seed(1103)

setwd("C:/Users/ytl_c/OneDrive/Desktop/23_24 WS (Master)/PTSFC")
source("dax_procs.R")

tau <- c(.025, .25, .5, .75, .975) # quantile levels

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# get dax data ####

start_date  <- "2022-01-01"
fcast_date  <- Sys.Date()

DAX_prices = get.hist.quote(instrument="^GDAXI", 
                            start=start_date, end=fcast_date, 
                            quote="Adjusted",
                            provider="yahoo",
                            compression="d", 
                            retclass="zoo")

DAX_prices <- fortify.zoo(DAX_prices)
names(DAX_prices) <- c("Date", "Adj.Close")

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# compute cumulative log returns ####

DAX_returns <- DAX_prices %>%
    mutate(ret1 = compute_return(Adj.Close, h = 1), 
           ret2 = compute_return(Adj.Close, h = 2),
           ret3 = compute_return(Adj.Close, h = 3),
           ret4 = compute_return(Adj.Close, h = 4),
           ret5 = compute_return(Adj.Close, h = 5))

# Create lagged versions of each column
DAX_returns_lagged <- DAX_returns %>% 
    mutate(
        
        ret1_lag1 = lag(ret1, 1),
        ret1_lag2 = lag(ret1, 2),
        ret1_lag3 = lag(ret1, 3),
        
        ret2_lag1 = lag(ret2, 1),
        ret2_lag2 = lag(ret2, 2),
        ret2_lag3 = lag(ret2, 3),
        
        ret3_lag1 = lag(ret3, 1),
        ret3_lag2 = lag(ret3, 2),
        ret3_lag3 = lag(ret3, 3),
        
        ret4_lag1 = lag(ret4, 1),
        ret4_lag2 = lag(ret4, 2),
        ret4_lag3 = lag(ret4, 3),
        
        ret5_lag1 = lag(ret5, 1),
        ret5_lag2 = lag(ret5, 2),
        ret5_lag3 = lag(ret5, 3)
    )

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# train test split ####

n_total <- nrow(DAX_returns_lagged)
n_train <- 0.7 * n_total

DAX_returns_lagged_train    <- DAX_returns_lagged[1:n_train,]
DAX_returns_lagged_test     <- DAX_returns_lagged[n_train+1:n_total,]

# = = = = = = = = = = = = = = = = = = = = = = = = = =

par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))

plot(ret1 ~ ret1_lag1, data = DAX_returns_lagged_train, pch = 16)

abline(lm(ret1 ~ ret1_lag1, data = DAX_returns_lagged_train), 
       col = "red", lty = 2)
abline(rq(ret1 ~ ret1_lag1, data = DAX_returns_lagged_train), 
       col = "blue", lty = 2)

legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)

# fit quantile regression on train set #### 

# simple quantile regression
# y = cumulative log returns h = {1,2,3,4,5}
# x = cumulative log returns of past 5 days

rqfit_ret1 <- rq(ret1 ~ ret1_lag1 + ret1_lag2 + ret1_lag3, 
                 tau = tau, data = DAX_returns_lagged_train)

rqfit_ret2 <- rq(ret2 ~ ret2_lag1 + ret2_lag2 + ret2_lag3, 
                 tau = tau, data = DAX_returns_lagged_train)

rqfit_ret3 <- rq(ret3 ~ ret3_lag1 + ret3_lag2 + ret3_lag3, 
                 tau = tau, data = DAX_returns_lagged_train)

rqfit_ret4 <- rq(ret4 ~ ret4_lag1 + ret4_lag2 + ret4_lag3, 
                 tau = tau, data = DAX_returns_lagged_train)

rqfit_ret5 <- rq(ret5 ~ ret5_lag1 + ret5_lag2 + ret5_lag3, 
                 tau = tau, data = DAX_returns_lagged_train)

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# predict on the test set ####

DAX_returns_lagged_test <- DAX_returns_lagged_test %>%
    mutate(
        pred_ret1 = predict(rqfit_ret1, newdata = ., interval = "confidence"),
        pred_ret2 = predict(rqfit_ret2, newdata = ., interval = "confidence"),
        pred_ret3 = predict(rqfit_ret3, newdata = ., interval = "confidence"),
        pred_ret4 = predict(rqfit_ret4, newdata = ., interval = "confidence"),
        pred_ret5 = predict(rqfit_ret5, newdata = ., interval = "confidence")
    )

# Extract actual returns from the test set
actual_returns <- DAX_returns_lagged_test %>%
    select(ret1, ret2, ret3, ret4, ret5)

# Extract predicted returns from the test set
predicted_returns <- DAX_returns_lagged_test %>%
    select(pred_ret1, pred_ret2, pred_ret3, pred_ret4, pred_ret5)

# Plot the actual and predicted returns for each quantile level
par(mfrow = c(5, 1), mar = c(2, 2, 2, 2))

for (i in 1:5) {
    
    plot(actual_returns[, i], type = "l", col = "blue", 
         ylab = paste("ret", i), 
         xaxt = "n", 
         main = paste("Quantile Regression - ret", i))
    
    lines(predicted_returns[, i], col = "red")
    
    # axis(1, at = seq(1, nrow(DAX_returns_lagged_test), by = 10), labels = FALSE)

    legend("topright", legend = c("Actual", "Predicted"), 
           col = c("blue", "red"), lty = 1)
    
}


# = = = = = = = = = = = = = = = = = = = = = = = = = =

# compute baseline predictions (rolling window)

# initialize matrix (rows are quantile levels, cols are horizons)
pred_baseline <- matrix(NA, nrow = length(tau), ncol = 5)
for (jj in 1:5){
  tmp <- dat_baseline[, paste0("ret", jj)] %>% na.omit %>% tail(1000)
  pred_baseline[,jj] <- quantile(tmp, probs = tau)
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# Plot 
quantile_comparison_plot(list(pred_baseline), 
                         model_names = c("Base"))
abline(h = 0, lwd = .5, lty = 2)

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# Make submission file (rows are horizons, cols are quantile levels)
pred_df <- data.frame(forecast_date = forecast_date, 
                      target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)

pred_df[,4:8] <- t(pred_baseline)

forecast_date_v2 <- gsub("-", "", forecast_date)
flnm <- paste0("../../ptsfc_results/", forecast_date_v2, "/",
               forecast_date_v2, "_DAX_benchmark.csv")
write.table(pred_df, flnm, sep = ",", row.names = FALSE, col.names = TRUE)
