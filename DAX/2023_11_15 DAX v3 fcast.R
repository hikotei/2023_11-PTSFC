#' v3 Note
#' Try ARMA GARCH models

rm(list = ls())

library(dplyr)
library(lubridate)

library(tseries)
library(zoo)

library(quantreg)

set.seed(1103)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# system directory ####

if (Sys.info()[[1]] == 'Windows') {
    comp <- 'surface'
    homedir <- "C:/2023_11-PTSFC"
    
} else {
    comp <- 'mac'
    homedir <- "/Users/yanting/OneDrive/Desktop/23_24 WS (Master)/VL - PTSFC"
}

setwd(homedir)
setwd('./dax')
source("dax_procs.R")
setwd('../')

tau_arr <- c(.025, .25, .5, .75, .975) # quantile levels

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# get dax data ####

start_date  <- "2020-01-01"
# fcast_date  <- "2024-01-03"
fcast_date  <- Sys.Date()

DAX_prices = get.hist.quote(instrument="^GDAXI", 
                            start=start_date, end=fcast_date, 
                            quote="Adjusted",
                            provider="yahoo",
                            compression="d", 
                            retclass="zoo")

DAX_prices <- fortify.zoo(DAX_prices)
names(DAX_prices) <- c("date", "price")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# data processing ####
# compute cumulative log returns

DAX_returns <- DAX_prices %>%
    mutate(ret1_sqrd = compute_return(price, h = 1)^2,
           ret1_sqrd_lag1 = lag(ret1_sqrd, 1),
           
           ret1 = compute_return(price, h = 1), 
           ret1_abs_lag1 = abs(lag(ret1, 1)),
           
           ret2 = compute_return(price, h = 2),
           ret2_abs_lag1 = abs(lag(ret2, 1)),
           
           ret3 = compute_return(price, h = 3),
           ret4 = compute_return(price, h = 4),
           ret5 = compute_return(price, h = 5))

plot(DAX_returns$price, type='l')
plot(DAX_returns$ret1, type='l')

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# Remove outliers ???

# DAX_returns <- DAX_returns %>%
#     filter(abs(ret1_sqrd_lag1) <= 10)

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# train test split ####
n_total <- nrow(DAX_returns)
n_train <- n_total

# Create the training and test sets
DAX_returns_train <- DAX_returns[1:n_train, ]

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# quant reg models #### 

# example quantile regression
# y = cumulative log returns h=1
# x = ret sqrd of yesterday

# all quantile regression models for h = {1,2,3,4,5}
# y = cumulative log returns h=i
# x = ret sqrd of yesterday

rqfit_ret1 <- rq(ret1 ~ ret1_sqrd_lag1 + ret1_abs_lag1 + ret2_abs_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret2 <- rq(ret2 ~ ret1_sqrd_lag1 + ret1_abs_lag1 + ret2_abs_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret3 <- rq(ret3 ~ ret1_sqrd_lag1 + ret1_abs_lag1 + ret2_abs_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret4 <- rq(ret4 ~ ret1_sqrd_lag1 + ret1_abs_lag1 + ret2_abs_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret5 <- rq(ret5 ~ ret1_sqrd_lag1 + ret1_abs_lag1 + ret2_abs_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# QR predictions on test set

DAX_returns_train <- DAX_returns_train %>%
    mutate(
        pred_ret1 = predict(rqfit_ret1, newdata = ., interval = "confidence"),
        pred_ret2 = predict(rqfit_ret2, newdata = ., interval = "confidence"),
        pred_ret3 = predict(rqfit_ret3, newdata = ., interval = "confidence"),
        pred_ret4 = predict(rqfit_ret4, newdata = ., interval = "confidence"),
        pred_ret5 = predict(rqfit_ret5, newdata = ., interval = "confidence")
    )

pred_ret <- t(rbind(DAX_returns_train$pred_ret1[n_total-4,], 
                    DAX_returns_train$pred_ret2[n_total-3,], 
                    DAX_returns_train$pred_ret3[n_total-2,],
                    DAX_returns_train$pred_ret4[n_total-1,], 
                    DAX_returns_train$pred_ret5[n_total,]))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# fit ARMA GARCH ####
library(rugarch)

spec_garch  <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(3,1)), 
                          mean.model = list(armaOrder = c(6, 6)))

garch_model <- ugarchfit(spec_garch, DAX_returns_train[2:n_train,5])
garch_fcast <- ugarchforecast(garch_model, n.ahead=5)

plot(garch_fcast, which=1)
plot(garch_fcast, which=3)

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# calc cumulative log returns

garch_fcast_cumulative <- numeric(5)
prev_ret <- 0

for (idx in 1:5) {
    ret <- fitted(garch_fcast)[idx] + prev_ret
    garch_fcast_cumulative[idx] <- ret
    prev_ret <- prev_ret + fitted(garch_fcast)[idx]
}

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# generate prediction quantiles assuming normal distribution using GARCH sigma's

# initialize matrix (rows are quantile levels, cols are horizons)
pred_garch <- matrix(NA, nrow = length(tau_arr), ncol = 5)

# loop over 5 fcast horizons
for (jj in 1:5){ 
    for (tau_idx in seq_along(tau_arr)) {
        
        sd <- qnorm(tau_arr[tau_idx]) * sigma(garch_fcast)[jj]
        pred <- garch_fcast_cumulative[jj] + sd
        pred_garch[tau_idx,jj] <- pred
        
    }
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# baseline model ####
# compute baseline predictions (rolling window)

# initialize matrix (rows are quantile levels, cols are horizons)
pred_baseline <- matrix(NA, nrow = length(tau_arr), ncol = 5)
n_past_values <- 100

# loop over 5 fcast horizons
for (jj in 1:5){ 

    tmp <- DAX_returns_train[, paste0("ret", jj)] %>% 
        na.omit %>% # removes any rows
        tail(n_past_values) # selects only #n values in tail
    
    # return quantiles at the specified probabilities given
    pred_baseline[,jj] <- quantile(tmp, probs = tau_arr)
    
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# plot against baseline #### 

pred_mean <- (pred_baseline + pred_ret + pred_garch) / 3

quantile_comparison_plot(list(pred_baseline, pred_ret, pred_garch, pred_mean),
                         model_names = c("Base", "QR", "GARCH", "comb"))

abline(h = 0, lwd = .5, lty = 2)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# plausibility check ####

check_ascending <- function(df) {
    for (col in names(df)) {
        if (!all(diff(df[[col]]) > 0)) {
            stop(paste0("Column '", col, "' does not have ascending values."))
        }
    }
    print("All columns have ascending values.")
}

# Call the test function
check_ascending(pred_mean)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# submission file ####
# (rows are horizons, cols are quantile levels)

pred_df <- data.frame(forecast_date = fcast_date, 
                      target = "DAX", horizon = paste(c(1, 2, 5:7), "day"),
                      q0.025 = NA, q0.25 = NA, q0.5 = NA, q0.75 = NA, 
                      q0.975 = NA)

pred_df[,4:8] <- t(pred_mean)

fcast_date_ <- gsub("-", "_", fcast_date)
print(paste0("saving to: ", fcast_date_))
newdir <- paste0(homedir, "/Submissions/", fcast_date_)
if (!file.exists(newdir)) { dir.create(newdir) }
setwd(newdir)

flnm <- paste0(fcast_date_, "_DAX.csv")
write.table(pred_df, flnm, sep = ",", row.names = FALSE, col.names = TRUE)
setwd(homedir)
