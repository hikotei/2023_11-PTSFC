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
    homedir <- "C:/Users/ytl_c/OneDrive/Desktop/23_24 WS (Master)/PTSFC"
    
} else { 
    comp <- 'mac'
    homedir <- "/Users/yanting/OneDrive/Desktop/23_24 WS (Master)/PTSFC"
}

setwd(homedir)
source("dax_procs.R")

tau_arr <- c(.025, .25, .5, .75, .975) # quantile levels

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# get dax data ####

start_date  <- "2020-01-01"
fcast_date  <- "2022-10-10"
# fcast_date  <- Sys.Date()

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
n_train <- n_total - 5 # round(0.7 * n_total)

# Create the training and test sets
DAX_returns_train <- DAX_returns[1:n_train, ]
DAX_returns_test <- DAX_returns[(n_train+1):n_total, ]

# true cumulative returns
true_ret1 <- cbind(DAX_returns_test$ret1[1],
                   DAX_returns_test$ret2[2],
                   DAX_returns_test$ret3[3],
                   DAX_returns_test$ret4[4],
                   DAX_returns_test$ret5[5])

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# quant reg models #### 

# example quantile regression
# y = cumulative log returns h=1
# x = ret sqrd of yesterday

par(mfrow = c(1,1))

plot(ret1 ~ ret1_sqrd_lag1, data = DAX_returns_train, pch = 16,
     xlab = "ret1_sqrd_lag1", ylab = "ret1")

abline(lm(ret1 ~ ret1_sqrd_lag1, data = DAX_returns_train), 
       col = "red", lty = 2)

abline(rq(ret1 ~ ret1_sqrd_lag1, data = DAX_returns_train, tau=0.75), 
       col = "lightblue", lty = 2)
abline(rq(ret1 ~ ret1_sqrd_lag1, data = DAX_returns_train, tau=0.25), 
       col = "blue", lty = 2)

legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)

# - - - - - - - - - - - - - - - - - - - - - - - - - -

# all quantile regression models for h = {1,2,3,4,5}
# y = cumulative log returns h=i
# x = ret sqrd of yesterday

rqfit_ret1 <- rq(ret1 ~ ret1_sqrd_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret2 <- rq(ret2 ~ ret1_sqrd_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret3 <- rq(ret3 ~ ret1_sqrd_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret4 <- rq(ret4 ~ ret1_sqrd_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

rqfit_ret5 <- rq(ret5 ~ ret1_sqrd_lag1, 
                 tau = tau_arr, data = DAX_returns_train)

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# with more lags

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

DAX_returns_test <- DAX_returns_test %>%
    mutate(
        pred_ret1 = predict(rqfit_ret1, newdata = ., interval = "confidence"),
        pred_ret2 = predict(rqfit_ret2, newdata = ., interval = "confidence"),
        pred_ret3 = predict(rqfit_ret3, newdata = ., interval = "confidence"),
        pred_ret4 = predict(rqfit_ret4, newdata = ., interval = "confidence"),
        pred_ret5 = predict(rqfit_ret5, newdata = ., interval = "confidence")
    )

# Extract actual returns from the test set
actual_returns <- DAX_returns_test %>%
    select(ret1, ret2, ret3, ret4, ret5)

# Extract predicted returns from the test set
predicted_returns <- DAX_returns_test %>%
    select(pred_ret1, pred_ret2, pred_ret3, pred_ret4, pred_ret5)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# plot QR ####
# actual and predicted returns for each quantile level

# # Function to generate colors with varying alpha
# get_alpha_color <- function(tau) {
#     alpha <- 1 - abs(0.5 - tau)
#     return(rgb(1, 0, 0, alpha = alpha))
# }
# 
# # Example of using the function
# colors  <- sapply(tau_arr, get_alpha_color)
# y_max   <- 10
# 
# for (h_idx in 1:5) {
# 
#     plot(actual_returns[, h_idx], type = "l", col = "blue",
#          main = paste("Quantile Regression - ret_h=", h_idx),
#          ylim = c(-y_max,y_max))
# 
#     legend("topright", legend = c("Actual", "Predicted"),
#            col = c("blue", "red"), lty = 1)
# 
#     for (tau_idx in seq_along(1:length(tau_arr))) {
# 
#         lines(predicted_returns[, h_idx][,tau_idx], col=colors[tau_idx])
#         label <- paste("tau=", tau_arr[tau_idx])
# 
#     }
# }

pred_ret <- t(rbind(predicted_returns$pred_ret1[1,], 
                  predicted_returns$pred_ret2[2,], 
                  predicted_returns$pred_ret3[3,],
                  predicted_returns$pred_ret4[4,], 
                  predicted_returns$pred_ret5[5,]))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# fit ARMA GARCH ####

# - - - - - - - - - - - - - - - - - - - - - - - - - -
## ARMA fit ####

library(forecast)

# try to find best ARMA order
max_lag <- 30

acf_plt <- acf(DAX_returns_train[2:n_train,5], lag.max=max_lag)
pacf_plt <- pacf(DAX_returns_train[2:n_train,5], lag.max=max_lag)

which.max(abs(acf_plt$acf[2:max_lag]))
which.max(abs(pacf_plt$acf))

# auto arima return arima(0,0,0) but then its just "straight line"
# fit <- auto.arima(DAX_returns_train[2:n_train,5], 
#                   max.p = 10, max.q = 10, 
#                   max.P = 10, max.Q = 10, 
#                   max.d = 3, stationary = TRUE, ic = 'aicc')
# 
# plot(forecast(fit,h=20))
# fit

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# try different GARCH orders

library(rugarch)

# ar_part <- 1:10
# ma_part <- 1:10
# 
# len <- length(ar_part) * length(ma_part)
# init <- rep(0,len)
# 
# models_garch <- data.frame(p=init, q=init, AIC=init, BIC=init)
# pos <- 0
# 
# for (p in ar_part) {
#     for (q in ma_part) {
# 
#         pos <- pos + 1
#         spec_garch <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(p,q)))
# 
#         tryCatch({
#             temp <- ugarchfit(spec_garch, DAX_returns_train[2:n_train,5])
#             models_garch[pos,] <- c(p, q, infocriteria(temp)[1], infocriteria(temp)[2])
#             print(paste0("Finished with Model ",p, ",",q))
#         })
# 
#     }
# }
# 
# models_garch$sum <- models_garch$AIC + models_garch$BIC
# print('best model is ...')
# models_garch[which.min(models_garch$sum),]

#' GARCH(4,1) seems to be best
#' AIC has more weight on likelihood
#' BIC punishes larger models more (depending on sample size)

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# implement best ARMA(p,q) GARCH(p,q) model

spec_garch  <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(3,1)), 
                          mean.model = list(armaOrder = c(6, 6)))

garch_model <- ugarchfit(spec_garch, DAX_returns_train[2:n_train,5])
garch_fcast <- ugarchforecast(garch_model, n.ahead=5)

# n.ahead   = The forecast horizon.
# n.roll    = The no. of rolling forecasts to create beyond the first one

#' The default argument of n.roll = 0 denotes no rolling and returns the standard n.ahead forecast. 
#' Critically, since n.roll depends on data being available from which to base the rolling forecast, 
#' the ugarchfit function needs to be called with the argument out.sample 
#' being at least as large as the n.roll argument, 
#' or in the case of a specification being used instead of a fit object, 
#' the out.sample argument directly in the forecast function.

# head(sigma(garch_fcast))
# head(fitted(garch_fcast))

# plot(garch_fcast, which=1)
# plot(garch_fcast, which=3)

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# calc cumulative log returns

garch_fcast_cumulative <- numeric(5)
prev_ret <- 0

for (idx in 1:5) {
    ret <- fitted(garch_fcast)[idx] + prev_ret
    garch_fcast_cumulative[idx] <- ret
    prev_ret <- prev_ret + fitted(garch_fcast)[idx]
}

# plot(seq(1,5), true_ret1, type='l', ylim=c(-1,3))
# lines(garch_fcast_cumulative, col='blue', lty='dashed', lw=2)

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
    
    # eh h=1 ... take trailing last n values for ret1
    # and estimate the 0.025 0.250 0.500 0.750 0.975 quantiles
    
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
lines(seq(1,5), true_ret1, col='blue', lty='dashed', lw=2)

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

pred_df[,4:8] <- t(pred_baseline)

fcast_date_ <- gsub("-", "_", fcast_date)
print(paste0("saving to: ", fcast_date_))
newdir <- paste0(homedir, "/Submissions/", fcast_date_)
if (!file.exists(newdir)) { dir.create(newdir) }
setwd(newdir)

flnm <- paste0(fcast_date_, "_DAX.csv")
write.table(pred_df, flnm, sep = ",", row.names = FALSE, col.names = TRUE)
setwd(homedir)
