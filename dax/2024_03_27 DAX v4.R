rm(list = ls())

library(dplyr)
library(lubridate)
library(tseries)
library(zoo)
library(quantreg)
library(MASS)
library(ghyp)

set.seed(1103)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# system directory ####

if (Sys.info()[[1]] == 'Windows') {
    comp <- 'surface'
    homedir <- "C:/2023_11-PTSFC"
    
} else {
    comp <- 'mac'
    homedir <- "/Users/yanting/Desktop/2023_11-PTSFC"
}

setwd(homedir)
setwd('./dax')
source("dax_procs.R")
setwd('../')

tau_arr <- c(.025, .25, .5, .75, .975) # quantile levels

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# get dax data ####

start_date  <- "2020-01-01"
fcast_date  <- "2024-02-20"

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
               ret5 = compute_return(price, h = 5)) %>% slice(6:n()) # Cut off the first 5 rows

plot(DAX_returns$price, type='l')
plot(DAX_returns$ret1, type='l')

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# Define start and end dates
start_date <- as.Date("2023-11-15")
end_date <- as.Date("2024-02-14")

# Generate a sequence of Wednesdays
wednesdays <- seq(start_date, end_date, by = "week")
wednesdays <- wednesdays[format(wednesdays, "%u") == "3"]  # Filter only Wednesdays

# = = = = = = = = = = = = = = = = = = = = = = = = = =

fcast_date <- wednesdays[1]
DAX_returns_train <- DAX_returns %>%
    filter(date <= fcast_date) 

n_train <- nrow(DAX_returns_train)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Check Distribution

for (i in 1:5) {
    
    data <- DAX_returns_train[[paste0("ret", i)]]
    
    title <- paste0(i, "-period Return Histogram with KDE")
    hist(data, breaks = 'FD', freq = FALSE, main = title, ylim = c(0,0.5))
    dens <- density(data) # Perform Kernel Density Estimation (KDE)
    lines(dens, col = "blue") # Plot KDE
    
    # Plot fitted gen hyp PDF
    ghyp_fit <- fit.ghypuv(data, lambda = 1, alpha.bar = 0.5, mu = median(data),
                           sigma = mad(data), gamma = 0, silent = TRUE)
    curve(dghyp(x, object = ghyp_fit, logvalue = FALSE), col="orange", add = TRUE, n=500)
    # Plot standard normal distribution PDF
    curve(dnorm(x, mean=0, sd=1), col="green", lwd=1, add = TRUE, n=500)
    # Plot fitted normal distribution PDF
    curve(dnorm(x, mean = mean(data), sd = sd(data)), col="green", lwd=1, add = TRUE, n=500)
    # Plot Student's t-distribution PDF
    curve(dt(x, df=10), col = "red", lwd=1, add = TRUE, n=500)
    
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# quant reg models #### 

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

pred_ret <- t(rbind(DAX_returns_train$pred_ret1[n_train-4,], 
                    DAX_returns_train$pred_ret2[n_train-3,], 
                    DAX_returns_train$pred_ret3[n_train-2,],
                    DAX_returns_train$pred_ret4[n_train-1,], 
                    DAX_returns_train$pred_ret5[n_train,]))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Check Quantile Crossing

# Function to check if a vector is strictly ascending
is_ascending <- function(x) {
    all(diff(x) > 0)
}

# Function to reorder matrix rows based on quantile crossing
reorder_rows <- function(mat) {
    n <- ncol(mat)
    for (i in 1:n) {
        if (!is_ascending(mat[, i])) {
            mat[, i] <- sort(mat[, i])
        }
    }
    return(mat)
}

# Reorder rows if quantile crossing is detected
pred_ret <- reorder_rows(pred_ret)

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
pred_garch_stdnorm <- matrix(NA, nrow = length(tau_arr), ncol = 5)

# loop over 5 fcast horizons
for (jj in 1:5){ 
    for (tau_idx in seq_along(tau_arr)) {
        
        sd <- qnorm(tau_arr[tau_idx]) * sigma(garch_fcast)[jj]
        pred <- garch_fcast_cumulative[jj] + sd
        pred_garch_stdnorm[tau_idx,jj] <- pred
        
    }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - -
# USE DIFFERENT PARAMETRIC DISTRIBUTIONS !!!

# initialize matrix (rows are quantile levels, cols are horizons)
pred_garch_tdist <- matrix(NA, nrow = length(tau_arr), ncol = 5)

# loop over 5 fcast horizons
for (jj in 1:5){ 
    for (tau_idx in seq_along(tau_arr)) {
        
        sd <- qnorm(tau_arr[tau_idx]) * sigma(garch_fcast)[jj]
        pred <- garch_fcast_cumulative[jj] + sd
        pred_garch_stdnorm[tau_idx,jj] <- pred
        
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
# calculate q score #### 

qscore <- function(pred, y, quantile) {
    if (pred > y) {
        return(2 * (1 - quantile) * (pred - y))
    } else {
        return(2 * quantile * (y - pred))
    }
}

# get "future" actual data
DAX_returns_true <- DAX_returns[(n_train+1):(n_train+5), ]
DAX_cumul_ret_true <- c(DAX_returns_true$ret1[1],
                        DAX_returns_true$ret2[2],
                        DAX_returns_true$ret3[3],
                        DAX_returns_true$ret4[4],
                        DAX_returns_true$ret5[5])

calculate_qscore_matrix <- function(pred_mat, y_vec, quantiles) {
    n <- ncol(pred_mat)
    m <- length(quantiles)
    qscore_matrix <- matrix(NA, nrow = m, ncol = n)
    for (i in 1:n) {
        for (j in 1:m) {
            pred <- pred_mat[j, i]
            y <- y_vec[i]
            quantile <- quantiles[j]
            qscore_matrix[j, i] <- qscore(pred, y, quantile)
        }
    }
    return(qscore_matrix)
}

calculate_qscore_matrices <- function(pred_mat, y_vec, quantiles) {
    n <- ncol(pred_mat)
    m <- length(quantiles)
    ret_list <- list()  # Create an empty list to store qscore matrices
    for (quantile in quantiles) {
        qscore_matrix <- matrix(NA, nrow = m, ncol = n)
        for (i in 1:n) {
            for (j in 1:m) {
                pred <- pred_mat[j, i]
                y <- y_vec[i]
                qscore_matrix[j, i] <- qscore(pred, y, quantile)
            }
        }
        ret_list[[as.character(quantile)]] <- qscore_matrix  # Add qscore matrix to the list
    }
    return(ret_list)
}

preds <- list(pred_baseline, pred_ret, pred_garch_stdnorm)

for (pred in preds){
    
    qscore_mat <- calculate_qscore_matrix(pred, DAX_cumul_ret_true, tau_arr)
    mean_qscore <- mean(qscore_mat)
    print(mean_qscore)
    
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# plot ensemble against baseline #### 

w_base  <- 0.3
w_qr    <- 0.4
w_garch <- 0.3
weight_sum <- w_base + w_qr + w_garch
pred_mean <- (w_base * pred_baseline + w_qr * pred_ret + w_garch * pred_garch_stdnorm) / weight_sum

quantile_comparison_plot(list(pred_baseline, pred_ret, pred_garch_stdnorm, pred_mean),
                         model_names = c("Base", "QR", "GARCH", "comb"))

abline(h = 0, lwd = .5, lty = 2)


