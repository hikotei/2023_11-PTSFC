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
        mutate(ret1 = compute_return(price, h = 1), 
               ret1_abs_lag1 = abs(lag(ret1, 1)),
               ret1_sqrd_lag1 = lag(ret1^2, 1),
               
               ret2 = compute_return(price, h = 2),
               ret2_abs_lag1 = abs(lag(ret2, 1)),
               ret2_sqrd_lag1 = lag(ret2^2, 1),
               
               ret3 = compute_return(price, h = 3),
               ret3_abs_lag1 = abs(lag(ret3, 1)),
               ret3_sqrd_lag1 = lag(ret3^2, 1),
               
               ret4 = compute_return(price, h = 4),
               ret4_abs_lag1 = abs(lag(ret4, 1)),
               ret4_sqrd_lag1 = lag(ret4^2, 1),
               
               ret5 = compute_return(price, h = 5),
               ret5_abs_lag1 = abs(lag(ret5, 1)),
               ret5_sqrd_lag1 = lag(ret5^2, 1)) %>% 

        slice(6:n()) # Cut off the first 5 rows since NA due to ret5

# plot(DAX_returns$price, type='l')
# plot(DAX_returns$ret1, type='l')

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
# quant reg models #### 

# all quantile regression models for h = {1,2,3,4,5}
# y = cumulative log returns h=i
# x = ret sqrd of yesterday

gen_quant_reg_pred <- function(input_vector, return_variables, tau_arr, train_data) {
    
    rqfit_list <- list()
    
    for (ret_var in return_variables) {
        formula <- paste(ret_var, "~", paste(input_vector, collapse=" + "))
        rqfit_ret <- rq(formula, tau = tau_arr, data = DAX_returns_train)
        rqfit_list[[ret_var]] <- rqfit_ret
    }
    
    DAX_returns_train <- DAX_returns_train %>%
        mutate(
            pred_ret1 = predict(rqfit_list$ret1, newdata = ., interval = "confidence"),
            pred_ret2 = predict(rqfit_list$ret2, newdata = ., interval = "confidence"),
            pred_ret3 = predict(rqfit_list$ret3, newdata = ., interval = "confidence"),
            pred_ret4 = predict(rqfit_list$ret4, newdata = ., interval = "confidence"),
            pred_ret5 = predict(rqfit_list$ret5, newdata = ., interval = "confidence")
        )
    
    pred_ret <- t(rbind(DAX_returns_train$pred_ret1[n_train-4,], 
                        DAX_returns_train$pred_ret2[n_train-3,], 
                        DAX_returns_train$pred_ret3[n_train-2,],
                        DAX_returns_train$pred_ret4[n_train-1,], 
                        DAX_returns_train$pred_ret5[n_train,]))
    
    return(pred_ret)
    
}

return_variables <- c("ret1", "ret2", "ret3", "ret4", "ret5")

lag1_colnames <- grep("lag1", colnames(DAX_returns), value = TRUE)
combinations <- combn(lag1_colnames, 2, simplify = FALSE) # return as vector

input_vectors <- list(c("ret1_sqrd_lag1", "ret2_sqrd_lag1", "ret1_abs_lag1", "ret2_abs_lag1"),
                      c("ret1_sqrd_lag1", "ret2_sqrd_lag1", "ret1_abs_lag1"),
                      c("ret1_sqrd_lag1", "ret1_abs_lag1", "ret2_abs_lag1"),
                      
                      c("ret1_sqrd_lag1","ret1_abs_lag1", "ret2_abs_lag1", "ret3_abs_lag1", "ret4_abs_lag1", "ret5_abs_lag1"),
                      
                      c("ret1_sqrd_lag1", "ret2_sqrd_lag1", "ret3_sqrd_lag1", "ret4_sqrd_lag1", "ret5_sqrd_lag1"),
                      c("ret1_sqrd_lag1", "ret2_sqrd_lag1", "ret3_sqrd_lag1", "ret4_sqrd_lag1"),
                      c("ret1_sqrd_lag1", "ret2_sqrd_lag1", "ret3_sqrd_lag1"),
                      
                      c("ret1_abs_lag1", "ret2_abs_lag1", "ret3_abs_lag1", "ret4_abs_lag1", "ret5_abs_lag1"),
                      c("ret1_abs_lag1", "ret2_abs_lag1", "ret3_abs_lag1", "ret4_abs_lag1"),
                      c("ret1_abs_lag1", "ret2_abs_lag1", "ret3_abs_lag1"))

input_vectors <- c(input_vectors, combinations)

pred_quant_reg_list <- list()

for (i in seq_along(input_vectors)) {

    pred_quant_reg <- gen_quant_reg_pred(input_vectors[[i]], return_variables, 
                                         tau_arr, DAX_returns_train)
    pred_quant_reg_list[[i]] <- pred_quant_reg
    
}

pred_quant_reg_1 <- pred_quant_reg_list[[1]]

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
pred_quant_reg_1 <- reorder_rows(pred_quant_reg_1)

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

preds <- list(pred_baseline, pred_garch_stdnorm)
preds <- c(preds, pred_quant_reg_list)

quant_reg_names <- sapply(input_vectors, function(x) paste('quant_reg :', paste(x, collapse = ' + ')))
model_names <- c("baseline", "garch_stdnorm", quant_reg_names)

mean_scores <- numeric(length(preds))
# Calculate mean scores and store names
for (i in seq_along(preds)){
    qscore_mat <- calculate_qscore_matrix(preds[[i]], DAX_cumul_ret_true, tau_arr)
    mean_scores[i] <- mean(qscore_mat)
}

result_df <- data.frame(Model = model_names, Mean_Score = mean_scores)
result_df <- result_df[order(result_df$Mean_Score), ]

# for each week
# need to save fcasts
# need to save true values

# 2 csv files for each week
# index = timestamp
# cols = modelname (eg quant_reg), quantiles