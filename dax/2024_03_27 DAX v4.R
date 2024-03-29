rm(list = ls())

library(dplyr)
library(lubridate)
library(tseries)
library(zoo)
library(quantreg)
library(MASS)
library(ghyp)
library(rugarch)

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
fcast_date  <- "2024-02-22"

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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# helper functions ####

qscore <- function(pred, y, quantile) {
    if (pred > y) {
        return(2 * (1 - quantile) * (pred - y))
    } else {
        return(2 * quantile * (y - pred))
    }
}


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

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# start and end dates ####

start_date <- as.Date("2023-11-15")
end_date <- as.Date("2024-02-14")

# Generate a sequence of Wednesdays
wednesdays <- seq(start_date, end_date, by="week")
wednesdays <- wednesdays[format(wednesdays, "%u") == "3"]  # Filter only Wednesdays

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# main loop over weeks ####

cwd <- getwd()
if (comp == 'surface') { homedir <- "C:/2023_11-PTSFC" }
setwd(homedir)

prefix <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
dirname <- paste0(prefix, " dax res")
print(paste0("saving to: ", dirname))

dir.create(dirname)
setwd(dirname)

# saving true values for comparison at end
truevals_output_df <- data.frame()
all_model_scores_df <- data.frame()

# Record start time
start_time_full <- Sys.time()

for (week_idx in seq_along(wednesdays)){
    
    # Record start time
    start_time <- Sys.time()
    
    fcast_date <- wednesdays[[week_idx]]
    # get dates of thursday, friday, monday, tuesday, wednesday
    subm_dates <- seq(fcast_date, by="day", length.out=8) 
    # remove weekend + first day
    subm_dates <- c(subm_dates[2:3],subm_dates[6:8])
    
    cat(paste(rep("=", 30)), "\n")
    print(paste0("start fcast week ", week_idx, "/", length(wednesdays), ", fcast on ", fcast_date))
    
    DAX_returns_train <- DAX_returns %>%
        filter(date <= fcast_date) 
    
    n_train <- nrow(DAX_returns_train)
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # fit distribution ####
    
    ghyp_fits_list <- list()
    tdist_fits_list <- list()
    
    for (i in 1:5) {
        
        data <- DAX_returns_train[[paste0("ret", i)]]
        
        # fit gen hyp PDF
        ghyp_fit <- fit.ghypuv(data, lambda = 1, alpha.bar = 0.5, mu = median(data),
                               sigma = mad(data), gamma = 0, silent = TRUE)
        ghyp_fits_list[[i]] <- ghyp_fit
        
        # fit student's t-distribution PDF
        t_fit <- fitdistr(data, "t", start=list(m=mean(data),s=sd(data),df=1), lower=c(-1, 0.001,1))
        tdist_fits_list[[i]] <- t_fit
        
    }
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # quant reg models ####
    
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
    
    preds_quant_reg_list <- list()
    
    for (i in seq_along(input_vectors)) {
    
        pred_quant_reg <- gen_quant_reg_pred(input_vectors[[i]], return_variables, 
                                             tau_arr, DAX_returns_train)
        preds_quant_reg_list[[i]] <- pred_quant_reg
        
    }
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # Check Quantile Crossing
    
    # Reorder rows if quantile crossing is detected 
    # for all prediction matrices in preds_quant_reg_list
    
    for (pred_idx in seq_along(preds_quant_reg_list)) {
        preds_quant_reg_list[[pred_idx]] <- reorder_rows(preds_quant_reg_list[[pred_idx]])
    }
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # fit ARMA GARCH ####
    
    garch_dists <- c("norm", "std", "ghyp")
    preds_garch_list <- list()

    for (dist_idx in seq_along(garch_dists)) {
        
        dist <- garch_dists[dist_idx]
        # sGARCH = standard GARCH
        spec_garch  <- ugarchspec(variance.model = list(model="sGARCH", garchOrder=c(3,1)), 
                                  mean.model = list(armaOrder = c(6, 6)), 
                                  distribution.model = dist)
        
        garch_model <- ugarchfit(spec_garch, DAX_returns_train[2:n_train,3])
        garch_fcast <- ugarchforecast(garch_model, n.ahead=5)
        
        # v1 = use quantile fnct directly
            # calculate cumulative returns ??? HOW
        
        # v2 = take pt fcasts calculate cumulative returns manually
            # then manually add uncertainty using garch sigma  
        
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
        # generate prediction quantiles assuming normal distribution 
        # using GARCH sigma's
        
        # initialize matrix (rows are quantile levels, cols are horizons)
        pred_garch <- matrix(NA, nrow = length(tau_arr), ncol = 5)
        
        # loop over 5 fcast horizons
        for (jj in 1:5){ 
            for (tau_idx in seq_along(tau_arr)) {
                
                quantile <- qnorm(tau_arr[tau_idx])
                sd <- quantile * sigma(garch_fcast)[jj]
                pred <- garch_fcast_cumulative[jj] + sd
                pred_garch[tau_idx,jj] <- pred
                
            }
        }
        preds_garch_list[[dist_idx]] <- pred_garch
    }

    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # baseline model ####
    # compute baseline predictions (rolling window)
    
    bench_lookbacks <- c(700, 500, 400, 300, 200, 100, 50)
    preds_bench_list <- list()
    
    for ( bench_idx in seq_along(bench_lookbacks) ) {
        
        n_past_values <- bench_lookbacks[bench_idx]
        
        # initialize matrix (rows are quantile levels, cols are horizons)
        pred_baseline <- matrix(NA, nrow = length(tau_arr), ncol = 5)
        
        # loop over 5 fcast horizons
        for (jj in 1:5){ 
            tmp <- DAX_returns_train[, paste0("ret", jj)] %>% 
                na.omit %>% # removes rows with na
                tail(n_past_values) # selects only #n values in tail
            # return quantiles at the specified probabilities given
            pred_baseline[,jj] <- quantile(tmp, probs = tau_arr)
        }
        preds_bench_list[[ bench_idx ]] <- pred_baseline
    }
    

    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # calculate q score #### 
    
    # get "future" actual data
    DAX_returns_true <- DAX_returns[(n_train+1):(n_train+5), ]
    DAX_cumul_ret_true <- c(DAX_returns_true$ret1[1],
                            DAX_returns_true$ret2[2],
                            DAX_returns_true$ret3[3],
                            DAX_returns_true$ret4[4],
                            DAX_returns_true$ret5[5])
    
    # save weekly true values to main df outside loop
    weekly_true_df <- data.frame(DAX_cumul_ret_true, row.names=subm_dates)
    truevals_output_df <- rbind(truevals_output_df, weekly_true_df)
    
    preds <- c(preds_bench_list, preds_garch_list)
    preds <- c(preds, preds_quant_reg_list)
    
    quant_reg_names <- sapply(input_vectors, function(x) paste('quant_reg :', paste(x, collapse = ' + ')))
    garch_names <- paste0('garch_', garch_dists)
    bench_names <- paste('bench_', bench_lookbacks)
    model_names <- c(bench_names,  garch_names, quant_reg_names)

    mean_scores <- numeric(length(preds))
    fcasts_output_df <- data.frame()
        
    # Calculate mean scores and store names
    for (i in seq_along(preds)){
        
        qscore_mat <- calculate_qscore_matrix(preds[[i]], DAX_cumul_ret_true, tau_arr)
        mean_scores[i] <- mean(qscore_mat)

        # transpose preds such that rows are timestamps
        preds_transpose <- data.frame(t(preds[[i]]))
        colnames(preds_transpose) <- paste0('q', tau_arr)
        # add column with model_name
        # add column with fcast index 1,2,3,4,5
        preds_transpose$model_name <- rep(model_names[[i]], 5)
        preds_transpose$fcast_target_date <- subm_dates
        # Combine with fcasts_output_df
        fcasts_output_df <- rbind(fcasts_output_df, preds_transpose)
        
    }
    
    result_df <- data.frame(Model = model_names, Mean_Score = mean_scores)

    # Check if all_model_scores_df is empty
    if (nrow(all_model_scores_df) == 0) {
        all_model_scores_df <- result_df
        colnames(all_model_scores_df) <- c("Model", paste0(fcast_date))
    } else {
        old_colnames <- colnames(all_model_scores_df)
        all_model_scores_df <- cbind(all_model_scores_df, result_df[,-1])
        colnames(all_model_scores_df) <- c(old_colnames, paste0(fcast_date))
    }
    
    # print ordered results
    result_df <- result_df[order(result_df$Mean_Score), ]
    result_df <- result_df[, c("Mean_Score", "Model")]
    rownames(result_df) <- NULL
    print(result_df[1:5,], right=FALSE)
    
    # = = = = = = = = = = = = = = = = = = = = = = = = = =
    # save to dir #### 

    csv_filename <- paste0("dax_weekly_fcasts_", week_idx, ".csv")
    write.csv(fcasts_output_df, csv_filename, row.names = FALSE)
    
    # Calculate time taken
    end_time <- Sys.time()
    time_taken <- end_time - start_time
    cat("Time taken for this iteration:", round(time_taken, 2), "\n")
    
}

# 1 big csv of all true values at submission timestamps 
write.csv(truevals_output_df, "dax_truevals.csv", row.names=TRUE)

setwd(cwd)

# Calculate time taken
end_time <- Sys.time()
time_taken <- end_time - start_time_full
cat("Total time taken:", time_taken, "\n")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# simple eval : best model #### 

# Take the mean along rows (axis=1)
mean_scores <- rowMeans(all_model_scores_df[, -1])

# Create a new data frame with Model column and Mean_Score column
result_df <- data.frame(Model = all_model_scores_df$Model, Mean_Score = mean_scores)

# Sort the data frame by Mean_Score in ascending order
result_df <- result_df[order(result_df$Mean_Score), ]

# Reset row names
rownames(result_df) <- NULL

# Print the sorted result_df
print(result_df)