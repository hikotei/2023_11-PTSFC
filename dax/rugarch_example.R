# = = = = = = = = = = = = = = = = = = = = = = = = = =
# Load necessary packages
library(rugarch)
library(tseries)
library(zoo)
library(dplyr)

source("dax_procs.R")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# get dax data ####

start_date  <- "2015-01-01"
# fcast_date  <- "2022-10-10"
fcast_date  <- Sys.Date()

DAX_prices = get.hist.quote(instrument="^GDAXI", 
                            start=start_date, end=fcast_date, 
                            quote="Adjusted",
                            provider="yahoo",
                            compression="d", 
                            retclass="zoo")

# convert zoo to dataframe
DAX_prices <- fortify.zoo(DAX_prices) 
names(DAX_prices) <- c("date", "price")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# data processing ####
# compute cumulative log returns

DAX_returns <- DAX_prices %>%
    mutate(ret1 = compute_return(price, h = 1), 
           ret2 = compute_return(price, h = 2),
           ret3 = compute_return(price, h = 3),
           ret4 = compute_return(price, h = 4),
           ret5 = compute_return(price, h = 5))

plot(DAX_returns$date, DAX_returns$price, type='l')
plot(DAX_returns$date, DAX_returns$ret1, type='l')

DAX_returns <- na.omit(DAX_returns)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# rugarch OWN ####

# - - - - - - - - - - - - - - - - - - - -
# testing external regressors and include.mean = FALSE

set.seed(123)

n <- 100
data <- arima.sim(model = list(ar=0.9), n = n) + rnorm(n) 
trend <- seq(from=1,to=10,length.out=n)
# data <- data + trend
plot(data)

spec = ugarchspec() # by default arma(1,1) and garch(1,1)
fit_mean = ugarchfit(data = data, spec = spec)

spec = ugarchspec(mean.model = list(armaOrder = c(1, 1), include.mean = FALSE), 
                  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
fit_no_mean = ugarchfit(data = data, spec = spec)

plot(data, type='l')
lines(coredata(fitted(fit_mean)), col='red')
lines(coredata(fitted(fit_no_mean)), col='blue')

# - - - - - - - - - - - - - - - - - - - -

mean_model <- list(armaOrder = c(1, 0))
variance_model <- list(model = "sGARCH", garchOrder = c(1, 1))

model <- ugarchspec(variance.model = variance_model, 
                    mean.model = mean_model, 
                    distribution.model = dist, fixed.pars = list(shape = fix_df))

modelfit <- ugarchfit(spec = model, data = DAX_returns[,3], solver = 'hybrid', tol = 1e-3)
sum(infocriteria(modelfit)[1:2])

# - - - - - - - - - - - - - - - - - - - -

library(rugarch)

find_best_arma_garch <- function(r_returns) { 
    
    dist <- 'std'
    fix_df <- 3
    
    # Define the parameter space
    p_values <- 1:5  # AR order
    q_values <- 1:5  # MA order
    r_values <- c(1, 2)  # GARCH order
    s_values <- c(1, 2)  # ARCH order
    
    # Generate all combinations of parameters
    param_combinations <- expand.grid(p = p_values, q = q_values, arch_p = r_values, arch_q = s_values)
    
    # Initialize an empty dataframe to store the results
    result_df <- data.frame(p = numeric(), q = numeric(), arch_p = numeric(), arch_q = numeric(), loglikelihood = numeric(), aic = numeric(), bic = numeric())
    
    # Iterate over parameter combinations
    for (i in 1:nrow(param_combinations)) {
        
        p <- param_combinations$p[i]
        q <- param_combinations$q[i]
        arch_p <- param_combinations$arch_p[i]
        arch_q <- param_combinations$arch_q[i]
        
        mean_model <- list(armaOrder = c(p, q))
        variance_model <- list(model = "sGARCH", garchOrder = c(arch_p, arch_q))
        
        if (dist == 'std') {
            model <- ugarchspec(variance.model = variance_model, 
                                mean.model = mean_model, 
                                distribution.model = dist, fixed.pars = list(shape = fix_df))
            
        } else if (dist == "norm") {
            model <- ugarchspec(variance.model = variance_model, 
                                mean.model = mean_model, 
                                distribution.model = dist)
        }
        
        modelfit <- ugarchfit(spec = model, data = r_returns, solver = 'hybrid', tol = 1e-3)
        
        # Get information criteria
        loglikelihood <- likelihood(modelfit)[1]
        aic <- infocriteria(modelfit)[1]
        bic <- infocriteria(modelfit)[2]
        shibata <- infocriteria(modelfit)[3]
        hannan_quinn <- infocriteria(modelfit)[4]
        
        # Append the results to the dataframe
        result_df <- rbind(result_df, c(p, q, arch_p, arch_q, loglikelihood, aic, bic, shibata, hannan_quinn))
        
    }
    
    # Set column names
    colnames(result_df) <- c("p", "q", "arch_p", "arch_q", "loglikelihood", "aic", "bic", "shibata", "hannan_quinn")
    
    return(result_df)
    
}

# Example usage:
df_result <- find_best_arma_garch(DAX_returns$ret1)

df_result <- df_result %>% 
    mutate(sum_aic_bic = aic + bic, 
           sum_inf_crit = aic + bic + shibata + hannan_quinn) 

head(df_result[order(df_result$sum_inf_crit), ])
head(df_result[order(-df_result$loglikelihood), ])

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# rugarch NILS ####

# Function to get rugarch model with specified parameters
get_rugarch_model <- function(p, q, arch_p, arch_q, dist) {
    
  variance_model <- list(model = "sGARCH", garchOrder = c(arch_p, arch_q))
  mean_model <- list(armaOrder = c(p, q), include.mean = TRUE)
  
  if (dist == 'std') {
    fix_df <- 3
    params <- list(shape = fix_df)
    model <- ugarchspec(variance.model = variance_model, mean.model = mean_model,
                        distribution.model = dist, fixed.pars = params)
  } else if (dist == 'norm') {
    model <- ugarchspec(variance.model = variance_model, mean.model = mean_model,
                        distribution.model = dist)
  }
  
  return(model)
  
}

# Function to perform GARCH forecasting
r_garch_forecast <- function(y, p, q, arch_p, arch_q, dist = 'std') {
    
  model <- get_rugarch_model(p, q, arch_p, arch_q, dist)
  modelfit <- ugarchfit(spec = model, data = y, solver.control = list(tol = 1e-3))
  fore <- ugarchforecast(modelfit)
  
  mu_hist <- fitted(modelfit)[, 1]
  sigma_hist <- sigma(modelfit)[, 1]
  mu <- fitted(fore)[, 1]
  sigma <- sigma(fore)[, 1]
  
  return(list(mu_hist = mu_hist, sigma_hist = sigma_hist, mu = mu, sigma = sigma))
  
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =
# model fit & fcast ####

# Parameters
p <- 5
q <- 3
arch_p <- 2
arch_q <- 3
ignore_first <- 1000

# Example usage
lr1_res <- r_garch_forecast(DAX_returns$ret1[(ignore_first + 1):length(DAX_returns$ret1)], p, q, arch_p, arch_q)
lr2_res <- r_garch_forecast(DAX_returns$ret2[(ignore_first + 1):length(DAX_returns$ret2)], p, q, arch_p, arch_q)
lr3_res <- r_garch_forecast(DAX_returns$ret3[(ignore_first + 1):length(DAX_returns$ret3)], p, q, arch_p, arch_q)
lr4_res <- r_garch_forecast(DAX_returns$ret4[(ignore_first + 1):length(DAX_returns$ret4)], p, q, arch_p, arch_q)
lr5_res <- r_garch_forecast(DAX_returns$ret5[(ignore_first + 1):length(DAX_returns$ret5)], p, q, arch_p, arch_q)

# Extract mean and sigma values
mean_vals <- c(lr1_res$mu, lr2_res$mu, lr3_res$mu, lr4_res$mu, lr5_res$mu)
sigma_vals <- c(lr1_res$sigma, lr2_res$sigma, lr3_res$sigma, lr4_res$sigma, lr5_res$sigma)

# Function to get quantiles from distribution
get_q_from_dist <- function(mean, std, q = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
    
  quantiles <- numeric(length(q))
  eps <- 1e-5
  
  for (i in seq_along(mean)) {
    quants <- qt(q, df = 3) * (std[i] + eps) + mean[i]
    quantiles <- cbind(quantiles, quants)
  }
  
  return(quantiles[, -1])
  
}

# Get quantiles
quantiles_val <- get_q_from_dist(mean_vals, sigma_vals)
