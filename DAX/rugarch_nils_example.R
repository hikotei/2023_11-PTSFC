# Install and load necessary packages
# install.packages("rugarch")
library(rugarch)

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

# Parameters
p <- 5
q <- 3
arch_p <- 2
arch_q <- 3
ignore_first <- 2000

# Example usage
lr1_res <- r_garch_forecast(hist$lr1[(ignore_first + 1):length(hist$lr1)], p, q, arch_p, arch_q)
lr2_res <- r_garch_forecast(hist$lr2[(ignore_first + 1):length(hist$lr2)], p, q, arch_p, arch_q)
lr3_res <- r_garch_forecast(hist$lr3[(ignore_first + 1):length(hist$lr3)], p, q, arch_p, arch_q)
lr4_res <- r_garch_forecast(hist$lr4[(ignore_first + 1):length(hist$lr4)], p, q, arch_p, arch_q)
lr5_res <- r_garch_forecast(hist$lr5[(ignore_first + 1):length(hist$lr5)], p, q, arch_p, arch_q)

# Extract mean and sigma values
mean_vals <- c(lr1_res$mu, lr2_res$mu, lr3_res$mu, lr4_res$mu, lr5_res$mu)
sigma_vals <- c(lr1_res$sigma, lr2_res$sigma, lr3_res$sigma, lr4_res$sigma, lr5_res$sigma)

# Function to get quantiles from distribution
get_q_from_dist <- function(mean, std, q = c(0.025, 0.25, 0.5, 0.75, 0.975)) {
  quantiles <- numeric(length(q))
  eps <- 1e-5
  
  for (i in seq_along(mean)) {
    quants <- qt(q, df = 3, location = mean[i], scale = std[i] + eps)
    quantiles <- cbind(quantiles, quants)
  }
  
  return(quantiles[, -1])
}

# Get quantiles
quantiles_val <- get_q_from_dist(mean_vals, sigma_vals)
