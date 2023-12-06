rm(list = ls())

library(dplyr)
library(lubridate)

set.seed(1103)

setwd("/your/working_directory/")
source("dax_procs.R")

# = = = = = = = = = = = = = = = = = = = = = = = = = =

tau <- c(.025, .25, .5, .75, .975) # quantile levels

# forecast date
forecast_date <- "2022-02-09"

# load data, select rows up to forecast date
dat <- read.table("^GDAXI.csv", sep = ",", header = TRUE, 
                  na.strings = "null") %>%
  mutate(Date = ymd(Date)) %>% filter(Date <= forecast_date)

# Check whether data contains latest observation
most_recent <- tail(dat$Date, 1)

if (most_recent < forecast_date){
  warning(paste0("Most recent observation is ", most_recent, 
                 ". Forecast date is ", forecast_date))
}

# = = = = = = = = = = = = = = = = = = = = = = = = = =

# compute baseline predictions (rolling window)
dat_baseline <- dat %>%
  mutate(ret1 = compute_return(Adj.Close, h = 1), 
         ret2 = compute_return(Adj.Close, h = 2),
         ret3 = compute_return(Adj.Close, h = 3),
         ret4 = compute_return(Adj.Close, h = 4),
         ret5 = compute_return(Adj.Close, h = 5))

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
