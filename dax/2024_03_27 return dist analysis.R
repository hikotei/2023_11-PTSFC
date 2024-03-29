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
               ret3_abs_lag1 = abs(lag(ret3, 1)),
               
               ret4 = compute_return(price, h = 4),
               ret4_abs_lag1 = abs(lag(ret4, 1)),
               
               ret5 = compute_return(price, h = 5),
               ret5_abs_lag1 = abs(lag(ret5, 1))) %>% 

        slice(6:n()) # Cut off the first 5 rows since NA due to ret5

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

ghyp_fits_list <- list()
tdist_fits_list <- list()

for (i in 1:5) {
    
    data <- DAX_returns_train[[paste0("ret", i)]]
    
    title <- paste0(i, "-period Return Histogram with KDE")
    hist(data, breaks = 'FD', freq = FALSE, main = title, ylim = c(0,0.5))
    dens <- density(data) # Perform Kernel Density Estimation (KDE)
    lines(dens, col = "blue") # Plot KDE
    
    # Plot fitted gen hyp PDF
    ghyp_fit <- fit.ghypuv(data, lambda = 1, alpha.bar = 0.5, mu = median(data),
                           sigma = mad(data), gamma = 0, silent = TRUE)
    ghyp_fits_list[[i]] <- ghyp_fit
    curve(dghyp(x, object=ghyp_fit, logvalue=FALSE), col="orange", add = TRUE, n=500)
    
    # Plot fitted normal distribution PDF
    curve(dnorm(x, mean = mean(data), sd = sd(data)), col="green", lwd=1, add = TRUE, n=500)
    
    # Plot Student's t-distribution PDF
    t_fit <- fitdistr(data, "t", start=list(m=mean(data),s=sd(data),df=1), lower=c(-1, 0.001,1))
    tdist_fits_list[[i]] <- t_fit
    
    m <- coef(t_fit)[[1]]
    s <- coef(t_fit)[[2]]
    df <- coef(t_fit)[[3]]
    print(df)
    curve(dt(x, ncp=m/s, df=1), col="red", lwd=1, add = TRUE, n=500)
    
}
