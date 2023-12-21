library(forecast)

# example
fit <- tbats(USAccDeaths)
plot(fit)
plot(forecast(fit))

# read csv
getwd()
df <- read.csv("./data/2015-01-01_2023-12-20_energy.csv")
tail(df)

# get column names of df
cols <- colnames(df)
cols

# subset smaller df with only date and gesamt using colnames
# subset only last 2 years ie where timestamp_UTC is greater than 2019-12-31
df_small <- df[df$timestamp_UTC > "2023-01-01", c("timestamp_UTC", "gesamt")]
head(df_small)

# fit tbats on df_small
fit <- tbats(df_small$gesamt, seasonal.periods = c(24, 24*7))

# get tbats summary
summary(fit)
plot(fit)
