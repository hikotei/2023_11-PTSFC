library(fpp3)

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## Chpt 5.2 ####

# Re-index based on trading days
google_stock <- gafa_stock |>
    filter(Symbol == "GOOG", year(Date) >= 2015) |>
    mutate(day = row_number()) |>
    update_tsibble(index = day, regular = TRUE)

# Filter the year of interest
google_2015 <- google_stock |> filter(year(Date) == 2015)

# Fit the models
google_fit <- google_2015 |>
    model(
        Mean = MEAN(Close),
        `Naïve` = NAIVE(Close),
        Drift = NAIVE(Close ~ drift())
    )

# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock |>
    filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit |>
    forecast(new_data = google_jan_2016)

# Plot the forecasts
google_fc |>
    autoplot(google_2015, level = NULL) +
    autolayer(google_jan_2016, Close, colour = "black") +
    labs(y = "$US",
         title = "Google daily closing stock prices",
         subtitle = "(Jan 2015 - Jan 2016)") +
    guides(colour = guide_legend(title = "Forecast"))

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## Chpt 5.4 ####

aug <- google_2015 |>
    model(NAIVE(Close)) |>
    augment()

autoplot(aug, .innov) +
    labs(y = "$US",
         title = "Residuals from the naïve method")

# = = = = = = = = = = = = = = = = = = = = = = = = = =
## Chpt 5.5 ####

google_2015 |>
    model(NAIVE(Close)) |>
    forecast(h = 10) |>
    hilo()

google_2015 |>
    model(NAIVE(Close)) |>
    forecast(h = 10) |>
    autoplot(google_2015) +
    labs(title="Google daily closing stock price", y="$US" )

fit <- google_2015 |>
    model(NAIVE(Close))

# generate five possible sample paths for the next 30 trading days.
sim <- fit |> generate(h = 30, times = 5, bootstrap = TRUE)

google_2015 |>
    ggplot(aes(x = day)) +
    geom_line(aes(y = Close)) +
    geom_line(aes(y = .sim, colour = as.factor(.rep)),
              data = sim) +
    labs(title="Google daily closing stock price", y="$US" ) +
    guides(colour = "none")

# Bootstrap functionality is all built into the forecast() function
# so you do not need to call generate() manually

fc <- fit |> forecast(h = 30, bootstrap = TRUE)
fc

#' the fcast dist is now represented as a simulation with 
#' times = 5000 sample paths by default
#' Because no normality assumption, the prediction intervals are not symmetric. 
#' The .mean column is the mean of the bootstrap samples

autoplot(fc, google_2015) +
labs(title="Google daily closing stock price", y="$US")

google_2015 |>
    model(NAIVE(Close)) |>
    forecast(h = 10, bootstrap = TRUE, times = 1000) |>
    hilo()
