# Lec11 code ----------------
library(tidyverse)
library(tsibble)
library(lubridate)
library(fpp3)



# Time series decompositions -------
# From FPP3 Chap 3.2: https://otexts.com/fpp3/components.html
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

# Note: period is [1M] = 1 month:
us_retail_employment

# Plot
us_retail_employment_plot <- us_retail_employment %>%
  autoplot(Employed, color = "gray") +
  labs(x = "Year", y = "Persons (thousands)", title = "Total employment in US retail")
us_retail_employment_plot

# Fit STL decomposition
stl_dcmp <- us_retail_employment %>%
  model(STL(Employed))

# Inspect contents:
components(stl_dcmp)

# What does this mean? The curve in red = signal which you separate from noise.
us_retail_employment_plot + 
  autolayer(components(stl_dcmp), trend, color = "red")

components(stl_dcmp) %>% 
  autoplot() + 
  xlab("Year")



# Simple forecasting methods -------
# From FPP3 Chap 5.2: https://otexts.com/fpp3/simple-methods.html

# Note: period is [1Q] = 1 quarter:
aus_production

# Plot
aus_production %>%
  autoplot(Beer) +
  labs(x = "Year", y = "Beer production (megalitres)", title = "Total beer production in Australia")

# Set "training" data: Only from 1992-2006
train <- aus_production %>% 
  filter_index("1992 Q1" ~ "2006 Q4")

train %>%
  autoplot(Beer) +
  labs(x = "Year", y = "Beer production (megalitres)", title = "Total beer production in Australia")

# Fit/train the models
beer_fit <- train %>%
  model(
    # Mean = MEAN(Beer),
    # `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )

# Generate predictions/forecasts for 14 quarters
beer_fc <- beer_fit %>% 
  forecast(h=14)

# Plot forecasts
beer_fc %>%
  autoplot(train, level = NULL) +
  labs(x = "Year", y = "Beer production (megalitres)", title = "Total beer production in Australia", col = "Forecast")

# Plot forecasts and actual observed values post 2007 Q1
beer_fc %>%
  autoplot(train, level = NULL) +
  labs(x = "Year", y = "Beer production (megalitres)", title = "Total beer production in Australia", col = "Forecast") +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .), color = "black")

# Plot forecasts with prediction error bands
beer_fc %>%
  autoplot(train) +
  labs(x = "Year", y = "Beer production (megalitres)", title = "Total beer production in Australia", col = "Forecast")


















# Lec09 code ----------------
library(moderndive)
library(broom)


# Added on Tue 9/29 9:40am: Visualize both models
library(patchwork)
p1 <- ggplot(MA_schools, aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Percent economically disadvantaged", y = "Math SAT Score",
    color = "School size", title = "Interaction model"
  ) +
  theme(legend.position = "none")
p2 <- ggplot(MA_schools,aes(x = perc_disadvan, y = average_sat_math, color = size)) +
  geom_point(alpha = 0.25) +
  geom_parallel_slopes(se = FALSE) +
  labs(
    x = "Percent economically disadvantaged", y = "Math SAT Score",
    color = "School size", title = "Parallel slopes model"
  ) +
  theme(axis.title.y = element_blank())
p1 + p2


# Fit parallel slopes model: Less complex
model_2_parallel_slopes <- 
  lm(average_sat_math ~ perc_disadvan + size, data = MA_schools)

# Fit interaction model: More complex
model_2_interaction <- 
  lm(average_sat_math ~ perc_disadvan * size, data = MA_schools)


# Compare regression tables. Notice k = 4 vs k = 6
tidy(model_2_parallel_slopes)
tidy(model_2_interaction)


# Compare R^2_adj and AIC
glance(model_2_parallel_slopes)
glance(model_2_interaction)





# Lec06 code ----------------
library(tidyverse)

dendroband_data <- read_csv("https://rudeboybert.github.io/SDS390/static/data/observed_dbh_vs_doy.csv")

# Plot data
base_plot <- ggplot() +
  geom_point(data = dendroband_data, mapping = aes(x = doy, y = dbh)) +
  labs(x = "day of year", y = "dbh")
base_plot


# Define the generalized logistic function. Note the params input has to be a
# vector of length 5 in the correct order:
lg5 <- function(params, doy) {
  # Get 5 parameter values
  L <- params[1]
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  
  # For specified 5 parameters and x = doy, compute y = dbh
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy.ip) / theta)) ^ theta))
  return(dbh)
}


# Let's set an initial guess and plot what the generalized logistic function
# looks like for this initial guess
K <- 12
L <- 14
doy.ip <- 100
r <- 0.05
theta <- 1
params_init <- c(K, L, doy.ip, r, theta)

# Not a great fit!
base_plot + 
  stat_function(fun = lg5, args = list(params = params_init), col = "red", n = 500)



# Let's define the "likelihood function" that we want to maximize/optimize. 
# Recall that this is our criteria for "best fitting" curve.
# These types of functions are covered in MTH 320 Mathematical Statistics
lg5_ML <- function(params, doy, dbh, resid.sd) {
  pred_dbh <- lg5(params, doy)
  pred_ML <- -sum(dnorm(dbh, pred_dbh, resid.sd, log = T))
  return(pred_ML)
}

# Numerical optimization via the optim() function:
lg5_optimization_output <- optim(
  # Our initial guess:
  par = params_init, 
  # The likelihood function we want to optimize:
  fn = lg5_ML, 
  # The points we want want to fit the S-shaped curve to:
  doy = dendroband_data$doy, dbh = dendroband_data$dbh,
  # Some extra stuff:
  resid.sd = 0.1, method = "BFGS", hessian = TRUE
  )
lg5_optimization_output

# The optimal (K*, L*, doy.ip*, r*, theta*) are saved in par
params_star <- lg5_optimization_output$par
params_star

# Let's plot what the generalized logistic function looks like for these optimal
# values. Not bad!
base_plot +
  stat_function(fun = lg5, args = list(params = params_star), col = "red", n = 500)










# Lec06 Data ----------------
library(tidyverse)

generalized_logistic_function <- function(params, doy) {
  L <- params[1]
  K <- params[2]
  doy.ip <- params[3]
  r <- params[4]
  theta <- params[5]
  dbh <- L + ((K - L) / (1 + 1/theta * exp(-(r * (doy - doy.ip) / theta)) ^ theta))
  return(dbh)
}

K <- 13
L <- 15
doy.ip <- 200
r <- 0.075
theta <- 2
sigma <- 0.05

params <- c(K, L, doy.ip, r, theta)

set.seed(79)
observed_values <- tibble(
  doy = seq(from = 1, to = 365, by = 5),
  dbh = lg5.pred(params, doy)
) %>%
  mutate(dbh = dbh + rnorm(n(), sd = sigma))
write_csv(observed_values, path = "static/data/observed_dbh_vs_doy.csv")



# Lec03 Code ----------------
library(tidyverse)
library(tsibble)
library(lubridate)
library(fpp3)

weatherdata <-
  read_csv("https://rudeboybert.github.io/SDS390/static/data/NCDC_NOAA_precip_temp.csv") %>%
  mutate(
    # Convert to variable of type date
    date = dmy(DATE), 
    # This will be used for the group_by() below
    month = floor_date(date, unit = "month"),
    # Using special function from fpp package to define yearmonth units
    month = yearmonth(month)
  ) %>% 
  group_by(month) %>% 
  summarize(TMAX_avg = mean(TMAX, na.rm = TRUE), PRCP_avg = mean(PRCP, na.rm = TRUE)) %>% 
  # Convert to tsibble = time series tibble data type
  as_tsibble(index = month) 

# FPP3 2.2 Fig 2.1
weatherdata %>% autoplot(TMAX_avg)

# FPP3 2.4 Fig 2.4
weatherdata %>% gg_season(TMAX_avg, labels = "both")

# FPP3 2.5 Fig 2.5 
weatherdata %>% gg_subseries(TMAX_avg)

# FPP3 2.7 Fig 2.16 
weatherdata %>% gg_lag(TMAX_avg, lags = 1:12)

# FPP3 2.8 Fig 2.17
weatherdata %>% ACF(TMAX_avg) %>% autoplot()



# Extra ----------------




dendro <- 
  read_csv("static/data/all_stems.csv") %>% 
  as_tibble() %>%
  filter(tag %in% c(082422, 202215, 172202)) %>%
  filter(tag %in% c(082422)) %>%
  mutate(sp = case_when(
    sp == "litu" ~ "tulip poplar",
    sp == "qual" ~ "white oak",
    sp == "quru" ~ "red oak"
  )) %>%
  unite("date", c(year, month, day), sep = "-") %>%
  mutate(date = ymd(date)) %>%
  filter(date >= ymd("2015-01-01")) %>% 
  mutate(prev_measure = lag(measure)) %>% 
  select(date, prev_measure, measure) %>% 
  mutate(growth = measure - prev_measure) %>% 
  as_tsibble(index = date, regular = FALSE)

dendro %>% autoplot(measure)
dendro %>% autoplot(growth)
dendro %>% tsibble::fill_gaps() %>% gg_season(growth, labels = "both")
dendro %>% gg_subseries(growth, period = "day")
dendro %>% gg_lag(growth, lags = 1:12)
dendro %>% ACF(growth) %>% autoplot()
