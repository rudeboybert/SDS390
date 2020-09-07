# Lec03 Code
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
weatherdata %>% autoplot(TMAX)

# FPP3 2.4 Fig 2.4
weatherdata %>% gg_season(TMAX, labels = "both")

# FPP3 2.5 Fig 2.5 
weatherdata %>% gg_subseries(TMAX)

# FPP3 2.7 Fig 2.16 
weatherdata %>% gg_lag(TMAX, lags = 1:12)

# FPP3 2.8 Fig 2.17
weatherdata %>% ACF(TMAX) %>% autoplot()




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
