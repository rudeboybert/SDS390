# Lec03
library(tidyverse)
library(tsibble)
library(lubridate)
library(fpp3)

weatherdata <-
  read_csv("~/Documents/Trees/growth_phenology/climate data/NCDC_NOAA_precip_temp.csv") %>%
  mutate(
    date = dmy(DATE), 
    month = floor_date(date, unit = "month"),
    month = yearmonth(month)
  ) %>% 
  group_by(month) %>% 
  summarize(TMAX = mean(TMAX, na.rm = TRUE), PRCP = mean(PRCP, na.rm = TRUE)) %>% 
  as_tsibble(index = month) 

weatherdata %>% autoplot(TMAX)
weatherdata %>% gg_season(TMAX, labels = "both")
weatherdata %>% gg_subseries()
weatherdata %>% gg_lag(TMAX, lags = 1:12)
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
