## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)
library(lubridate)

## Load data -------------------------------------------------------------------

gdp <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "gdp growth") %>%
  filter(Quarter >= "2004-01-01") %>%
  rename(gdp = `QoQ growth`)
ip <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "IP index") %>%
  filter(Month >= "2004-01-01") %>%
  rename(ip_abs = `IP index`) %>%
  rename(ip_mom = `MoM growth`)
esi <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "DE ESI") %>%
  filter(Month >= "2004-01-01")

# cut all data to same length as gdp data

gdp_latest <- gdp %>%
  slice(n()) %>%
  select(Quarter)
gdp_latest <- as.matrix(gdp_latest)

ip <- ip %>%
  mutate(Month = as.Date(Month)) %>%
  filter(Month <= gdp_latest)

day(esi$Month) <- 1

esi <- esi %>%
  mutate(Month = as.Date(Month)) %>%
  filter(Month <= gdp_latest)

rm(gdp_latest)

## Prep data for bridge equations ----------------------------------------------

# GDP

y_bridge <- gdp
y_bridge <- y_bridge %>%
  select(c(Quarter, gdp)) %>%
  slice(rep(1:nrow(y_bridge), each = 3))
y_bridge$Month <- ip$Month
y_bridge <- y_bridge %>%
  relocate(Month, .after = Quarter)

# ESi

esi_bridge <- esi %>%
  add_column(esi_b = NA)

for (ii in 1:nrow(esi_bridge)) {
  if (month(esi_bridge$Month[ii]) == 1 |
      month(esi_bridge$Month[ii]) == 4 |
      month(esi_bridge$Month[ii]) == 7 |
      month(esi_bridge$Month[ii]) == 10) {
    esi_bridge$esi_b[ii] = esi_bridge$ESI[ii]
  } else if (month(esi_bridge$Month[ii]) == 2 |
             month(esi_bridge$Month[ii]) == 5 |
             month(esi_bridge$Month[ii]) == 8 |
             month(esi_bridge$Month[ii]) == 11) {
    esi_bridge$esi_b[ii] = (esi_bridge$ESI[ii] + esi_bridge$ESI[ii - 1]) / 2
  } else{
    esi_bridge$esi_b[ii] = (esi_bridge$ESI[ii] + esi_bridge$ESI[ii - 1] + esi_bridge$ESI[ii -
                                                                                           2]) / 3
  }
}

# IP

ip_bridge <- ip %>%
  select(c(Month, ip_abs, ip_mom)) %>%
  mutate(ip_b = lag(ip_mom, n = 2))

## Ridge Regression

source("functions/fun_m1_nogoogle.R")
source("functions/fun_m2_nogoogle.R")
source("functions/fun_m3_nogoogle.R")

# Period 1: Recession - trainings sample: 2005Q1-2007Q3 ------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2007-10-01"
max_date_test <- "2009-04-01"

m1_p1 <- m1_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m2_p1 <- m2_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m3_p1 <- m3_nogoogle(esi_bridge, ip_bridge, y_bridge, min_date_train, min_date_test, max_date_test)

period1 <- bind_cols(m1_p1, m2_p1, m3_p1)
names <- c("M1", "M2", "M3")
colnames(period1) <- names

# Period 2: Cyclical stability - trainings sample: 2005Q1-2013Q3 ---------------

min_date_train <- "2005-01-01"
min_date_test <- "2013-10-01"
max_date_test <- "2016-01-01"

m1_p2 <- m1_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m2_p2 <- m2_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m3_p2 <- m3_nogoogle(esi_bridge, ip_bridge, y_bridge, min_date_train, min_date_test, max_date_test)

period2 <- bind_cols(m1_p2, m2_p2, m3_p2)
names <- c("M1", "M2", "M3")
colnames(period2) <- names

# Period 3: Sharp downturn - trainings sample: 2005Q1-2016Q3 -------------------

min_date_train <- "2005-01-01"
min_date_test <- "2017-01-01"
max_date_test <- "2018-12-01"

m1_p3 <- m1_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m2_p3 <- m2_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m3_p3 <- m3_nogoogle(esi_bridge, ip_bridge, y_bridge, min_date_train, min_date_test, max_date_test)

period3 <- bind_cols(m1_p3, m2_p3, m3_p3)
names <- c("M1", "M2", "M3")
colnames(period3) <- names

# Period 4: COVID-19 - trainings sample: 2005Q1-2019Q3 -------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2019-12-01"
max_date_test <- "2021-04-01"

m1_p4 <- m1_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m2_p4 <- m2_nogoogle(esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
m3_p4 <- m3_nogoogle(esi_bridge, ip_bridge, y_bridge, min_date_train, min_date_test, max_date_test)

period4 <- bind_cols(m1_p4, m2_p4, m3_p4)
names <- c("M1", "M2", "M3")
colnames(period4) <- names

## Save results ----------------------------------------------------------------

no_google <- bind_rows(period1, period2, period3, period4)

