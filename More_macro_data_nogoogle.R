## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)
library(lubridate)

## Load data -------------------------------------------------------------------

## Load data -------------------------------------------------------------------

gtd_data <- read_csv("Data prep/gtd_germany.csv") %>%
  filter(date >= "2005-01-01")

gdp <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "gdp growth") %>%
  filter(Quarter >= "2005-01-01") %>%
  rename(gdp = `QoQ growth`)
ip <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "IP index") %>%
  filter(Month >= "2005-01-01") %>%
  rename(ip_abs = `IP index`) %>%
  rename(ip_mom = `MoM growth`)
esi <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "DE ESI") %>%
  filter(Month >= "2005-01-01")

vacancies <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "vacancies") %>%
  select(-vacancies) %>% 
  filter(date >= "2005-01-01")

ifo <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "ifo")

auftragseingang <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "Auftragseingang") %>%
  select(-auftragseingang) %>%
  filter(date >= "2005-01-01")

ten_year <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "10y") %>%
  filter(date >= "2005-01-01")

short_term <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "st") %>%
  filter(date >= "2005-01-01")

# cut all data to same length as gdp data

gdp_latest <- gdp %>%
  slice(n()) %>%
  select(Quarter)
gdp_latest <- as.matrix(gdp_latest)

gtd_data <- gtd_data %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= gdp_latest)

ip <- ip %>%
  mutate(Month = as.Date(Month)) %>%
  filter(Month <= gdp_latest)

day(esi$Month) <- 1

esi <- esi %>%
  mutate(Month = as.Date(Month)) %>%
  filter(Month <= gdp_latest)

auftragseingang <- auftragseingang %>%
  mutate(Month = as.Date(date), .before = 1) %>%
  filter(Month <= gdp_latest) %>% 
  select(-date)

ifo <- ifo %>%
  mutate(Month = as.Date(date), .before = 1) %>%
  filter(Month <= gdp_latest) %>% 
  select(-date)

short_term <- short_term %>%
  mutate(Month = as.Date(date), .before = 1) %>%
  filter(Month <= gdp_latest) %>% 
  select(-date)

ten_year <- ten_year %>%
  mutate(Month = as.Date(date), .before = 1) %>%
  filter(Month <= gdp_latest) %>% 
  select(-date)

vacancies <- vacancies %>%
  mutate(Month = as.Date(date), .before = 1) %>%
  filter(Month <= gdp_latest) %>% 
  select(-date)

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

# ESI

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

# ifo

ifo_bridge <- ifo %>%
  add_column(ifo_b = NA)

for (ii in 1:nrow(ifo_bridge)) {
  if (month(ifo_bridge$Month[ii]) == 1 |
      month(ifo_bridge$Month[ii]) == 4 |
      month(ifo_bridge$Month[ii]) == 7 |
      month(ifo_bridge$Month[ii]) == 10) {
    ifo_bridge$ifo_b[ii] = ifo_bridge$ifo_klima[ii]
  } else if (month(ifo_bridge$Month[ii]) == 2 |
             month(ifo_bridge$Month[ii]) == 5 |
             month(ifo_bridge$Month[ii]) == 8 |
             month(ifo_bridge$Month[ii]) == 11) {
    ifo_bridge$ifo_b[ii] = (ifo_bridge$ifo_klima[ii] + ifo_bridge$ifo_klima[ii - 1]) / 2
  } else{
    ifo_bridge$ifo_b[ii] = (ifo_bridge$ifo_klima[ii] + ifo_bridge$ifo_klima[ii - 1] + ifo_bridge$ifo_klima[ii -
                                                                                                             2]) / 3
  }
}

# vacancies

vacancies_bridge <- vacancies %>%
  add_column(vacancies_b = NA) %>% 
  rename(vacancies = vacancies_mom)

for (ii in 1:nrow(vacancies_bridge)) {
  if (month(vacancies_bridge$Month[ii]) == 1 |
      month(vacancies_bridge$Month[ii]) == 4 |
      month(vacancies_bridge$Month[ii]) == 7 |
      month(vacancies_bridge$Month[ii]) == 10) {
    vacancies_bridge$vacancies_b[ii] = vacancies_bridge$vacancies[ii]
  } else if (month(vacancies_bridge$Month[ii]) == 2 |
             month(vacancies_bridge$Month[ii]) == 5 |
             month(vacancies_bridge$Month[ii]) == 8 |
             month(vacancies_bridge$Month[ii]) == 11) {
    vacancies_bridge$vacancies_b[ii] = (vacancies_bridge$vacancies[ii] + vacancies_bridge$vacancies[ii - 1]) / 2
  } else{
    vacancies_bridge$vacancies_b[ii] = (vacancies_bridge$vacancies[ii] + vacancies_bridge$vacancies[ii - 1] + vacancies_bridge$vacancies[ii -
                                                                                                                                           2]) / 3
  }
}

# Short term

short_term_bridge <- short_term %>% 
  add_column(short_term_b = NA)

for (ii in 1:nrow(short_term_bridge)) {
  if (month(short_term_bridge$Month[ii]) == 1 |
      month(short_term_bridge$Month[ii]) == 2 |
      month(short_term_bridge$Month[ii]) == 4 |
      month(short_term_bridge$Month[ii]) == 5 |
      month(short_term_bridge$Month[ii]) == 7 |
      month(short_term_bridge$Month[ii]) == 8 |
      month(short_term_bridge$Month[ii]) == 10 |
      month(short_term_bridge$Month[ii]) == 11) {
    short_term_bridge$short_term_b[ii] = short_term_bridge$short_term[ii]
  } else{
    short_term_bridge$short_term_b[ii] = (short_term_bridge$short_term[ii] + short_term_bridge$short_term[ii - 1]) / 2
  }
}

# Ten year

ten_year_bridge <- ten_year %>% 
  add_column(ten_year_b = NA) %>% 
  rename(ten_year = long_term)

for (ii in 1:nrow(ten_year_bridge)) {
  if (month(ten_year_bridge$Month[ii]) == 1 |
      month(ten_year_bridge$Month[ii]) == 2 |
      month(ten_year_bridge$Month[ii]) == 4 |
      month(ten_year_bridge$Month[ii]) == 5 |
      month(ten_year_bridge$Month[ii]) == 7 |
      month(ten_year_bridge$Month[ii]) == 8 |
      month(ten_year_bridge$Month[ii]) == 10 |
      month(ten_year_bridge$Month[ii]) == 11) {
    ten_year_bridge$ten_year_b[ii] = ten_year_bridge$ten_year[ii]
  } else{
    ten_year_bridge$ten_year_b[ii] = (ten_year_bridge$ten_year[ii] + ten_year_bridge$ten_year[ii - 1]) / 2
  }
}

# IP

ip_bridge <- ip %>%
  select(c(Month, ip_abs, ip_mom)) %>%
  mutate(ip_b = lag(ip_mom, n = 2))

# Auftragseingang

auftragseingang_bridge <- auftragseingang %>% 
  rename(auftragseingang = mom_auftragseingang) %>% 
  mutate(auftragseingang_b = lag(auftragseingang, n = 2))

## Ridge Regression

source("functions/fun_m1_nogoogle.R")
source("functions/fun_m2_nogoogle.R")
source("functions/fun_m3_nogoogle.R")

data_m1 <- esi_bridge %>% 
  left_join(ifo_bridge) %>% 
  left_join(vacancies_bridge) %>% 
  select(-c(ifo_klima, vacancies))

data_m2 <- data_m1 %>% 
  left_join(short_term_bridge) %>% 
  left_join(ten_year_bridge) %>% 
  select(-c(short_term, ten_year))

data_m3 <- data_m2 %>% 
  left_join(auftragseingang_bridge) %>% 
  select(-auftragseingang)


# Period 1: Recession - trainings sample: 2005Q1-2007Q3 ------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2007-07-01"
max_date_test <- "2009-06-01"

m1_p1 <-
  m1_nogoogle(data_m1,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m2_p1 <-
  m2_nogoogle(data_m2,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m3_p1 <-
  m3_nogoogle(data_m3,
              ip_bridge,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)

period1 <- bind_cols(m1_p1, m2_p1, m3_p1)
names <- c("M1", "M2", "M3")
colnames(period1) <- names

# Period 3: Sharp downturn - trainings sample: 2005Q1-2016Q3 -------------------

min_date_train <- "2005-01-01"
min_date_test <- "2016-07-01"
max_date_test <- "2018-12-01"

m1_p3 <-
  m1_nogoogle(data_m1,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m2_p3 <-
  m2_nogoogle(data_m2,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m3_p3 <-
  m3_nogoogle(data_m3,
              ip_bridge,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)

period3 <- bind_cols(m1_p3, m2_p3, m3_p3)
names <- c("M1", "M2", "M3")
colnames(period3) <- names

# Period 4: COVID-19 - trainings sample: 2005Q1-2019Q3 -------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2019-04-01"
max_date_test <- "2021-06-01"

m1_p4 <-
  m1_nogoogle(esi_bridge,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m2_p4 <-
  m2_nogoogle(esi_bridge,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)
m3_p4 <-
  m3_nogoogle(esi_bridge,
              ip_bridge,
              y_bridge,
              min_date_train,
              min_date_test,
              max_date_test)

period4 <- bind_cols(m1_p4, m2_p4, m3_p4)
names <- c("M1", "M2", "M3")
colnames(period4) <- names


no_google <- bind_rows(period1, period3, period4)

saveRDS(no_google, "tables/more_macro_data_no_google.RDS")
