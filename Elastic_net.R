## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(glmnet)
library(elasticnet)
library(zoo)
library(lubridate)

## Load data -------------------------------------------------------------------

gtd_data <- read_csv("Data prep/gtd_germany.csv")
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

rm(gdp_latest)

## Data prep -------------------------------------------------------------------

# gdp
y_bridge <- gdp
y_bridge <- y_bridge %>%
  select(c(Quarter, gdp)) %>%
  slice(rep(1:nrow(y_bridge), each = 3))
y_bridge$Month <- ip$Month
y_bridge <- y_bridge %>%
  relocate(Month, .after = Quarter)

# esi

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

# GTD

source("functions/fun_bridge_gtd.R")

gtd_prep <- gtd_data %>%
  rename(Month = date)

gtd_bridge_large <- gtd_prep %>%
  mutate(across(all_of(c(2:ncol(
    gtd_data
  ))), ~ ., .names = "{col}_b"))

gtd_bridge <- bridge_gtd(gtd_data, gtd_bridge_large)
colnames(gtd_bridge) <- gsub('_b', '', colnames(gtd_bridge))

source("functions/fun_elastic_net_m1.R")
source("functions/fun_elastic_net_m2.R")
source("functions/fun_elastic_net_m3.R")

# Period 1: Recession - trainings sample: 2005Q1-2007Q2 ------------------------

min_train <- "2005-01-01"
min_test <- "2007-07-01"
max_test <- "2009-06-01"

m1_p1 <-
  elastic_net_m1(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m2_p1 <-
  elastic_net_m2(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m3_p1 <-
  elastic_net_m3(y_bridge,
                 esi_bridge,
                 gtd_bridge,
                 ip_bridge,
                 min_train,
                 min_test,
                 max_test)

# Period 2: Cyclical stability - trainings sample: 2005Q1-2013Q2 ---------------

min_train <- "2005-01-01"
min_test <- "2013-07-01"
max_test <- "2016-03-01"

m1_p2 <-
  elastic_net_m1(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m2_p2 <-
  elastic_net_m2(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m3_p2 <-
  elastic_net_m3(y_bridge,
                 esi_bridge,
                 gtd_bridge,
                 ip_bridge,
                 min_train,
                 min_test,
                 max_test)

# Period 3: Sharp downturn - trainings sample: 2005Q1-2016Q2 -------------------

min_train <- "2005-01-01"
min_test <- "2016-07-01"
max_test <- "2018-12-01"

m1_p3 <-
  elastic_net_m1(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m2_p3 <-
  elastic_net_m2(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m3_p3 <-
  elastic_net_m3(y_bridge,
                 esi_bridge,
                 gtd_bridge,
                 ip_bridge,
                 min_train,
                 min_test,
                 max_test)

# Period 4: COVID-19 - trainings sample: 2005Q1-2019Q2 -------------------------

min_train <- "2005-01-01"
min_test <- "2019-04-01"
max_test <- "2021-06-01"

m1_p4 <-
  elastic_net_m1(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m2_p4 <-
  elastic_net_m2(y_bridge, esi_bridge, gtd_bridge, min_train, min_test, max_test)
m3_p4 <-
  elastic_net_m3(y_bridge,
                 esi_bridge,
                 gtd_bridge,
                 ip_bridge,
                 min_train,
                 min_test,
                 max_test)

## Save results ----------------------------------------------------------------

results_p1 <- t(c(m1_p1[[1]], m2_p1[[1]], m3_p1[[1]]))
elastic_net_p1 <- as.data.frame(results_p1)
elastic_net_p1 <- elastic_net_p1 %>% 
  rename(M1 = V1) %>% 
  rename(M2 = V2) %>% 
  rename(M3 = V3)

results_p2 <- t(c(m1_p2[[1]], m2_p2[[1]], m3_p2[[1]]))
elastic_net_p2 <- as.data.frame(results_p2)
elastic_net_p2 <- elastic_net_p2 %>% 
  rename(M1 = V1) %>% 
  rename(M2 = V2) %>% 
  rename(M3 = V3)

results_p3 <- t(c(m1_p3[[1]], m2_p3[[1]], m3_p3[[1]]))
elastic_net_p3 <- as.data.frame(results_p3)
elastic_net_p3 <- elastic_net_p3 %>% 
  rename(M1 = V1) %>% 
  rename(M2 = V2) %>% 
  rename(M3 = V3)

results_p4 <- t(c(m1_p4[[1]], m2_p4[[1]], m3_p4[[1]]))
elastic_net_p4 <- as.data.frame(results_p4)
elastic_net_p4 <- elastic_net_p4 %>% 
  rename(M1 = V1) %>% 
  rename(M2 = V2) %>% 
  rename(M3 = V3)

# saveRDS(elastic_net_p1, "tables/elastic_net_p1.RDS")
# saveRDS(elastic_net_p2, "tables/elastic_net_p2.RDS")
# saveRDS(elastic_net_p3, "tables/elastic_net_p3.RDS")
# saveRDS(elastic_net_p4, "tables/elastic_net_p4.RDS")

## Prep errors for DM-test





