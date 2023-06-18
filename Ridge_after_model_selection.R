## Housekeeping ----------------------------------------------------------------
library(tidyverse)
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

## Preselection ----------------------------------------------------------------

source("functions/fun_preselection.R")
source("functions/fun_z_scores.R")

gtd_pre <- gtd_data %>%
  group_by(date = format(as.yearqtr(date, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = date) %>%
  filter(quarter_average != "2023Q2")

ip_pre <- ip %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  select(quarter_average, ip_mom)

esi_pre <- esi %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

# Period 1: Recession - trainings sample: 2005Q1-2007Q3

gdp_p1 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2007-12-01") %>%
  select(gdp)

gtd_pre_p1 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

ip_pre_p1 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

esi_pre_p1 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

preselection_p1 <-
  preselection(gdp_p1, gtd_pre_p1, esi_pre_p1, ip_pre_p1)

gtd_choice_p1 <- preselection_p1 %>%
  filter(!is.na(tau))

# Period 2: Cyclical Stability - trainings sample: 2005Q1-2013Q3

gdp_p2 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2013-12-01") %>%
  select(gdp)

gtd_pre_p2 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

ip_pre_p2 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

esi_pre_p2 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

preselection_p2 <-
  preselection(gdp_p2, gtd_pre_p2, esi_pre_p2, ip_pre_p2)

gtd_choice_p2 <- preselection_p2 %>%
  filter(!is.na(tau))

# Period 3: Sharp Downturn - trainings sample: 2005Q1-2016Q3

gdp_p3 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2016-12-01") %>%
  select(gdp)

gtd_pre_p3 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q3")

ip_pre_p3 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q3")

esi_pre_p3 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q3")

preselection_p3 <-
  preselection(gdp_p3, gtd_pre_p3, esi_pre_p3, ip_pre_p3)

gtd_choice_p3 <- preselection_p3 %>%
  filter(!is.na(tau))

# Period 4: COVID-19 - trainings sample: 2005Q1-2021Q2

gdp_p4 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2021-09-01") %>%
  select(gdp)

gtd_pre_p4 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2021Q2")

ip_pre_p4 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2021Q2")

esi_pre_p4 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2021Q2")

preselection_p4 <-
  preselection(gdp_p4, gtd_pre_p4, esi_pre_p4, ip_pre_p4)

gtd_choice_p4 <- preselection_p4 %>%
  filter(!is.na(tau))

# Finish preselection and prep for ridge regression

rm(esi_pre, esi_pre_p1, esi_pre_p2, esi_pre_p3, esi_pre_p4)
rm(gdp_p1, gdp_p2, gdp_p3, gdp_p4)
rm(gtd_pre, gtd_pre_p1, gtd_pre_p2, gtd_pre_p3, gtd_pre_p4)
rm(ip_pre, ip_pre_p1, ip_pre_p2, ip_pre_p3, ip_pre_p4)
rm(preselection_p1, preselection_p2, preselection_p3, preselection_p4)

# Save data frames for table creation

saveRDS(gtd_choice_p1, "tables/gtd_choice_p1.RDS")
saveRDS(gtd_choice_p2, "tables/gtd_choice_p2.RDS")
saveRDS(gtd_choice_p3, "tables/gtd_choice_p3.RDS")
saveRDS(gtd_choice_p4, "tables/gtd_choice_p4.RDS")

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

# GTD Period 1

source("functions/fun_bridge_gtd.R")

gtd_choice_p1_var <- gtd_choice_p1$keyword

gtd_bridge_p1_prep <- gtd_data %>%
  select(any_of(gtd_choice_p1_var)) %>%
  mutate(Month = gtd_data$date, .before = 1)
gtd_bridge_p1 <- gtd_bridge_p1_prep %>%
  mutate(across(all_of(c(
    2:ncol(gtd_bridge_p1_prep)
  )), ~ ., .names = "{col}_b"))

gtd_bridge_p1 <- bridge_gtd(gtd_bridge_p1_prep, gtd_bridge_p1)

rm(gtd_bridge_p1_prep, gtd_choice_p1, gtd_choice_p1_var)


# GTD Period 2

gtd_choice_p2_var <- gtd_choice_p2$keyword

gtd_bridge_p2_prep <- gtd_data %>%
  select(any_of(gtd_choice_p2_var)) %>%
  mutate(Month = gtd_data$date, .before = 1)
gtd_bridge_p2 <- gtd_bridge_p2_prep %>%
  mutate(across(all_of(c(
    2:ncol(gtd_bridge_p2_prep)
  )), ~ ., .names = "{col}_b"))

gtd_bridge_p2 <- bridge_gtd(gtd_bridge_p2_prep, gtd_bridge_p2)

rm(gtd_bridge_p2_prep, gtd_choice_p2, gtd_choice_p2_var)

# GTD Period 3

gtd_choice_p3_var <- gtd_choice_p3$keyword

gtd_bridge_p3_prep <- gtd_data %>%
  select(any_of(gtd_choice_p3_var)) %>%
  mutate(Month = gtd_data$date, .before = 1)
gtd_bridge_p3 <- gtd_bridge_p3_prep %>%
  mutate(across(all_of(c(
    2:ncol(gtd_bridge_p3_prep)
  )), ~ ., .names = "{col}_b"))

gtd_bridge_p3 <- bridge_gtd(gtd_bridge_p3_prep, gtd_bridge_p3)

rm(gtd_bridge_p3_prep, gtd_choice_p3, gtd_choice_p3_var)

# GTD Period 4

gtd_choice_p4_var <- gtd_choice_p4$keyword

gtd_bridge_p4_prep <- gtd_data %>%
  select(any_of(gtd_choice_p4_var)) %>%
  mutate(Month = gtd_data$date, .before = 1)
gtd_bridge_p4 <- gtd_bridge_p4_prep %>%
  mutate(across(all_of(c(
    2:ncol(gtd_bridge_p4_prep)
  )), ~ ., .names = "{col}_b"))

gtd_bridge_p4 <- bridge_gtd(gtd_bridge_p4_prep, gtd_bridge_p4)

rm(gtd_bridge_p4_prep, gtd_choice_p4, gtd_choice_p4_var)

## Step 2: Ridge Regression ----------------------------------------------------

# Period 1: Recession - trainings sample: 2005Q1-2007Q3 ------------------------

source("functions/fun_bridge_gtd.R")
source("functions/fun_model_m1.R")

min_date_train <- "2005-01-01"
min_date_test <- "2007-10-01"
max_date_test <- "2009-04-01"

results_m1_p1 <- m1(gtd_choice_p1, gtd_data, esi_bridge, y_bridge, min_date_train, min_date_test, max_date_test)
