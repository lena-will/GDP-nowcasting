## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)
library(lubridate)

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

retail <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "Einzelhandel") %>%
  select(-retail)  %>%
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

# retail <- retail %>%
#   mutate(Month = as.Date(date), .before = 1) %>%
#   filter(Month <= gdp_latest) %>%
#   select(-date)

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

auftragseingang_pre <- auftragseingang %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

ifo_pre <- ifo %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

short_term_pre <- short_term %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

ten_year_pre <- ten_year %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

vacancies_pre <- vacancies %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

all_pre <- esi_pre %>% 
  left_join(auftragseingang_pre) %>% 
  left_join(ifo_pre) %>% 
  left_join(short_term_pre) %>% 
  left_join(ten_year_pre) %>% 
  left_join(vacancies_pre)

# Period 1: Recession - trainings sample: 2005Q1-2007Q3

gdp_p1 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2007-12-01") %>%
  select(gdp)

gtd_pre_p1 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

ip_pre_p1 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

all_p1 <- all_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

preselection_p1 <-
  preselection(gdp_p1, gtd_pre_p1, all_p1, ip_pre_p1)

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

all_p2 <- all_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

preselection_p2 <-
  preselection(gdp_p2, gtd_pre_p2, all_p2, ip_pre_p2)

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

all_p3 <- all_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q3")

preselection_p3 <-
  preselection(gdp_p3, gtd_pre_p3, all_p3, ip_pre_p3)

gtd_choice_p3 <- preselection_p3 %>%
  filter(!is.na(tau))

# Period 4: COVID-19 - trainings sample: 2005Q1-2019Q2

gdp_p4 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2019-09-01") %>%
  select(gdp)

gtd_pre_p4 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2019Q2")

ip_pre_p4 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2019Q2")

all_p4 <- all_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2019Q2")

preselection_p4 <-
  preselection(gdp_p4, gtd_pre_p4, all_p4, ip_pre_p4)

gtd_choice_p4 <- preselection_p4 %>%
  filter(!is.na(tau))

# Finish preselection and prep for ridge regression

rm(all_pre, all_p1, all_p2, all_p3, all_p4)
rm(gdp_p1, gdp_p2, gdp_p3, gdp_p4)
rm(gtd_pre, gtd_pre_p1, gtd_pre_p2, gtd_pre_p3, gtd_pre_p4)
rm(ip_pre, ip_pre_p1, ip_pre_p2, ip_pre_p3, ip_pre_p4)
rm(preselection_p1,
   preselection_p2,
   preselection_p3,
   preselection_p4)

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

# Gathering according to months

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

## Step 2: Ridge Regression ----------------------------------------------------

#source("/Users/lena/Git/GDP-nowcasting/functions/fun_bridge_gtd.R")
source("functions/fun_model_m1.R")
source("functions/fun_model_m2.R")
source("functions/fun_model_m3.R")

# Period 1: Recession - trainings sample: 2005Q1-2007Q3 ------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2007-07-01"
max_date_test <- "2009-06-01"

results_m1_p1 <-
  m1(
    gtd_choice_p1,
    gtd_data,
    data_m1,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p1 <-
  m2(
    gtd_choice_p1,
    gtd_data,
    data_m2,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p1 <-
  m3(
    gtd_choice_p1,
    gtd_data,
    data_m3,
    ip_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

# Period 2: Cyclical stability - trainings sample: 2005Q1-2013Q2 ---------------

min_date_train <- "2005-01-01"
min_date_test <- "2013-07-01"
max_date_test <- "2016-03-01"

results_m1_p2 <-
  m1(
    gtd_choice_p2,
    gtd_data,
    data_m1,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p2 <-
  m2(
    gtd_choice_p2,
    gtd_data,
    data_m2,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p2 <-
  m3(
    gtd_choice_p2,
    gtd_data,
    data_m3,
    ip_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )


# Period 3: Sharp downturn - trainings sample: 2005Q1-2016Q2 -------------------

min_date_train <- "2005-01-01"
min_date_test <- "2016-07-01"
max_date_test <- "2018-12-01"

results_m1_p3 <-
  m1(
    gtd_choice_p3,
    gtd_data,
    data_m1,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p3 <-
  m2(
    gtd_choice_p3,
    gtd_data,
    data_m2,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p3 <-
  m3(
    gtd_choice_p3,
    gtd_data,
    data_m3,
    ip_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

# Period 4: COVID-19 - trainings sample: 2005Q1-2019Q2 -------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2019-04-01"
max_date_test <- "2021-06-01"

results_m1_p4 <-
  m1(
    gtd_choice_p4,
    gtd_data,
    data_m1,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p4 <-
  m2(
    gtd_choice_p4,
    gtd_data,
    data_m2,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p4 <-
  m3(
    gtd_choice_p4,
    gtd_data,
    data_m3,
    ip_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

## Save results ----------------------------------------------------------------

results_p1 <- as.data.frame(results_m1_p1[[1]]) %>% 
  rename(M1 = rmsfe) %>% 
  left_join(as.data.frame(results_m2_p1[[1]])) %>% 
  rename(M2 = rmsfe) %>% 
  left_join(as.data.frame(results_m3_p1[[1]])) %>% 
  rename(M3 = rmsfe) %>% 
  relocate(tau, .before = M1)

results_p2 <- as.data.frame(results_m1_p2[[1]]) %>% 
  rename(M1 = rmsfe) %>% 
  left_join(as.data.frame(results_m2_p2[[1]])) %>% 
  rename(M2 = rmsfe) %>% 
  left_join(as.data.frame(results_m3_p2[[1]])) %>% 
  rename(M3 = rmsfe) %>% 
  relocate(tau, .before = M1)

results_p3 <- as.data.frame(results_m1_p3[[1]]) %>% 
  rename(M1 = rmsfe) %>% 
  left_join(as.data.frame(results_m2_p3[[1]])) %>% 
  rename(M2 = rmsfe) %>% 
  left_join(as.data.frame(results_m3_p3[[1]])) %>% 
  rename(M3 = rmsfe) %>% 
  relocate(tau, .before = M1)

results_p4 <- as.data.frame(results_m1_p4[[1]]) %>% 
  rename(M1 = rmsfe) %>% 
  left_join(as.data.frame(results_m2_p4[[1]])) %>% 
  rename(M2 = rmsfe) %>% 
  left_join(as.data.frame(results_m3_p4[[1]])) %>% 
  rename(M3 = rmsfe) %>% 
  relocate(tau, .before = M1)


saveRDS(results_p1, "tables/more_macro_data_p1.RDS")
saveRDS(results_p2, "tables/more_macro_data_results_p2.RDS")
saveRDS(results_p3, "tables/more_macro_data_results_p3.RDS")
saveRDS(results_p4, "tables/more_macro_data_results_p4.RDS")

## Save oos error results ------------------------------------------------------

# Period 3

oos_error_m1_p3 <- as.data.frame(t(results_m1_p3[[2]]))
colnames(oos_error_m1_p3) <- results_m1_p3[[1]]$tau
colnames(oos_error_m1_p3)[1:ncol(oos_error_m1_p3)] <-
  paste(colnames(oos_error_m1_p3)[1:ncol(oos_error_m1_p3)], "M1", sep = "_")

oos_error_m2_p3 <- as.data.frame(t(results_m2_p3[[2]]))
colnames(oos_error_m2_p3) <- results_m2_p3[[1]]$tau
colnames(oos_error_m2_p3)[1:ncol(oos_error_m2_p3)] <-
  paste(colnames(oos_error_m2_p3)[1:ncol(oos_error_m2_p3)], "M2", sep = "_")

oos_error_m3_p3 <- as.data.frame(t(results_m3_p3[[2]]))
colnames(oos_error_m3_p3) <- results_m3_p3[[1]]$tau
colnames(oos_error_m3_p3)[1:ncol(oos_error_m3_p3)] <-
  paste(colnames(oos_error_m3_p3)[1:ncol(oos_error_m3_p3)], "M3", sep = "_")

oos_error_p3 <- cbind(oos_error_m1_p3, oos_error_m2_p3, oos_error_m3_p3)

rm(oos_error_m1_p3, oos_error_m2_p3, oos_error_m3_p3)

# Period 4

oos_error_m1_p4 <- as.data.frame(t(results_m1_p4[[2]]))
colnames(oos_error_m1_p4) <- results_m1_p4[[1]]$tau
colnames(oos_error_m1_p4)[1:ncol(oos_error_m1_p4)] <-
  paste(colnames(oos_error_m1_p4)[1:ncol(oos_error_m1_p4)], "M1", sep = "_")

oos_error_m2_p4 <- as.data.frame(t(results_m2_p4[[2]]))
colnames(oos_error_m2_p4) <- results_m2_p4[[1]]$tau
colnames(oos_error_m2_p4)[1:ncol(oos_error_m2_p4)] <-
  paste(colnames(oos_error_m2_p4)[1:ncol(oos_error_m2_p4)], "M2", sep = "_")

oos_error_m3_p4 <- as.data.frame(t(results_m3_p4[[2]]))
colnames(oos_error_m3_p4) <- results_m3_p4[[1]]$tau
colnames(oos_error_m3_p4)[1:ncol(oos_error_m3_p4)] <-
  paste(colnames(oos_error_m3_p4)[1:ncol(oos_error_m3_p4)], "M3", sep = "_")

oos_error_p4 <- cbind(oos_error_m1_p4, oos_error_m2_p4, oos_error_m3_p4)

rm(oos_error_m1_p4, oos_error_m2_p4, oos_error_m3_p4)


saveRDS(oos_error_p3, "tests/more_macro_data_oos_errors_p3_ridge.RDS")
saveRDS(oos_error_p4, "tests/more_macro_data_oos_errors_p4_ridge.RDS")


