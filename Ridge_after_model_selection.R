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

# Period 1: Recession - trainings sample: 2005Q1-2007Q2

gdp_p1 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2007-09-01") %>%
  select(gdp)

gtd_pre_p1 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q2")

ip_pre_p1 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q2")

esi_pre_p1 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q2")

preselection_p1 <-
  preselection(gdp_p1, gtd_pre_p1, esi_pre_p1, ip_pre_p1)

gtd_choice_p1 <- preselection_p1 %>%
  filter(!is.na(tau))

# Period 2: Cyclical Stability - trainings sample: 2005Q1-2013Q2

gdp_p2 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2013-09-01") %>%
  select(gdp)

gtd_pre_p2 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q2")

ip_pre_p2 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q2")

esi_pre_p2 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q2")

preselection_p2 <-
  preselection(gdp_p2, gtd_pre_p2, esi_pre_p2, ip_pre_p2)

gtd_choice_p2 <- preselection_p2 %>%
  filter(!is.na(tau))

# Period 3: Sharp Downturn - trainings sample: 2005Q1-2016Q2

gdp_p3 <- gdp %>%
  filter(Quarter >= "2005-03-01" & Quarter < "2016-09-01") %>%
  select(gdp)

gtd_pre_p3 <- gtd_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q2")

ip_pre_p3 <- ip_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q2")

esi_pre_p3 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2016Q2")

preselection_p3 <-
  preselection(gdp_p3, gtd_pre_p3, esi_pre_p3, ip_pre_p3)

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

esi_pre_p4 <- esi_pre %>%
  filter(quarter_average >= "2005Q1" & quarter_average <= "2019Q2")

preselection_p4 <-
  preselection(gdp_p4, gtd_pre_p4, esi_pre_p4, ip_pre_p4)

gtd_choice_p4 <- preselection_p4 %>%
  filter(!is.na(tau))

# Finish preselection and prep for ridge regression

rm(esi_pre, esi_pre_p1, esi_pre_p2, esi_pre_p3, esi_pre_p4)
rm(gdp_p1, gdp_p2, gdp_p3, gdp_p4)
rm(gtd_pre, gtd_pre_p1, gtd_pre_p2, gtd_pre_p3, gtd_pre_p4)
rm(ip_pre, ip_pre_p1, ip_pre_p2, ip_pre_p3, ip_pre_p4)
rm(preselection_p1,
   preselection_p2,
   preselection_p3,
   preselection_p4)

# Save data frames for table creation

# for_table_p1 <- gtd_choice_p1 %>% 
#   filter(tau < 0.1) %>% 
#   select(-t_stat)
# for_table_p2 <- gtd_choice_p2 %>% 
#   filter(tau < 1) %>% 
#   select(-t_stat)

#saveRDS(for_table_p1, "/tables/pre_p1.RDS")
#saveRDS(for_table_p2, "/tables/pre_p2.RDS")


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

## Step 2: Ridge Regression ----------------------------------------------------

source("functions/fun_bridge_gtd.R")
source("functions/fun_model_m1.R")
source("functions/fun_model_m2.R")
source("functions/fun_model_m3.R")

# Period 1: Recession - trainings sample: 2005Q1-2007Q2 ------------------------

min_date_train <- "2005-01-01"
min_date_test <- "2007-07-01"
max_date_test <- "2009-06-01"

results_m1_p1 <-
  m1(
    gtd_choice_p1,
    gtd_data,
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p1 <-
  m2(
    gtd_choice_p1,
    gtd_data,
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p1 <-
  m3(
    gtd_choice_p1,
    gtd_data,
    esi_bridge,
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
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p2 <-
  m2(
    gtd_choice_p2,
    gtd_data,
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p2 <-
  m3(
    gtd_choice_p2,
    gtd_data,
    esi_bridge,
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
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p3 <-
  m2(
    gtd_choice_p3,
    gtd_data,
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p3 <-
  m3(
    gtd_choice_p3,
    gtd_data,
    esi_bridge,
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
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m2_p4 <-
  m2(
    gtd_choice_p4,
    gtd_data,
    esi_bridge,
    y_bridge,
    min_date_train,
    min_date_test,
    max_date_test
  )

results_m3_p4 <-
  m3(
    gtd_choice_p4,
    gtd_data,
    esi_bridge,
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

# saveRDS(results_p1, "tables/results_p1.RDS")
# saveRDS(results_p2, "tables/results_p2.RDS")
# saveRDS(results_p3, "tables/results_p3.RDS")
# saveRDS(results_p4, "tables/results_p4.RDS")

## Save oos error results ------------------------------------------------------

# Period 1

quarters_p1 <-
  c("2008Q1", "2008Q2", "2008Q3", "2008Q4", "2009Q1", "2009Q2")

oos_error_m1_p1 <- as.data.frame(results_m1_p1[[2]]) %>%
  mutate(tau = results_m1_p1[[1]]$tau, .before = 1)
colnames(oos_error_m1_p1) <- c("tau", quarters_p1)
colnames(oos_error_m1_p1)[2:ncol(oos_error_m1_p1)] <-
  paste(colnames(oos_error_m1_p1)[2:ncol(oos_error_m1_p1)], "M1", sep = "_")

oos_error_m2_p1 <- as.data.frame(results_m2_p1[[2]]) %>%
  mutate(tau = results_m2_p1[[1]]$tau, .before = 1)
colnames(oos_error_m2_p1) <- c("tau", quarters_p1)
colnames(oos_error_m2_p1)[2:ncol(oos_error_m2_p1)] <-
  paste(colnames(oos_error_m2_p1)[2:ncol(oos_error_m2_p1)], "M2", sep = "_")

oos_error_m3_p1 <- as.data.frame(results_m3_p1[[2]]) %>%
  mutate(tau = results_m3_p1[[1]]$tau, .before = 1)
colnames(oos_error_m3_p1) <- c("tau", quarters_p1)
colnames(oos_error_m3_p1)[2:ncol(oos_error_m3_p1)] <-
  paste(colnames(oos_error_m3_p1)[2:ncol(oos_error_m3_p1)], "M3", sep = "_")

oos_error_p1 <- oos_error_m1_p1 %>% 
  left_join(oos_error_m2_p1, by = "tau") %>% 
  left_join(oos_error_m3_p1, by = "tau")

rm(oos_error_m1_p1, oos_error_m2_p1, oos_error_m3_p1)

# Period 2

quarters_p2 <-
  c("2014Q1", "2014Q2", "2014Q3", "2014Q4", "2015Q1", "2015Q2", "2015Q3", "2015Q4", "2016Q1")

oos_error_m1_p2 <- as.data.frame(results_m1_p2[[2]]) %>%
  mutate(tau = results_m1_p2[[1]]$tau, .before = 1)
colnames(oos_error_m1_p2) <- c("tau", quarters_p2)
colnames(oos_error_m1_p2)[2:ncol(oos_error_m1_p2)] <-
  paste(colnames(oos_error_m1_p2)[2:ncol(oos_error_m1_p2)], "M1", sep = "_")

oos_error_m2_p2 <- as.data.frame(results_m2_p2[[2]]) %>%
  mutate(tau = results_m2_p2[[1]]$tau, .before = 1)
colnames(oos_error_m2_p2) <- c("tau", quarters_p2)
colnames(oos_error_m2_p2)[2:ncol(oos_error_m2_p2)] <-
  paste(colnames(oos_error_m2_p2)[2:ncol(oos_error_m2_p2)], "M2", sep = "_")

oos_error_m3_p2 <- as.data.frame(results_m3_p2[[2]]) %>%
  mutate(tau = results_m3_p2[[1]]$tau, .before = 1)
colnames(oos_error_m3_p2) <- c("tau", quarters_p2)
colnames(oos_error_m3_p2)[2:ncol(oos_error_m3_p2)] <-
  paste(colnames(oos_error_m3_p2)[2:ncol(oos_error_m3_p2)], "M3", sep = "_")

oos_error_p2 <- oos_error_m1_p2 %>% 
  left_join(oos_error_m2_p2, by = "tau") %>% 
  left_join(oos_error_m3_p2, by = "tau")

rm(oos_error_m1_p2, oos_error_m2_p2, oos_error_m3_p2)

# Period 3

quarters_p3 <-
  c("2017Q1", "2017Q2", "2017Q3", "2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4")

oos_error_m1_p3 <- as.data.frame(results_m1_p3[[2]]) %>%
  mutate(tau = results_m1_p3[[1]]$tau, .before = 1)
colnames(oos_error_m1_p3) <- c("tau", quarters_p3)
colnames(oos_error_m1_p3)[2:ncol(oos_error_m1_p3)] <-
  paste(colnames(oos_error_m1_p3)[2:ncol(oos_error_m1_p3)], "M1", sep = "_")

oos_error_m2_p3 <- as.data.frame(results_m2_p3[[2]]) %>%
  mutate(tau = results_m2_p3[[1]]$tau, .before = 1)
colnames(oos_error_m2_p3) <- c("tau", quarters_p3)
colnames(oos_error_m2_p3)[2:ncol(oos_error_m2_p3)] <-
  paste(colnames(oos_error_m2_p3)[2:ncol(oos_error_m2_p3)], "M2", sep = "_")

oos_error_m3_p3 <- as.data.frame(results_m3_p3[[2]]) %>%
  mutate(tau = results_m3_p3[[1]]$tau, .before = 1)
colnames(oos_error_m3_p3) <- c("tau", quarters_p3)
colnames(oos_error_m3_p3)[2:ncol(oos_error_m3_p3)] <-
  paste(colnames(oos_error_m3_p3)[2:ncol(oos_error_m3_p3)], "M3", sep = "_")

oos_error_p3 <- oos_error_m1_p3 %>% 
  left_join(oos_error_m2_p3, by = "tau") %>% 
  left_join(oos_error_m3_p3, by = "tau")

rm(oos_error_m1_p3, oos_error_m2_p3, oos_error_m3_p3)

# Period 4

quarters_p4 <-
  c("2019Q4", "2020Q1", "2020Q2", "2020Q3", "2020Q4", "2021Q1", "2021Q2")

oos_error_m1_p4 <- as.data.frame(results_m1_p4[[2]]) %>%
  mutate(tau = results_m1_p4[[1]]$tau, .before = 1)
colnames(oos_error_m1_p4) <- c("tau", quarters_p4)
colnames(oos_error_m1_p4)[2:ncol(oos_error_m1_p4)] <-
  paste(colnames(oos_error_m1_p4)[2:ncol(oos_error_m1_p4)], "M1", sep = "_")

oos_error_m2_p4 <- as.data.frame(results_m2_p4[[2]]) %>%
  mutate(tau = results_m2_p4[[1]]$tau, .before = 1)
colnames(oos_error_m2_p4) <- c("tau", quarters_p4)
colnames(oos_error_m2_p4)[2:ncol(oos_error_m2_p4)] <-
  paste(colnames(oos_error_m2_p4)[2:ncol(oos_error_m2_p4)], "M2", sep = "_")

oos_error_m3_p4 <- as.data.frame(results_m3_p4[[2]]) %>%
  mutate(tau = results_m3_p4[[1]]$tau, .before = 1)
colnames(oos_error_m3_p4) <- c("tau", quarters_p4)
colnames(oos_error_m3_p4)[2:ncol(oos_error_m3_p4)] <-
  paste(colnames(oos_error_m3_p4)[2:ncol(oos_error_m3_p4)], "M3", sep = "_")

oos_error_p4 <- oos_error_m1_p4 %>% 
  left_join(oos_error_m2_p4, by = "tau") %>% 
  left_join(oos_error_m3_p4, by = "tau")

rm(oos_error_m1_p4, oos_error_m2_p4, oos_error_m3_p4)

saveRDS(oos_error_p1, "tests/oos_errors_p1_ridge.RDS")
saveRDS(oos_error_p2, "tests/oos_errors_p2_ridge.RDS")
saveRDS(oos_error_p3, "tests/oos_errors_p3_ridge.RDS")
saveRDS(oos_error_p4, "tests/oos_errors_p4_ridge.RDS")





