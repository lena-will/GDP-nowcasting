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
  mutate(
    tau = case_when(
      t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
      t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
      t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
      t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
      t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
      t_stat >= 2.81 ~ 0.005
    )
  ) %>%
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
  mutate(
    tau = case_when(
      t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
      t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
      t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
      t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
      t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
      t_stat >= 2.81 ~ 0.005
    )
  ) %>%
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
  mutate(
    tau = case_when(
      t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
      t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
      t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
      t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
      t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
      t_stat >= 2.81 ~ 0.005
    )
  ) %>%
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
  mutate(
    tau = case_when(
      t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
      t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
      t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
      t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
      t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
      t_stat >= 2.81 ~ 0.005
    )
  ) %>%
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

## Prep data for bridge equations ----------------------------------------------

# GDP

y_ridge <- gdp
y_ridge <- y_ridge %>%
  select(c(Quarter, gdp)) %>%
  slice(rep(1:nrow(y_ridge), each = 3))
y_ridge$Month <- ip$Month
y_ridge <- y_ridge %>%
  relocate(Month, .after = Quarter)

# ESi

esi_bridge <- esi %>%
  add_column(esi_b = NA)
day(esi_bridge$Month) <- 1

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

# Model 1 (First month of a quarter)

source("functions/fun_model_m1.R")

min_date_train <- "2005-01-01"
max_date_train <- "2007-10-01"
min_date_test <- 
max_date_test <- 

gtd_period <- gtd_bridge_p1

# -----

X_m1 <- esi_bridge %>% 
  select(-ESI) %>% 
  rename(esi = esi_b) %>% 
  left_join(gtd_period, by = "Month") %>% 
  filter(month(Month) == 1 | month(Month) == 4 | month(Month) == 7 | month(Month) == 10 )

y_m1 <- y_ridge %>% 
  filter(month(Month) == 1 | month(Month) == 4 | month(Month) == 7 | month(Month) == 10 )

# training

X_m1_train <- X_m1 %>% 
  filter(Month >= min_date_train & Month < max_date_train) %>% 
  select(-Month)
y_m1_train <- y_m1 %>% 
  filter(Month >= min_date_train & Month < max_date_train) %>% 
  select(gdp)
X_m1_train <- as.matrix(X_m1_train)
y_m1_train <- as.matrix(y_m1_train)

alpha_ini <- as.matrix(seq(from = 0.01, to = 1, length.out = 15))
counter <- 0
n <- nrow(y_m1)
gcv <- c()
alpha <- alpha_ini[5, 1]*n

for(ii in 1:length(alpha_ini)){
  ident <- diag(ncol(X_m1_train))
  alpha <- alpha_ini[ii]*n
  beta_hat_pls <- solve(t(X_m1_train)%*%X_m1_train + alpha*ident)%*%t(X_m1_train)%*%y_m1_train
  y_hat_pls <- X_m1_train%*%beta_hat_pls
  gcv[ii] <- (1/n)%*%t(y_m1_train - y_hat_pls)%*%(y_m1_train - y_hat_pls)/(1-sum(diag(X_m1_train%*%solve(t(X_m1_train)%*%X_m1_train + alpha*ident)%*%t(X_m1_train)))*(1/n))^2
}

gcv_min <- which(gcv == min(gcv))
alpha_min <- alpha_ini[gcv_min]

# testing

X_m1_test <- X_m1 %>% 
  filter(Month >= min_date_test & Month < max_date_test) %>% 
  select(-Month)
y_m1_test <- y_m1 %>% 
  filter(Month >= min_date_test & Month < max_date_test) %>% 
  select(gdp)
X_m1_test <- as.matrix(X_m1_test)
y_m1_test <- as.matrix(y_m1_test)





# Model 2 (Second month of a quarter)

source("functions/fun_model_m2.R")

# Model 3 (Third month of a quarter)

source("functions/fun_model_m3.R")








