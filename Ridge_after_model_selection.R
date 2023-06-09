## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)

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

source("fun_preselection.R")
source("fun_z_scores.R")

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

# Period 1: Recession - trainings sample: 2005Q1-2007Q3 ------------------------

gdp_p1 <- gdp %>% 
  filter(Quarter >= "2005-03-01" & Quarter < "2007-12-01") %>% 
  select(gdp)

gtd_pre_p1 <- gtd_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

ip_pre_p1 <- ip_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")

esi_pre_p1 <- esi_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2007Q3")
  
preselection_p1 <- preselection(gdp_p1, gtd_pre_p1, esi_pre_p1, ip_pre_p1)

gtd_choice_p1 <- preselection_p1 %>% 
  mutate(tau = case_when(t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
                         t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
                         t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
                         t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
                         t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
                         t_stat >= 2.81 ~ 0.005)) %>% 
  filter(!is.na(tau))

# Period 2: Cyclical Stability - trainings sample: 2005Q1-2013Q3 ---------------

gdp_p2 <- gdp %>% 
  filter(Quarter >= "2005-03-01" & Quarter < "2013-12-01") %>% 
  select(gdp)

gtd_pre_p2 <- gtd_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

ip_pre_p2 <- ip_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

esi_pre_p2 <- esi_pre %>% 
  filter(quarter_average >= "2005Q1" & quarter_average <= "2013Q3")

preselection_p2 <- preselection(gdp_p2, gtd_pre_p2, esi_pre_p2, ip_pre_p2)

gtd_choice_p2 <- preselection_p2 %>% 
  mutate(tau = case_when(t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
                         t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
                         t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
                         t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
                         t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
                         t_stat >= 2.81 ~ 0.005)) %>% 
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

preselection_p3 <- preselection(gdp_p3, gtd_pre_p3, esi_pre_p3, ip_pre_p3)

gtd_choice_p3 <- preselection_p3 %>% 
  mutate(tau = case_when(t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
                         t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
                         t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
                         t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
                         t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
                         t_stat >= 2.81 ~ 0.005)) %>% 
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

preselection_p4 <- preselection(gdp_p4, gtd_pre_p4, esi_pre_p4, ip_pre_p4)

gtd_choice_p4 <- preselection_p4 %>% 
  mutate(tau = case_when(t_stat >= 1.28 & t_stat < 1.645 ~ 0.2,
                         t_stat >= 1.645 & t_stat < 1.96 ~ 0.1,
                         t_stat >= 1.96 & t_stat < 2.24 ~ 0.05,
                         t_stat >= 2.24 & t_stat < 2.576 ~ 0.025,
                         t_stat >= 2.576 & t_stat < 2.81 ~ 0.01,
                         t_stat >= 2.81 ~ 0.005)) %>% 
  filter(!is.na(tau))




