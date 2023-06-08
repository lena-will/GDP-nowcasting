## Housekeeping
library(tidyverse)
library(zoo)

## Load data

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

## Preselection

gtd_pre <- gtd_data %>%
  group_by(date = format(as.yearqtr(date, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = date) %>%
  filter(quarter_average != "2023Q2")

ip_pre <- ip %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>% 
  select(-ip_abs)

esi_pre <- esi %>%
  group_by(Month = format(as.yearqtr(Month, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean) %>%
  rename(quarter_average = Month) %>%
  filter(quarter_average != "2023Q2")

z_scores <- function(x){
  (x-mean(x))/sd(x)
}

t_stat <- NULL
y <- gdp$gdp
for (ii in 2:length(gtd_pre)){
  gtd_keyword <- gtd_pre[,c(1,ii)]
  X <- esi_pre %>% 
    left_join(ip_pre) %>% 
    left_join(gtd_keyword) %>% 
    select(-quarter_average)
  X <- as.data.frame(apply(X, 2, z_scores))
  X <- X %>% 
    mutate(intercept = 1, .before = ESI)
  X <- as.matrix(X)
  beta_hat <- solve(t(X)%*%X)%*%t(X)%*%y
  sigma_hat <- t(y - X%*%beta_hat)%*%(y - X%*%beta_hat)/(nrow(X) - ncol(X))
  inv_X <- solve(t(X)%*%X)
  t_stat_tmp <- abs(beta_hat[4,1]/sqrt(sigma_hat%*%inv_X[4,4]))
  t_stat_tmp <- as.data.frame(t_stat_tmp) 
  t_stat <- bind_rows(t_stat, t_stat_tmp)
}

t_stat_ordered <- t_stat %>% 
  mutate(keyword = colnames(gtd_pre)[2:ncol(gtd_pre)]) %>% 
  rename(t_stat = V1) %>% 
  arrange(desc(t_stat))
