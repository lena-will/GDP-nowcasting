## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)
library(lubridate)
library(forecast)

## Load oos errors from elastic net and Ridge after model selection ------------
 
oos_error_p1_ridge <- readRDS("tests/oos_errors_p1_ridge.RDS")
oos_error_p2_ridge <- readRDS("tests/oos_errors_p2_ridge.RDS")
oos_error_p3_ridge <- readRDS("tests/oos_errors_p3_ridge.RDS")
oos_error_p4_ridge <- readRDS("tests/oos_errors_p4_ridge.RDS")

rmsfe_p1_ridge <- readRDS("tables/results_p1.RDS")
rmsfe_p2_ridge <- readRDS("tables/results_p2.RDS")
rmsfe_p3_ridge <- readRDS("tables/results_p3.RDS")
rmsfe_p4_ridge <- readRDS("tables/results_p4.RDS")

oos_error_p1_en <- readRDS("tests/oos_errors_p1.RDS")
oos_error_p2_en <- readRDS("tests/oos_errors_p2.RDS")
oos_error_p3_en <- readRDS("tests/oos_errors_p3.RDS")
oos_error_p4_en <- readRDS("tests/oos_errors_p4.RDS")

## DM test Ridge vs Elastic net ------------------------------------------------

# get min rmsfe and corresponding tau for Ridge results

# Period 1
index <- which(rmsfe_p1_ridge$M1 == min(rmsfe_p1_ridge$M1))
m1_p1_tau <- rmsfe_p1_ridge[index, 1]

index <- which(rmsfe_p1_ridge$M2 == min(rmsfe_p1_ridge$M2))
m2_p1_tau <- rmsfe_p1_ridge[index, 1]

index <- which(rmsfe_p1_ridge$M3 == min(rmsfe_p1_ridge$M3))
m3_p1_tau <- rmsfe_p1_ridge[index, 1]

min_p1 <- c(m1_p1_tau, m2_p1_tau, m3_p1_tau)
min_p1 <- paste(min_p1, c("M1", "M2", "M3"), sep = "_")

ridge_p1 <- oos_error_p1_ridge %>% 
  select(any_of(min_p1))
colnames(ridge_p1) <- c("M1", "M2", "M3")

rm(index, m1_p1_tau, m2_p1_tau, m3_p1_tau )

# Period 2

index <- which(rmsfe_p2_ridge$M1 == min(rmsfe_p2_ridge$M1))
m1_p2_tau <- rmsfe_p2_ridge[index, 1]

index <- which(rmsfe_p2_ridge$M2 == min(rmsfe_p2_ridge$M2))
m2_p2_tau <- rmsfe_p2_ridge[index, 1]

index <- which(rmsfe_p2_ridge$M3 == min(rmsfe_p2_ridge$M3))
m3_p2_tau <- rmsfe_p2_ridge[index, 1]

min_p2 <- c(m1_p2_tau, m2_p2_tau, m3_p2_tau)
min_p2 <- paste(min_p2, c("M1", "M2", "M3"), sep = "_")

ridge_p2 <- oos_error_p2_ridge %>% 
  select(any_of(min_p2))
colnames(ridge_p2) <- c("M1", "M2", "M3")

rm(index, m1_p2_tau, m2_p2_tau, m3_p2_tau)

# Period 3

index <- which(rmsfe_p3_ridge$M1 == min(rmsfe_p3_ridge$M1))
m1_p3_tau <- rmsfe_p3_ridge[index, 1]

index <- which(rmsfe_p3_ridge$M2 == min(rmsfe_p3_ridge$M2))
m2_p3_tau <- rmsfe_p3_ridge[index, 1]

index <- which(rmsfe_p3_ridge$M3 == min(rmsfe_p3_ridge$M3))
m3_p3_tau <- rmsfe_p3_ridge[index, 1]

min_p3 <- c(m1_p3_tau, m2_p3_tau, m3_p3_tau)
min_p3 <- paste(min_p3, c("M1", "M2", "M3"), sep = "_")

ridge_p3 <- oos_error_p3_ridge %>% 
  select(any_of(min_p3))
colnames(ridge_p3) <- c("M1", "M2", "M3")

rm(index, m1_p3_tau, m2_p3_tau, m3_p3_tau)

# Period 4

index <- which(rmsfe_p4_ridge$M1 == min(rmsfe_p4_ridge$M1))
m1_p4_tau <- rmsfe_p4_ridge[index, 1]

index <- which(rmsfe_p4_ridge$M2 == min(rmsfe_p4_ridge$M2))
m2_p4_tau <- rmsfe_p4_ridge[index, 1]

index <- which(rmsfe_p4_ridge$M3 == min(rmsfe_p4_ridge$M3))
m3_p4_tau <- rmsfe_p4_ridge[index, 1]

min_p4 <- c(m1_p4_tau, m2_p4_tau, m3_p4_tau)
min_p4 <- paste(min_p4, c("M1", "M2", "M3"), sep = "_")

ridge_p4 <- oos_error_p4_ridge %>% 
  select(any_of(min_p4))
colnames(ridge_p4) <- c("M1", "M2", "M3")

rm(index, m1_p4_tau, m2_p4_tau, m3_p4_tau)

## Testing ---------------------------------------------------------------------

# Period 1
test_p1_m1 <- dm.test(ridge_p1$M1,
                      oos_error_p1_en$M1,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
                      )

test_p1_m2 <- dm.test(ridge_p1$M2,
                      oos_error_p1_en$M2,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p1_m3 <- dm.test(ridge_p1$M3,
                      oos_error_p1_en$M3,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

statistic_p1 <- as.data.frame(test_p1_m1[[1]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p1_m2[[1]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p1_m3[[1]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "test statistic", .before = 1)

pvalue_p1 <- as.data.frame(test_p1_m1[[5]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p1_m2[[5]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p1_m3[[5]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "p-value", .before = 1)

test_results_p1 <- statistic_p1 %>% 
  rbind(pvalue_p1)

saveRDS(test_results_p1, "tables/test_results_p1.RDS")

# Period 2
test_p2_m1 <- dm.test(ridge_p2$M1,
                      oos_error_p2_en$M1,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p2_m2 <- dm.test(ridge_p2$M2,
                      oos_error_p2_en$M2,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p2_m3 <- dm.test(ridge_p2$M3,
                      oos_error_p2_en$M3,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

statistic_p2 <- as.data.frame(test_p2_m1[[1]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p2_m2[[1]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p2_m3[[1]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "test statistic", .before = 1)

pvalue_p2 <- as.data.frame(test_p2_m1[[5]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p2_m2[[5]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p2_m3[[5]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "p-value", .before = 1)

test_results_p2 <- statistic_p2 %>% 
  rbind(pvalue_p2)

saveRDS(test_results_p2, "tables/test_results_p2.RDS")

# Period 3
test_p3_m1 <- dm.test(ridge_p3$M1,
                      oos_error_p3_en$M1,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p3_m2 <- dm.test(ridge_p3$M2,
                      oos_error_p3_en$M2,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p3_m3 <- dm.test(ridge_p3$M3,
                      oos_error_p3_en$M3,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

statistic_p3 <- as.data.frame(test_p3_m1[[1]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p3_m2[[1]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p3_m3[[1]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "test statistic", .before = 1)

pvalue_p3 <- as.data.frame(test_p3_m1[[5]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p3_m2[[5]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p3_m3[[5]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "p-value", .before = 1)

test_results_p3 <- statistic_p3 %>% 
  rbind(pvalue_p3)

saveRDS(test_results_p3, "tables/test_results_p3.RDS")

# Period 4
test_p4_m1 <- dm.test(ridge_p4$M1,
                      oos_error_p4_en$M1,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p4_m2 <- dm.test(ridge_p4$M2,
                      oos_error_p4_en$M2,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

test_p4_m3 <- dm.test(ridge_p4$M3,
                      oos_error_p4_en$M3,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
)

statistic_p4 <- as.data.frame(test_p4_m1[[1]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p4_m2[[1]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p4_m3[[1]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "test statistic", .before = 1)

pvalue_p4 <- as.data.frame(test_p4_m1[[5]]) %>% 
  rename(M1 = 1) %>% 
  cbind(as.data.frame(test_p4_m2[[5]])) %>% 
  rename(M2 = 2) %>%
  cbind(as.data.frame(test_p4_m3[[5]])) %>% 
  rename(M3 = 3) %>% 
  mutate(" " = "p-value", .before = 1)

test_results_p4 <- statistic_p4 %>% 
  rbind(pvalue_p4)

saveRDS(test_results_p4, "tables/test_results_p4.RDS")







