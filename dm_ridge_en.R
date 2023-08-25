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

# testing

test_p1_m1 <- dm.test(ridge_p1$M1,
                      oos_error_p1_en$M1,
                      alternative = "two.sided",
                      h = 2,
                      power = 2,
                      varestimator = "acf"
                      )









