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

## Testing

test <- dm.test(oos_error_p1_ridge$`0.005_M1`,
                oos_error_p1_ridge$`0.01_M1`,
                alternative = "two.sided",
                h = 2,
                power = 2,
                varestimator = "bartlett")
