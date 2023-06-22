## Housekeeping
library(tidyverse)

## Set parameters

N = 150 # total number of variables in X 
t = 100 # Sample size
s = 105 # sparsity (number of variables X related tothe common factor)
train = 75 # n of the trainings sample 
r = 3 # number of factors
h = 0 # forecasting lag (nowcasting)
gap = 1 # period between train and test sample
mc = 500  # number of iterations
matrix_ones <- matrix(1, nrow = 2, ncol = N)
delta_2 = 0.2*matrix_ones
delta_8 = 0.8*matrix_ones
sigma2_v = 1
var_z <- rbind(c(1, 0.3), c(0.3, 1))
gamma <- as.matrix(c(1,2))
# option 1: beta specification
beta_coeff_1 <- as.matrix(rnorm(N))
# option 2: beta specification
beta_2_prep1 <- as.matrix(rnorm(s))
beta_2_prep2 <- matrix(0, nrow = (N-s), ncol = 1)
beta_coeff_2 <- rbind(beta_2_prep1, beta_2_prep2)
# option 3: beta specification
beta_3_prep1 <- as.matrix(rnorm(s))
vector_prep <- as.matrix(c((s+1):N)^2)
beta_3_prep2 <- vector_prep * runif(N-s)
beta_coeff_3 <- rbind(beta_2_prep, beta_2_prep2)
# Psi option 1:
psi_1 <- diag(N)
# Psi option 2:


