## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(expm)

## Set parameters --------------------------------------------------------------

N = 150 # total number of variables in X 
t = 100 # Sample size
s = 105 # sparsity (number of variables X related to the common factor)
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

## Beta specifications ---------------------------------------------------------

# option 1
beta_coeff_1 <- as.matrix(rnorm(N))

# option 2
beta_2_prep1 <- as.matrix(rnorm(s))
beta_2_prep2 <- matrix(0, nrow = (N-s), ncol = 1)
beta_coeff_2 <- rbind(beta_2_prep1, beta_2_prep2)

# option 3
# beta_3_prep1 <- as.matrix(rnorm(s))
# vector_prep <- as.matrix(c((s+1):N)^2)
# beta_3_prep2 <- solve(vector_prep * runif(N-s))
# beta_coeff_3 <- rbind(beta_3_prep1 beta_3_prep2)

## Psi specifications ----------------------------------------------------------

# option 1:
psi_1 <- diag(N)
psi_1_sqrtm <- sqrtm(psi_1)
eigen_psi_1 <- eigen(psi_1)$values
eigen_psi_1_sum <- sum(eigen_psi_1)

# option 2:
A <- matrix(NA, nrow = N, ncol = N)
seq <- seq(from = 1, to = N, by = 1)
for(ii in 1:N){
  A[ii, ] = seq
}
A_ini <- t(A)
B <- A_ini - t(A_ini)
B_abs <- abs(B)
psi_2 <- 0.5^B_abs
psi_2_sqrtm <- sqrtm(psi_2)
eigen_psi_2 <- eigen(psi_2)$values
eigen_psi_2_order <- sort(eigen_psi_2, decreasing = TRUE)
eigen_psi_2_sum <- sum(eigen_psi_2_order > 1e-03)

## Simulation

v <- as.matrix(rnorm(t))
z <- matrix(rnorm(t), t, 2) %*% sqrtm(var_z)
tmp <- matrix(rnorm(t), t, N)
x <- z %*% delta_2 + tmp %*% psi_1_sqrtm
y <- z %*% gamma + x %*% beta_coeff_1 + sqrt(sigma2_v) * v


for (m in 1:MC) {
  # Generate Data
  v <- as.matrix()
}
