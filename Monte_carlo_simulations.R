## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(expm)

## Set parameters --------------------------------------------------------------

N = 150 # total number of variables in X
t = 100 # Sample size
s = 105 # sparsity (number of variables X related to the common factor)
train = 75 # n of the trainings sample
mc = 500  # number of iterations
matrix_ones <- matrix(1, nrow = 2, ncol = N)
delta_2 = 0.2 * matrix_ones
delta_8 = 0.8 * matrix_ones
sigma2_v = 1
var_z <- rbind(c(1, 0.3), c(0.3, 1))
gamma <- as.matrix(c(1, 2))

## Beta specifications ---------------------------------------------------------

beta_prep1 <- as.matrix(rnorm(s))
beta_prep2 <- matrix(0, nrow = (N - s), ncol = 1)
beta_coeff_2 <- rbind(beta_prep1, beta_prep2)

## Psi specifications ----------------------------------------------------------

# option 1:
psi_1 <- diag(N)
psi_1_sqrtm <- sqrtm(psi_1)
#eigen_psi_1 <- eigen(psi_1)$values
#eigen_psi_1_sum <- sum(eigen_psi_1)

# option 2:
A <- matrix(NA, nrow = N, ncol = N)
seq <- seq(from = 1, to = N, by = 1)
for (ii in 1:N) {
  A[ii,] = seq
}
A_ini <- t(A)
B <- A_ini - t(A_ini)
B_abs <- abs(B)
psi_2 <- 0.5 ^ B_abs
psi_2_sqrtm <- sqrtm(psi_2)
#eigen_psi_2 <- eigen(psi_2)$values
#eigen_psi_2_order <- sort(eigen_psi_2, decreasing = TRUE)
#eigen_psi_2_sum <- sum(eigen_psi_2_order > 1e-03)

## Simulation

v <- as.matrix(rnorm(t))
z <- matrix(rnorm(t * 2), t, 2) %*% sqrtm(var_z)
tmp <- matrix(rnorm(t * N), t, N)
x <- z %*% delta_2 + tmp %*% psi_2_sqrtm
y <- z %*% gamma + x %*% beta_coeff_2 + sqrt(sigma2_v) * v
x_for_tau <- as.data.frame(x)
z_for_loop <- as.data.frame(z)

##Preselection

t_stat <- NULL
for (ii in 1:ncol(x)) {
  X_tmp <- cbind(z, x[, ii])
  X_scaled <- scale(X_tmp, center = TRUE, scale = TRUE)
  X <- as.data.frame(X_scaled)
  X <- X %>%
    mutate(intercept = 1, .before = V1)
  X <- as.matrix(X)
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  sigma_hat <-
    t(y - X %*% beta_hat) %*% (y - X %*% beta_hat) / (nrow(X) - ncol(X))
  inv_X <- solve(t(X) %*% X)
  t_stat_tmp <-
    abs(beta_hat[4, 1] / sqrt(sigma_hat %*% inv_X[4, 4]))
  t_stat_tmp <- as.data.frame(t_stat_tmp)
  t_stat <- bind_rows(t_stat, t_stat_tmp)
}

t_stat_prep <- t_stat %>%
  mutate(column = colnames(x_for_tau)) %>%
  rename(t_stat = V1) %>%
  arrange(desc(t_stat))

category_choice <- t_stat_prep %>%
  mutate(
    tau = case_when(
      t_stat < 0.8416 ~ 1.0,
      t_stat >= 0.8416 & t_stat < 1.2816 ~ 0.2,
      t_stat >= 1.2816 & t_stat < 1.6449 ~ 0.1,
      t_stat >= 1.6449 & t_stat < 1.96 ~ 0.05,
      t_stat >= 1.96 & t_stat < 2.3263 ~ 0.025,
      t_stat >= 2.3263 & t_stat < 2.5758 ~ 0.01,
      t_stat >= 2.5758 ~ 0.005
    )
  )

tau_options <- unique(category_choice$tau)
msfe <- c()
oos_error <- c()
mse <- c()
mean_tmp <- c()


for(tau_loop in 1:length(tau_options)){
  in_sample <- NULL
  print(tau_options[tau_loop])
  which_category <- category_choice %>% 
    filter(tau <= tau_options[tau_loop])
  relevant_cols <- which_category$column
  
  selection_x <- x_for_tau %>% 
    select(any_of(relevant_cols))
  
  x_train_ini <- selection_x %>% 
    bind_cols(z_for_loop)
  
  y_train_ini <- as.data.frame(y)
  
  for(window in 75:(t-1)){
    x_train <- x_train_ini[1:window, ]
    mean <- apply(x_train, 2, mean)
    sd   <- apply(x_train, 2, sd)
    x_train_z <- scale(x_train, center=mean, scale=sd)
    
    y_train <- as.matrix(y_train_ini[1:window, ])
    
    alpha_ini <- as.matrix(seq(
      from = 0.01,
      to = 2,
      length.out = 100
    ))
    n <- nrow(y_train)
    gcv <- c()
    
    for (ii in 1:length(alpha_ini)) {
      ident <- diag(ncol(x_train_z))
      alpha <- alpha_ini[ii] * n
      beta_hat_pls <-
        solve(t(x_train_z) %*% x_train_z + alpha * ident) %*% t(x_train_z) %*%
        y_train
      y_hat_pls <- x_train_z %*% beta_hat_pls
      gcv[ii] <-
        (1 / n) %*% t(y_train - y_hat_pls) %*% (y_train - y_hat_pls) / (1 -
                                                                          sum(diag(
                                                                            x_train_z %*% solve(t(x_train_z) %*% x_train_z + alpha * ident) %*% t(x_train_z)
                                                                          )) * (1 / n)) ^ 2
    }
    
    gcv_min <- which(gcv == min(gcv))
    alpha_min <- alpha_ini[gcv_min] * n
    beta_hat_opt <-
      solve(t(x_train_z) %*% x_train_z + alpha_min * ident) %*% t(x_train_z) %*%
      y_train
    
    y_in_sample <- x_train_z %*% beta_hat_opt
    
    x_test <- x_train_ini[window+1, ]
    x_test_z <- scale(x_test, center = mean, scale = sd)
    
    y_test <- as.matrix(y_train_ini[window+1, ])
    y_test <- as.numeric(y_test)
    
    y_pred <- x_test_z %*% beta_hat_opt
    y_pred <- as.numeric(y_pred)
    
    in_sample_tmp <- as.data.frame(y_in_sample - y_train)
    #print(in_sample_tmp)
    in_sample <- in_sample %>% 
      bind_rows(in_sample_tmp)
    mean_tmp[window-74] <- mean(in_sample[,1])

    oos_error[window-74] <- (y_pred - y_test)
    
  }
  
  mse[tau_loop] <- mean_tmp
  msfe[tau_loop] <- mean(oos_error ^ 2)
  rm(in_sample)
}


