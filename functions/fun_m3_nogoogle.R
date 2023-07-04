m3_nogoogle <- function(esi_bridge,
                        ip_bridge,
                        y_bridge,
                        min_date_train,
                        min_date_test,
                        max_date_test) {
  X_m1 <- esi_bridge %>%
    mutate(Month = as.Date(Month)) %>%
    select(-ESI) %>%
    rename(esi = esi_b) %>%
    left_join(ip_bridge) %>%
    select(-c(ip_abs, ip_mom)) %>%
    rename(ip = ip_b) %>%
    filter(month(Month) == 3 |
             month(Month) == 6 |
             month(Month) == 9 | month(Month) == 12)
  
  y_m1 <- y_bridge %>%
    mutate(Month = as.Date(Month)) %>%
    filter(month(Month) == 3 |
             month(Month) == 6 |
             month(Month) == 9 | month(Month) == 12)
  
  # training
  
  window <- X_m1 %>%
    select(Month) %>%
    filter(Month >= min_date_test & Month <= max_date_test)
  window <- as.matrix(window)
  
  oos_error <- c()
  
  
  for (month in 1:(nrow(window) - 1)) {
    X_m1_train <- X_m1 %>%
      filter(Month >= min_date_train & Month <= window[month]) %>%
      select(-Month)
    mean <- apply(X_m1_train, 2, mean)
    sd   <- apply(X_m1_train, 2, sd)
    X_m1_train <- scale(X_m1_train, center = mean, scale = sd)
    y_m1_train <- y_m1 %>%
      filter(Month >= min_date_train & Month <= window[month]) %>%
      select(gdp)
    X_m1_train <- as.matrix(X_m1_train)
    y_m1_train <- as.matrix(y_m1_train)
    
    alpha_ini <- as.matrix(seq(
      from = 0.01,
      to = 2,
      length.out = 100
    ))
    n <- nrow(y_m1_train)
    gcv <- c()
    
    for (ii in 1:length(alpha_ini)) {
      ident <- diag(ncol(X_m1_train))
      alpha <- alpha_ini[ii] * n
      beta_hat_pls <-
        solve(t(X_m1_train) %*% X_m1_train + alpha * ident) %*% t(X_m1_train) %*%
        y_m1_train
      y_hat_pls <- X_m1_train %*% beta_hat_pls
      gcv[ii] <-
        (1 / n) %*% t(y_m1_train - y_hat_pls) %*% (y_m1_train - y_hat_pls) / (1 -
                                                                                sum(diag(
                                                                                  X_m1_train %*% solve(t(X_m1_train) %*% X_m1_train + alpha * ident) %*% t(X_m1_train)
                                                                                )) * (1 / n)) ^ 2
    }
    
    gcv_min <- which(gcv == min(gcv))
    alpha_min <- alpha_ini[gcv_min] * n
    
    beta_hat_opt <-
      solve(t(X_m1_train) %*% X_m1_train + alpha_min * ident) %*% t(X_m1_train) %*%
      y_m1_train
    
    X_m1_test <- X_m1 %>%
      filter(Month == window[month + 1]) %>%
      select(-Month)
    X_m1_test <- scale(X_m1_test, center = mean, scale = sd)
    y_m1_test <- y_m1 %>%
      filter(Month == window[month + 1]) %>%
      select(gdp)
    X_m1_test <- as.matrix(X_m1_test)
    y_m1_test <- as.matrix(y_m1_test)
    
    y_pred <- X_m1_test %*% beta_hat_opt
    
    oos_error[month] <- (y_pred - y_m1_test)
    
  }
  
  rmsfe <- sqrt(mean(oos_error ^ 2))
  rmsfe_results <- as.data.frame(rmsfe)
}
