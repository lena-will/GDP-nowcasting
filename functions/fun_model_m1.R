model_m1 <-
  function(esi_bridge,
           gtd_period,
           y_ridge,
           min_date_train,
           max_date_train,
           min_date_test,
           max_date_test) {
    X_m1 <- esi_bridge %>%
      select(-ESI) %>%
      rename(esi = esi_b) %>%
      left_join(gtd_period, by = "Month") %>%
      filter(month(Month) == 1 |
               month(Month) == 4 | month(Month) == 7 | month(Month) == 10)
    
    y_m1 <- y_ridge %>%
      filter(month(Month) == 1 |
               month(Month) == 4 | month(Month) == 7 | month(Month) == 10)
    
    # training
    
    X_m1_train <- X_m1 %>%
      filter(Month >= min_date_train & Month < max_date_train) %>%
      select(-Month)
    y_m1_train <- y_m1 %>%
      filter(Month >= min_date_train & Month < max_date_train) %>%
      select(gdp)
    X_m1_train <- as.matrix(X_m1_train)
    y_m1_train <- as.matrix(y_m1_train)
    
    alpha_ini <- as.matrix(seq(
      from = 0.01,
      to = 2,
      length.out = 100
    ))
    n <- nrow(y_m1)
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
    
    ident_test <- diag(ncol(X_m1_test))
    beta_hat_oos <-
      solve(t(X_m1_test) %*% X_m1_test + alpha_min * ident_test) %*% t(X_m1_test) %*%
      y_m1_test
    y_pred <- X_m1_train %*% beta_hat_pls
    
    
  }