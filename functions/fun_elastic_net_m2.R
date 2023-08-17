elastic_net_m2 <-
  function(y_bridge,
           esi_bridge,
           gtd_bridge,
           min_train,
           min_test,
           max_test) {
    y_m2 <- y_bridge %>%
      mutate(Month = as.Date(Month)) %>%
      filter(month(Month) == 2 |
               month(Month) == 5 |
               month(Month) == 8 | month(Month) == 11)
    
    X_m2 <- esi_bridge %>%
      mutate(Month = as.Date(Month)) %>%
      select(-ESI) %>%
      rename(esi = esi_b) %>%
      left_join(gtd_bridge, by = "Month") %>%
      filter(month(Month) == 2 |
               month(Month) == 5 |
               month(Month) == 8 | month(Month) == 11)
    
    window <- X_m2 %>%
      select(Month) %>%
      filter(Month >= min_test & Month <= max_test)
    window <- as.matrix(window)
    
    predictions <- c()
    oos_error <- c()
    oos_error_all <- NULL
    rmsfe <- NULL
    
    alpha_ini <- as.matrix(seq(
      from = 0.1,
      to = 0.9,
      length.out = 9
    ))
    
    for (ii in 1:nrow(alpha_ini)) {
      for (month in 1:(nrow(window) - 2)) {
        y_m2_train <- y_m2 %>%
          filter(Month >= min_train & Month <= window[month]) %>%
          select(gdp)
        y_m2_train <- as.matrix(y_m2_train)
        X_m2_train <- X_m2 %>%
          filter(Month >= min_train & Month <= window[month]) %>%
          select(-Month)
        
        mean_x <- apply(X_m2_train, 2, mean)
        sd_x <- apply(X_m2_train, 2, sd)
        
        X_m2_train_z <- scale(X_m2_train, center = mean_x, scale = sd_x)
        
        fit_en <-
          cv.glmnet(
            X_m2_train_z,
            y_m2_train,
            alpha = alpha_ini[ii],
            type.measure = "mse",
            nfolds = 10,
            family = "gaussian"
          )
        
        X_m2_test <- X_m2 %>%
          filter(Month == window[month + 2]) %>%
          select(-Month)
        X_m2_test_z <- scale(X_m2_test, center = mean_x, scale = sd_x)
        
        y_m2_test <- y_m2 %>%
          filter(Month == window[month + 2]) %>%
          select(gdp)
        y_m2_test <- as.matrix(y_m2_test)
        
        predict_en <-
          predict(fit_en, s = fit_en$lambda.1se, newx = X_m2_test_z)
        
        predictions[month] <- predict_en
        
        oos_error[month] <- (predict_en - y_m2_test)
        
      }
      oos_error_prep <- t(oos_error)
      oos_error_all <- oos_error_all %>% 
        rbind(oos_error_prep)
      rmsfe[ii] <- sqrt(mean(oos_error ^ 2))
    }
    min_rmsfe <- min(rmsfe)
    min_index <- which(rmsfe == min(rmsfe))
    oos_error_min <- oos_error_all[min_index[1], ]
    results <- list(min_rmsfe, oos_error_all, oos_error_min, min_index)
  }