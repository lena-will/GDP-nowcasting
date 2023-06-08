preselection <- function(y, gtd_pre, esi_pre, ip_pre){
  t_stat <- NULL
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
    y <- as.matrix(y)
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
}