model_m3 <- function(esi_bridge, gtd_period, y_ridge, min_date, max_date){
  X_m3 <- esi_bridge %>% 
    select(-ESI) %>% 
    rename(esi = esi_b) %>%
    left_join(ip_bridge, by = "Month") %>% 
    select(-c(ip_abs, ip_mom)) %>% 
    rename(ip = ip_b) %>% 
    left_join(gtd_data, by = "Month") %>% 
    filter(month(Month) == 3 | month(Month) == 6 | month(Month) == 9 | month(Month) == 12 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(-Month)
  y_m3 <- y_ridge %>% 
    filter(month(Month) == 3 | month(Month) == 6 | month(Month) == 9 | month(Month) == 12 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(gdp)
  X_m3 <- as.matrix(X_m1)
  y_m3 <- as.matrix(y_m1)
}