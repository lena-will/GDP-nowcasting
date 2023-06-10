model_m2 <- function(esi_bridge, gtd_period, y_ridge, min_date, max_date){
  X_m2 <- esi_bridge %>% 
    select(-ESI) %>% 
    rename(esi = esi_b) %>% 
    left_join(gtd_period, by = "Month") %>% 
    filter(month(Month) == 2 | month(Month) == 5 | month(Month) == 8 | month(Month) == 11 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(-Month)
  y_m2 <- y_ridge %>% 
    filter(month(Month) == 2 | month(Month) == 5 | month(Month) == 8 | month(Month) == 11 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(gdp)
  X_m2 <- as.matrix(X_m1)
  y_m2 <- as.matrix(y_m1)
}