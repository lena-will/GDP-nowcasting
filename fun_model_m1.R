model_m1 <- function(esi_bridge, gtd_period, y_ridge, min_date, max_date){
  X_m1 <- esi_bridge %>% 
    select(-ESI) %>% 
    rename(esi = esi_b) %>% 
    left_join(gtd_period, by = "Month") %>% 
    filter(month(Month) == 1 | month(Month) == 4 | month(Month) == 7 | month(Month) == 10 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(-Month)
  y_m1 <- y_ridge %>% 
    filter(month(Month) == 1 | month(Month) == 4 | month(Month) == 7 | month(Month) == 10 ) %>% 
    filter(Month >= min_date & Month < max_date) %>% 
    select(gdp)
  X_m1 <- as.matrix(X_m1)
  y_m1 <- as.matrix(y_m1)
}