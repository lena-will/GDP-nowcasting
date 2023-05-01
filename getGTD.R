## Housekeeping 
library(tidyverse)
library(gtrendsR)

setwd("/Users/lena/Git/GDP-nowcasting")

## GTD categories

data("categories")
categories <- categories %>% 
  distinct(.keep_all = TRUE)

## Get data

category_id <- categories$id[2:nrow(categories)] # must start at second row, as category 0 ("All categories") should not be used.

GTD <- NULL
counter <- 1

for(ii in category_id) {
  raw_data <- gtrends(
    geo = "US",
    time = "all",
    gprop = "web",
    category = ii,
    onlyInterest = TRUE
  )$interest_over_time
  raw_data$category <- as.character(raw_data$category)
  GTD_tmp <- raw_data %>%
    select(-c(time, geo, gprop)) %>%
    rename(id = category) %>%
    left_join(categories, by = "id") %>%
    select(-id) %>% 
    mutate(hits = replace(hits, hits == "<1", 0)) %>% 
    mutate(hits = as.integer(hits))
  GTD <- bind_rows(GTD, GTD_tmp)
  Sys.sleep(runif(1, min = 0.5, max = 2))
  print(counter)
  counter = counter + 1
}

GTD <- GTD %>% 
  pivot_wider(names_from = name, values_from = hits)

write.csv(GTD, "/Users/lena/Git/GDP-nowcasting/gtd_categories.csv", row.names = FALSE)
