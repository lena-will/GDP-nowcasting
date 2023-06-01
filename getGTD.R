## Housekeeping 
library(tidyverse)
library(gtrendsR)

setwd("/Users/lena/Git/GDP-nowcasting")

## GTD  all categories

data("categories")
categories <- categories %>% 
  distinct(.keep_all = TRUE)

## Get data - all categories

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

## Save data - all categories

write.csv(GTD, "/Users/lena/Git/GDP-nowcasting/gtd_categories.csv", row.names = FALSE)

df_gtd <- data.table::fread("/Users/lena/Git/GDP-nowcasting/gtd_categories.csv") #testing

## -----------------------------------------------------------------------------
## Get data - summary categories

categories_summary <- read.csv("/Users/lena/Git/GDP-nowcasting/summary_categories.csv")
categories_summary <- subset(categories, categories$name %in% categories_summary$keyword) %>% 
  rename(keyword = name)

category_summary_id <- categories_summary$id

GTD <- NULL
counter <- 1

for(ii in category_summary_id) {
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

write.csv(GTD, "/Users/lena/Git/GDP-nowcasting/gtd_categories_summary.csv", row.names = FALSE)

## Data for Germany 

categories_summary <- read.csv("/Users/lena/Git/GDP-nowcasting/summary_categories.csv")
categories_summary <- subset(categories, categories$name %in% categories_summary$keyword) %>% 
  rename(keyword = name)

category_summary_id <- categories_summary$id


GTD <- NULL
counter <- 1

for(ii in category_summary_id) {
  raw_data <- gtrends(
    geo = "DE",
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

write.csv(GTD, "/Users/lena/Git/GDP-nowcasting/gtd_germany.csv", row.names = FALSE)








