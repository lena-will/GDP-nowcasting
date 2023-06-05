## Housekeeping
library(tidyverse)

## Load data

gtd_data <- read_csv("Data prep/gtd_germany.csv")
gdp <- readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "gdp growth") %>% 
  filter(Quarter >= "2004-01-01")
ip <- readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "IP index") %>% 
  filter(Month >= "2004-01-01")
esi <- readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "DE ESI") %>% 
  filter(Month >= "2004-01-01")



  
