## Housekeeping
library(tidyverse)

## Macroeconomic data for Germany

# GDP QoQ growth

gdp_growth <- readxl::read_xlsx("/Users/lena/Git/GDP-nowcasting/macro_data.xlsx", sheet = "gdp growth")

gdp_growth <- gdp_growth %>% 
  filter(!is.na(Quarter))

# IP index MoM growth

ip_index <- readxl::read_xlsx("/Users/lena/Git/GDP-nowcasting/macro_data.xlsx", sheet = "IP index")

# Sentiment Index European Commission

esi_de <- readxl::read_xlsx("/Users/lena/Git/GDP-nowcasting/macro_data.xlsx", sheet = "DE ESI")



  
  

