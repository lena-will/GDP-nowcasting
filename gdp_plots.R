## Housekeeping ----------------------------------------------------------------
library(tidyverse)
library(zoo)

## Load data -------------------------------------------------------------------

gdp <-
  readxl::read_xlsx("Data prep/macro_data.xlsx", sheet = "gdp growth") %>%
  filter(Quarter >= "2004-01-01") %>% 
  rename(gdp = `QoQ growth`)

## GDP plots: QoQ growth -------------------------------------------------------

p1_plot <- ggplot(gdp %>%
                    filter(Quarter >= "2007-03-01" &
                             Quarter <= "2009-12-01"),
                  aes(x = Quarter, y = gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  geom_hline(yintercept = 0.0, color = "grey") +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2008-01-01"), xmax = as.POSIXct("2009-04-01"),
           ymin = -Inf, ymax = Inf)
  

p2_plot <- ggplot(gdp %>%
                    filter(Quarter >= "2013-03-01" &
                             Quarter <= "2016-12-01"),
                  aes(x = Quarter, y = gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  geom_hline(yintercept = 0.0, color = "grey")

p3_plot <- ggplot(gdp %>%
                    filter(Quarter >= "2016-03-01" &
                             Quarter <= "2019-12-01"),
                  aes(x = Quarter, y = gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  geom_hline(yintercept = 0.0, color = "grey")

p4_plot <- ggplot(gdp %>%
                    filter(Quarter >= "2018-03-01" &
                             Quarter <= "2021-12-01"),
                  aes(x = Quarter, y = gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  geom_hline(yintercept = 0.0, color = "grey") +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2019-12-01"), xmax = as.POSIXct("2020-06-01"),
           ymin = -Inf, ymax = Inf)

## GDP plots: log --------------------------------------------------------------


p1_plot_log <- ggplot(gdp %>%
                    filter(Quarter >= "2007-03-01" &
                             Quarter <= "2009-12-01"),
                  aes(x = Quarter, y = log_gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2008-01-01"), xmax = as.POSIXct("2009-04-01"),
           ymin = -Inf, ymax = Inf)


p2_plot_log <- ggplot(gdp %>%
                    filter(Quarter >= "2013-03-01" &
                             Quarter <= "2016-12-01"),
                  aes(x = Quarter, y = log_gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light()

p3_plot_log <- ggplot(gdp %>%
                    filter(Quarter >= "2016-03-01" &
                             Quarter <= "2019-12-01"),
                  aes(x = Quarter, y = log_gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light()

p4_plot_log <- ggplot(gdp %>%
                    filter(Quarter >= "2018-03-01" &
                             Quarter <= "2021-12-01"),
                  aes(x = Quarter, y = log_gdp)) +
  geom_line() +
  scale_x_datetime(labels = function(x) zoo::format.yearqtr(x, "%YQ%q"), breaks = "6 months") +
  theme_light() +
  annotate("rect", fill = "grey", alpha = 0.5, 
           xmin = as.POSIXct("2019-12-01"), xmax = as.POSIXct("2020-06-01"),
           ymin = -Inf, ymax = Inf)


  
