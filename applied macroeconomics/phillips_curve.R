## ----setup, message = FALSE---------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)  # <3
library(gridExtra)  # For arranging multiple grid-based plots on a page, and draw tables.
library(ggthemes)   # Extra Themes, Scales and Geoms for ggplot2
library(skimr)      # Compact and Flexible Summaries of Data
library(fredr)      # An R client for the 'Federal Reserve Economic Data' ('FRED') API


## ----setting the API key, eval = FALSE----------------------------------------------------------------------------
## fredr_set_key("your_key_here")


## ----loading the data manually------------------------------------------------------------------------------------
#' to load the data from csv run the following 2 lines of code and
#' ignore everything until the time series plots

# phillips_data_ger <- read_csv("phillips_data_ger.csv")
# uk_data <- read_csv("uk_data.csv")


## ----loading and cleaning the data, echo = FALSE------------------------------------------------------------------
#' to load the data directly from FRED using the fredr package, 

# Consumer Price Index of All Items in Germany
# seasonally adjusted, index 2015 = 100, quarterly data
cpi_ger_adj <- fredr(series_id = "DEUCPALTT01IXOBSAQ") %>%
  rename(cpi_adj = value)

# Consumer Price Index of All Items in Germany
# NOT seasonally adjusted, index 2015 = 100, quarterly data
cpi_ger_notadj <- fredr(series_id = "DEUCPIALLQINMEI") %>%
  rename(cpi_notadj = value)

# Private Final Consumption Expenditure in Germany
# seasonally adjusted, quarterly data, units: euros
pce_ger_adj <- fredr(series_id = "DEUPFCEQDSMEI") %>%
  rename(pce_adj = value)

# Producer Prices Index: Total Consumer Goods for Germany
# NOT seasonally adjusted, Index 2015 = 100, quarterly data
ppi_ger_notadj <- fredr(series_id = "PITGCG01DEQ661N") %>%
  rename(ppi_notadj = value)

# Registered Unemployment Rate for Germany
# unit: percent, seasonally adjusted, quarterly data
unemployment_ger_adj <- fredr(series_id = "LMUNRRTTDEQ156S") %>%
  rename(unemployment_rate_adj = value)

# Registered Unemployment Rate for Germany
# unit: percent, NOT seasonally adjusted, quarterly data
unemployment_ger_notadj <- fredr(series_id = "LMUNRRTTDEQ156N") %>%
  rename(unemployment_rate_notadj = value)


# Joining the datasets
phillips_data_ger <- cpi_ger_adj %>%
  inner_join(cpi_ger_notadj, by = c("date" = "date")) %>%
  inner_join(pce_ger_adj, by = c("date" = "date")) %>%
  inner_join(ppi_ger_notadj, by = c("date" = "date")) %>%
  inner_join(unemployment_ger_adj, by = c("date" = "date")) %>%
  inner_join(unemployment_ger_notadj, by = c("date" = "date")) %>%
  select(date,
         cpi_adj, cpi_notadj,
         pce_adj,
         ppi_notadj,
         unemployment_rate_adj, unemployment_rate_notadj)


## calculating inflation rates using dplyr
phillips_data_ger <- phillips_data_ger %>%
  mutate(cpi_inflation_adj = ((cpi_adj - lag(cpi_adj))*100)/lag(cpi_adj),
         cpi_inflation_notadj = ((cpi_notadj - lag(cpi_notadj))*100)/lag(cpi_notadj),
         ppi_inflation_notadj = ((ppi_notadj - lag(ppi_notadj))*100)/lag(ppi_notadj),
         pce_inflation_adj = ((pce_adj - lag(pce_adj))*100)/lag(pce_adj))

glimpse(phillips_data_ger)

# saving the data as a csv for future use:
# phillips_data_ger %>% write_csv("phillips_data_ger.csv")


## ----plotting the time series, fig.width = 8, fig.height = 7, fig.align = 'center', warning = FALSE, echo = FALSE----
inflation_tsplot <- phillips_data_ger %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = pce_inflation_adj), color = "steelblue1") +
  geom_line(aes(y = ppi_inflation_notadj), color = "orangered") +
  geom_line(aes(y = cpi_inflation_adj), color = "seagreen") +
  geom_line(aes(y = cpi_inflation_notadj), color = "palegreen3") +
  # color legend
  annotate("text",
           label = paste("PCE (seasonally adjusted)"), colour = "steelblue1",
           y = 8, x = as.Date("2002-01-01")) +
  annotate("text",
           label = paste("PPI (not seasonally adjusted)"), colour = "orangered",
           y = 6, x = as.Date("2002-01-01")) +
  annotate("text",
           label = paste("CPI (seasonally adjusted)"), colour = "seagreen",
           y = 8, x = as.Date("2013-01-01")) +
  annotate("text",
           label = paste("CPI (not seasonally adjusted)"), colour = "palegreen3",
           y = 6, x = as.Date("2013-01-01")) +
  labs(x = "Date", 
       y = "Inflation rate",
       title = "Inflation rates based on CPI, PPI and PCE",
       subtitle = "Germany 1995 - 2021") +
  theme_minimal()



unemployment_tsplot <- phillips_data_ger %>% 
  ggplot() +
  geom_line(aes(x = date, y = unemployment_rate_adj), color = "tomato4") +
  geom_line(aes(x = date, y = unemployment_rate_notadj), color = "tomato") +
  labs(x = "Date",
       y = "Unemployment rate",
       title = "Unemployment rates: seasonally adjusted and not seasonally adjusted",
       subtitle = "Germany 1995 - 2021",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  theme_minimal()

grid.arrange(inflation_tsplot, unemployment_tsplot, nrow = 2)



## ----plotting phillips curves, fig.height = 6, fig.width = 6.5, fig.align = "center", warning = FALSE, message = FALSE, echo = FALSE----
pplot_cpi_adj <- phillips_data_ger %>% 
  ggplot() +
  geom_point(aes(y = cpi_inflation_adj, x = unemployment_rate_adj)) +
  geom_smooth(aes(y = cpi_inflation_adj, x = unemployment_rate_adj), 
              method = 'lm', se = FALSE, 
              color = "seagreen") +
  labs(x = "Unemployment Rate", y = "Inflationrate",
       title = "CPI",
       subtitle = "Seasonally adjusted", caption = "") +
  theme_minimal()

pplot_cpi_notadj <- phillips_data_ger %>% 
  ggplot() +
  geom_point(aes(y = cpi_inflation_notadj, x = unemployment_rate_notadj)) +
  geom_smooth(aes(y = cpi_inflation_notadj, x = unemployment_rate_notadj), 
              method = 'lm', se = FALSE, 
              color = "seagreen") +
  labs(x = "Unemployment Rate", y = "Inflationrate",
       title = "CPI",
       subtitle = "Not seasonally adjusted", caption = "") +
  theme_minimal()

pplot_pce <- phillips_data_ger %>% 
  ggplot() +
  geom_point(aes(x = unemployment_rate_adj, y = pce_inflation_adj)) +
  geom_smooth(aes(x = unemployment_rate_adj, y = pce_inflation_adj), 
              method = 'lm', se = FALSE,
              color = "steelblue1") +
  labs(x = "Unemployment Rate", y = "Inflationrate",
       title = "PCE",
       subtitle = "Seasonally adjusted", caption = "") +
  theme_minimal()

pplot_ppi <- phillips_data_ger %>% 
  ggplot() +
  geom_point(aes(y = ppi_inflation_notadj, x = unemployment_rate_notadj)) +
  geom_smooth(aes(y = ppi_inflation_notadj, x = unemployment_rate_notadj), 
              method = 'lm', se = FALSE,
              color = "orangered") +
  labs(y = "Inflationrate", x = "Unemployment Rate",
       title = "PPI",
       subtitle = "Not seasonally adjusted",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  theme_minimal()

grid.arrange(pplot_cpi_adj, pplot_cpi_notadj, pplot_pce, pplot_ppi, nrow = 2)


## ----uk data, echo = FALSE----------------------------------------------------------------------------------------
# ignore this code (until the time series plots) if you already read in the data via csv

# Consumer Price Index of All Items in the United Kingdom
# Units: Index 2015=100, Not Seasonally Adjusted, Frequency:  Quarterly
uk_cpi <- fredr(series_id = "GBRCPIALLQINMEI") %>%
  rename(cpi = value)

# Registered Unemployment Rate for the United Kingdom
# Units: Percent, Not Seasonally Adjusted, Frequency:  Quarterly
uk_unemployment <- fredr(series_id = "LMUNRRTTGBQ156N") %>%
  rename(unemployment_rate = value)

# Joining the data
uk_data <- uk_cpi %>%
  inner_join(uk_unemployment, by = c("date" = "date")) %>%
  select(date, cpi, unemployment_rate) %>%
  filter(date >= "1984-01-01")

# Calculating UK inflation rates
uk_data <- uk_data %>%
  mutate(inflation = (cpi - lag(cpi))/lag(cpi),
         inflation = inflation * 100)

# saving the data for future use:
# uk_data %>% write_csv("uk_data.csv")


## ----calculating inflation rates using a loop, echo = FALSE-------------------------------------------------------
#' another way to calculate the inflation rates: use loops
#' above, I used dplyr's mutate()

# inflation <- c()
# for(i in 1:nrow(uk_data)) {
#   inflation[i] <- (
#     (uk_data$cpi[i+1] - uk_data$cpi[i])
#     / uk_data$cpi[i]
#     )*100
# }


## ----plotting time series uk, warning = FALSE, echo = FALSE-------------------------------------------------------
uk_inflation_plot <- uk_data %>% 
  ggplot() +
  geom_line(aes(x = date, y = inflation), color = "seagreen") +
  # vertical lines at the cut-offs for the comparison between time periods
  geom_vline(xintercept = as.Date("1998-01-01"), linetype = 2, color = "darkgray") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = 2, color = "darkgray") +
  labs(x = "",
       y = "Inflation Rate",
       title = "Inflation and Unemployment rate (not seasonally adjusted)",
       subtitle = "UK 1984 - 2021, Quarterly Data") +
  theme_minimal()

uk_unemployment_plot <- uk_data %>% 
  ggplot() +
  geom_line(aes(x = date, y = unemployment_rate), color = "tomato") +
  # vertical lines at the cut-offs for the comparison between time periods
  geom_vline(xintercept = as.Date("1998-01-01"), linetype = 2, color = "darkgray") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = 2, color = "darkgray") +
  labs(x = "Date",
       y = "Unemployment Rate",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  theme_minimal()
  
grid.arrange(uk_inflation_plot, uk_unemployment_plot, nrow = 2)


## ----plotting uk phillips curves, fig.width = 8, fig.height = 4, warning = FALSE, message = FALSE, echo = FALSE----
max_inflation <- max(uk_data$inflation, na.rm = TRUE)
min_inflation <- min(uk_data$inflation, na.rm = TRUE)
max_unemployment <- max(uk_data$unemployment_rate, na.rm = TRUE)
min_unemployment <- min(uk_data$unemployment_rate, na.rm = TRUE)

# Phillips curve 1984 - 1997
phillips_84_97 <- uk_data %>% 
  # selecting the years 1984 until 1997:
  filter(date >= "1984-01-01" & date < "1998-01-01") %>% 
  ggplot() +
  geom_point(aes(y = inflation, x = unemployment_rate)) +
  geom_smooth(aes(y = inflation, x = unemployment_rate), 
              method = 'lm', 
              se = FALSE, 
              color = "seagreen") +
  # fixing axis for better comparability
  scale_x_continuous(limits = c(min_unemployment, max_unemployment)) +
  scale_y_continuous(limits = c(min_inflation, max_inflation)) +
  labs(title = "Phillips curves UK, Inflation based on not-seasonally adjusted CPI",
       subtitle = "1984 - 1997",
       y = "Inflation Rate", 
       x = "Unemployment Rate",
       caption = "") +
  theme_minimal()

# Phillips curve 1998 - 2012
phillips_98_12 <- uk_data %>% 
  # selecting the years 1998 until 2012:
  filter(date >= "1998-01-01" & date < "2013-01-01") %>% 
  ggplot() +
  geom_point(aes(y = inflation, x = unemployment_rate)) +
  geom_smooth(aes(y = inflation, x = unemployment_rate), 
              method = 'lm', 
              se = FALSE, 
              color = "seagreen") +
  # fixing axis for better comparability
  scale_x_continuous(limits = c(min_unemployment, max_unemployment)) +
  scale_y_continuous(limits = c(min_inflation, max_inflation)) +
  labs(title = "",
       subtitle = "1998 - 2012",
       y = "", 
       x = "Unemployment Rate",
       caption = "") +
  theme_minimal()

# Phillips curve 2013 - 2020
phillips_13_20 <- uk_data %>% 
  # selecting the years 2013 until 2020:
  filter(date >= "2013-01-01" & date < "2021-01-01") %>% 
  ggplot() +
  geom_point(aes(y = inflation, x = unemployment_rate)) +
  geom_smooth(aes(y = inflation, x = unemployment_rate), 
              method = 'lm', 
              se = FALSE, 
              color = "seagreen") +
  # fixing axis for better comparability
  scale_x_continuous(limits = c(min_unemployment, max_unemployment)) +
  scale_y_continuous(limits = c(min_inflation, max_inflation)) +
  labs(title = "",
       subtitle = "2013 - 2020",
       y = "", 
       x = "Unemployment Rate",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  theme_minimal()
  

# Combining the 3 plots
grid.arrange(phillips_84_97, phillips_98_12, phillips_13_20,
             ncol = 3)

