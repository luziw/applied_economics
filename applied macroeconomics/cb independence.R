## ----setup, include = FALSE----------------------------------------------------------------------------------------------------
library(tidyverse)   # <3
library(skimr)       # Compact and Flexible Summaries of Data
library(readxl)      # Import excel files into R
library(wesanderson) # Colour Palettes generated mostly from 'Wes Anderson' movies.
library(fredr)       # An R client for the 'Federal Reserve Economic Data' ('FRED') API - Functions to retrieve economic time series and other data from 'FRED'.
library(lubridate)   # Make Dealing with Dates a Little Easier
library(gridExtra)   # For arranging multiple grid-based plots on a page, and draw tables.
library(ggrepel)     # Provides text and label geoms for 'ggplot2' that help to avoid overlapping text labels


## ----data, echo = FALSE--------------------------------------------------------------------------------------------------------
# go to https://davideromelli.com/cbidata/ to download the data set

cb_data <- read_excel("CBIData_Romelli2021.xlsx") 

# select only relevant countries 
cb_data <- cb_data %>% 
  filter(iso_a3 %in% c("AUS", "CAN", "FRA", "DEU", "ITA", "JPN", "NZL", "CHE", "GBR", "USA"))

# # to read in the data from the provided cpi_data.csv file run the following line 
# cpi_data <- read_csv("cpi_data.csv")


## ----fredr data, message = FALSE-----------------------------------------------------------------------------------------------
# run the following code if you use a FRED API key to download the CPI data
fredr_set_key("your_key_here")

# Australia
cpi_aus <- fredr(series_id = "AUSCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "AUS")

# Canada
cpi_can <- fredr(series_id = "CANCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "CAN")

# France
cpi_fra <- fredr(series_id = "FRACPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "FRA")

# Germany
cpi_ger <- fredr(series_id = "DEUCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "DEU")

# Italy
cpi_ita <- fredr(series_id = "ITACPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "ITA")

# Japan
cpi_jpn <- fredr(series_id = "JPNCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "JPN")

# New Zealand
cpi_nzl <- fredr(series_id = "NZLCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "NZL")

# Switzerland
cpi_che <- fredr(series_id = "CHECPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "CHE")

# United Kingdom
cpi_gbr <- fredr(series_id = "GBRCPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "GBR")

# United States
cpi_usa <- fredr(series_id = "USACPIALLQINMEI") %>%
  rename(cpi = value) %>%
  mutate(country = "USA")

# joining the data: creating a long data set for tidyverse convenience
cpi_data <- cpi_aus %>%
  full_join(cpi_can) %>%
  full_join(cpi_fra) %>%
  full_join(cpi_ger) %>%
  full_join(cpi_ita) %>%
  full_join(cpi_jpn) %>%
  full_join(cpi_nzl) %>%
  full_join(cpi_che) %>%
  full_join(cpi_gbr) %>%
  full_join(cpi_usa) %>%
  # selecting only the relevant columns
  select(date, country, cpi) %>%
  # selecting data from 1972-2010 like arone and romelli
  # but starting in 1971 for inflation calculation for the year 1972
  filter(date >= "1971-01-01" & date < "2011-01-01" )

# calculating inflation rates
cpi_data <- cpi_data %>%
  group_by(country) %>%
  # default log in R: natural logarithm
  mutate(inflation = 100 * (log(cpi) - log(lag(cpi, n = 4))))

# saving the data as a csv for future use:
cpi_data %>% write_csv("cpi_data.csv")

## ----ts plots inflation and cbie, fig.width=8, fig.height=8, fig.align='center'------------------------------------------------
ts_inflation <- cpi_data %>% 
  ggplot() +
  geom_line(aes(x = date, y = inflation, colour = country)) +
  facet_wrap(.~country, nrow = 2) +
  scale_color_manual(values = wes_palette(10, name = "IsleofDogs1", type = "continuous")) +
  labs(title = "Inflation in selected OECD Countries",
       subtitle = "based on not seasonally adjusted CPI",
       x = "", y = "Inflation Rate in %",
       caption = "Source: Federal Reserve Economic Data (FRED)") +
  theme_minimal() +
  theme(legend.position= "",
        axis.text.x = element_text(angle = 45)) 

ts_cbie <- cb_data %>% 
  ggplot() +
  geom_line(aes(x = year, y = CBIE, color = iso_a3)) +
  facet_wrap(.~iso_a3, nrow = 2) + 
  scale_color_manual(values = wes_palette(10, name = "IsleofDogs1", type = "continuous")) +
  labs(title = "Central Bank Independence",
       subtitle = "CBIE ",
       x = "", y = "Central Bank Independence",
       caption = "Source: Davide Romelli") +
  theme_minimal() +
  theme(legend.position= "",
        axis.text.x = element_text(angle = 45)) 

grid.arrange(ts_inflation, ts_cbie, nrow = 2)


## ----how many entries per year?, eval = FALSE----------------------------------------------------------------------------------
## cpi_data %>%
##   group_by(country, year(date)) %>%
##   count(year(date)) %>%
##   pull(n) # -> 4 observations per year -> we can simply divide by 4

## ----annual average inflation--------------------------------------------------------------------------------------------------
cpi_data <- cpi_data %>% 
  mutate(year = year(date)) %>% 
  group_by(country, year) %>% 
  mutate(annual_avg_inflation = sum(inflation) / 4) %>% 
  ungroup()


## ----annual average inflation vs cbi-------------------------------------------------------------------------------------------
plot_data <- cpi_data %>% 
  group_by(year) %>% 
  inner_join(cb_data, by = c("year" = "year", "country" = "iso_a3")) %>% 
  select(year, country, annual_avg_inflation, CBIE) %>% 
  # remove duplicate rows
  distinct()

plot_data %>% 
  ggplot(aes(x = CBIE, y = annual_avg_inflation, colour = country)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "darkorange") +
  scale_color_manual(values = wes_palette(10, name = "IsleofDogs1", type = "continuous")) +
  labs(x = "Central Bank Independence",
       y = "Annual Average Inflation",
       colour = "Country",
       caption = "Data: Own calculations based on FRED and data from Davide Romelli") +
  theme_minimal() +
  theme(legend.position = "bottom")


## ----average over period under study-------------------------------------------------------------------------------------------
plot_data %>% 
  group_by(country) %>% 
  mutate(avg_inflation = mean(annual_avg_inflation),
         avg_cbie = mean(CBIE)) %>% 
  ggplot(aes(x = avg_cbie,
             y = avg_inflation, 
             colour = country,
             label = country)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "darkorange") +
  scale_color_manual(values = wes_palette(10, name = "IsleofDogs1", type = "continuous")) +
  labs(subtitle = "1972 - 2010",
       x = "Mean Central Bank Independence",
       y = "Mean Inflation",
       colour = "Country",
       caption = "Data: Own calculations based on FRED and data from Davide Romelli") +
  theme_minimal() +
  theme(legend.position = "") +
  geom_text(vjust = 0, nudge_y = 0.1)



## ----standard deviation of average annual inflation----------------------------------------------------------------------------
plot_data <- plot_data %>% 
  mutate(sd_aa_inflation = sd(annual_avg_inflation)) 

plot_data %>% 
  ggplot(aes(x = CBIE,
             y = sd_aa_inflation,
             color = country)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = "darkorange") +
  scale_color_manual(values = wes_palette(10, name = "IsleofDogs1", type = "continuous")) +
  labs(x = "Central Bank Independence",
       y = "St.Dev. of Annual Average Inflation",
       colour = "Country",
       caption = "Data: Own calculations based on FRED and data from Davide Romelli") +
  theme_minimal() +
  theme(legend.position = "bottom")

