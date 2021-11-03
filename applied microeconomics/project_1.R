## ----setup, include = FALSE----------------------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(skimr)
library(stargazer)
library(ggdist)
library(latex2exp)
library(wesanderson)
library(spelling)


## ----loading the data----------------------------------------------------------------------------------------------------------
data <- read_excel("pwt100.xlsx")


## ----scatterplot, fig.height=3, fig.width=5, fig.align='center', echo = TRUE---------------------------------------------------
data <- data %>% 
  mutate(real_aggregate_wage = rgdpna * labsh,
         aggregate_hours_worked = avh * emp) 

data %>% 
  ggplot() +
  geom_point(aes(x = log(real_aggregate_wage/aggregate_hours_worked), y = log(avh)), alpha = 0.3) +
  geom_smooth(aes(x = log(real_aggregate_wage/aggregate_hours_worked), y = log(avh)), 
              method = 'lm', 
              se = FALSE, 
              colour = "seagreen") +
  labs(x = "log(real aggregate wage)",
       y = "log(aggregate hours worked)") +
  theme_minimal()


## ----elasticity, echo = TRUE---------------------------------------------------------------------------------------------------
# elasticity: log-log model
elasticity <- data %>% 
  lm(formula = log(avh) ~ log(real_aggregate_wage / aggregate_hours_worked))

stargazer(elasticity, type = 'text')


## ----keynes, echo = TRUE-------------------------------------------------------------------------------------------------------
min(data$year) 
# 1950 

data %>% 
  filter(year == 1950) %>% 
  summarize(mean_hours_worked_1950 = mean(avh, na.rm = TRUE)/52)

data %>% 
  filter(year == 2000) %>% 
  summarize(mean_hours_worked_2000 = mean(avh, na.rm = TRUE)/52)


## ----economic growth, fig.height=5, fig.width=8, fig.align='center'------------------------------------------------------------
# real gdp at constant 2017 national prices in mil 2017 US$
oecd <- c("Austria", "Belgium", "Canada", "Denmark", "France", "Germany", 
          "Greece", "Iceland", "Ireland", "Italy", "Luxembourg", "Netherlands", 
          "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "Turkey", 
          "United Kingdom", "United States")

data %>% 
  # filtering for OECD countries
  filter(country %in% oecd) %>% 
  ggplot(aes(x = year, y = rgdpna/pop)) +
  geom_line(aes(color = country)) +
  scale_color_manual(values = wes_palette(20, name = "GrandBudapest1", type = "continuous")) +
  labs(x = "Year",
       y = "real GDP per capita",
       colour = "Country") +
  facet_wrap(.~country) +
  theme_minimal() +
  theme(legend.position = "") 

data %>% 
  # filtering for OECD countries
  filter(country %in% oecd) %>% 
  ggplot(aes(x = year, y = avh/52)) +
  geom_line(aes(color = country)) +
  scale_color_manual(values = wes_palette(20, name = "GrandBudapest1", type = "continuous")) +
  labs(x = "Year",
       y = "Average Hours Worked per Week",
       colour = "Country") +
  facet_wrap(.~country) +
  theme_minimal() +
  theme(legend.position = "") 



