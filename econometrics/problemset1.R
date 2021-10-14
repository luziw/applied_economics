
# setup -------------------------------------------------------------------

library(tidyverse)  # <3
library(AER)        # Functions, data sets, etc. for the book Applied Econometrics with R
library(skimr)      # Compact and Flexible Summaries of Data
library(stargazer)  # Well-Formatted Regression and Summary Statistics Tables
library(gridExtra)  # For arranging multiple grid-based plots on a page, and draw tables.
library(scales)     # Methods for automatically determining breaks and labels for axes and legends




# a) Estimation -----------------------------------------------------------

## data
data(HousePrices)
glimpse(HousePrices)


## Manual Estimation

avg_price <- mean(HousePrices$price)

avg_bedrooms <- mean(HousePrices$bedrooms)


## beta_0 and beta_1 estimation
beta_1 <- (sum((HousePrices$bedrooms - avg_bedrooms) * (HousePrices$price - avg_price))) /
  sum((HousePrices$bedrooms - avg_bedrooms)^2)

beta_0 <- avg_price - beta_1 * avg_bedrooms


# calculating fitted values and residuals
fitted_manually <- beta_0 + beta_1 * HousePrices$bedrooms

residuals_manually <- fitted_manually - HousePrices$bedrooms



## Estimation using the implemented R function
# estimation using lm()
using_lm <- lm(HousePrices, formula = price ~ bedrooms) 
stargazer(using_lm, type = 'text')



# b) Plot -----------------------------------------------------------------

HousePrices %>% 
  ggplot(aes(x = bedrooms, y = price)) +
  geom_point(color = "gray35") +
  geom_smooth(method = 'lm', se = FALSE, color = "darkgreen") +
  labs(x = "Number of Bedrooms",
       y = "Price",
       caption = "Source: AER Package - Dataset HousePrices") +
  theme_minimal()


# c) Verify the numerical properties of the residuals ---------------------

# numerical properties
sum(resid(using_lm))

# Extracting the residuals
residuals_reg1 <- resid(using_lm)

# The residuals have mean zero:
summary(residuals_reg1)

# Extracting the fitted values:
fitted_values_reg1 <- fitted(using_lm)

# heteroscedastic residuals: 
cbind(fitted_values_reg1, residuals_reg1) %>% as_tibble() %>% 
  ggplot() +
  geom_point(aes(x = fitted_values_reg1, y = residuals_reg1), color = "tomato") +
  scale_x_continuous(labels = comma) +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


## r regression through the origin
# Adding 0 tells the lm()-function to fit the line through the origin
lm_through_origin <- lm(HousePrices, formula = price ~ 0 + bedrooms)
stargazer(lm_through_origin, type = "text")

# Residuals
residuals_origin <- resid(lm_through_origin)
sum(residuals_origin)



# d) Adding `lotsize` as an additional regressor --------------------------

# regression on x  
avg_lotsize <- mean(HousePrices$lotsize)

gamma_1 <- (sum((HousePrices$bedrooms - avg_bedrooms) * (HousePrices$lotsize - avg_lotsize))) /
  sum((HousePrices$bedrooms - avg_bedrooms)^2)
gamma_1

gamma_0 <- avg_lotsize - gamma_1 * avg_bedrooms
gamma_0


# Calculating the residuals and beta2
residuals <- HousePrices$lotsize - (gamma_0 + gamma_1 * HousePrices$bedrooms)

covariance <- cov(HousePrices$price, residuals)
variance <- var(residuals)

beta_2 <- covariance / variance



# Now we compare our results to a regression using `lm()`:
lm_2 <- lm(HousePrices, formula = price ~ bedrooms + lotsize) 
stargazer(lm_2, type = 'text')





