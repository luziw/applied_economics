## ----setup, message = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)  # useful package collection 
library(AER)        # Functions, data sets, etc. for the book Applied Econometrics with R
library(skimr)      # Compact and Flexible Summaries of Data
library(stargazer)  # Well-Formatted Regression and Summary Statistics Tables
library(gridExtra)  # For arranging multiple grid-based plots on a page, and draw tables.
library(lmtest)     # tests and data sets for diagnostic checking in linear regression models
library(sandwich)   # Robust Covariance Matrix Estimators


## ----data, results = FALSE---------------------------------------------------------------------
data(DoctorVisits)
skim(DoctorVisits)


## ----linear regression-------------------------------------------------------------------------
reg <- DoctorVisits %>% 
  lm(formula = visits ~ gender + age + income + health + illness + private) 

reg %>% 
  stargazer(type = "text")


## ----var-cov matrix under homoskedasticity-----------------------------------------------------
vcov(reg) 


## ----var-cov matrix under heteroskedasticity---------------------------------------------------
vcovHC(reg, type = 'HC0')


## ----rsquared----------------------------------------------------------------------------------
residuals_reg <- resid(reg)

SSE <- sum(residuals_reg^2)

SST <- sum((DoctorVisits$visits - mean(DoctorVisits$visits))^2)

r_squared <- 1-(SSE/SST)
r_squared


## ----adjusted rsquared-------------------------------------------------------------------------
adjusted_rsquared <- 1 - ((nrow(DoctorVisits) - 1) * sum(residuals_reg^2)) / 
  ((nrow(DoctorVisits) - 6) * sum((DoctorVisits$visits - mean(DoctorVisits$visits))^2)) 
adjusted_rsquared


## ----variance inflation factor-----------------------------------------------------------------
vif(reg)


## ----reg2--------------------------------------------------------------------------------------
reg2 <- DoctorVisits %>% 
  lm(formula = visits ~ age + income) 

reg2 %>% 
  stargazer(type = "text")


## ----residuals vs fitted values plot, fig.height = 3, fig.width = 3.5, fig.align = "center"----
residuals_reg2 <- resid(reg2)
fitted_reg2 <- fitted(reg2)

cbind(residuals_reg2, fitted_reg2) %>% as_tibble %>% 
  ggplot() +
  geom_point(aes(x = fitted_reg2, y = residuals_reg2), color = 'tomato') +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


## ----auxiliary regression----------------------------------------------------------------------
# constructing the left-handside variable for the auxiliary regression:
left_handside <- residuals_reg2^2 / var(residuals_reg2)

# auxiliary regression: we use the x_i from our original regression as z_i:
aux_reg <- lm(data = DoctorVisits, formula = left_handside ~ age + income)

aux_reg %>% stargazer(type = 'text')


## ----centered R2 auxiliary reg-----------------------------------------------------------------
rsquared_auxreg <- summary(aux_reg)$r.squared
rsquared_auxreg


## ----BPG manually------------------------------------------------------------------------------
# scale the centered rsquared by the number of observations: 
bpg_manually <- nrow(DoctorVisits) * rsquared_auxreg

# we get the test statistic
bpg_manually

# p-value
pchisq(bpg_manually, df = 2, lower.tail = FALSE)


## ----using bptest------------------------------------------------------------------------------
bptest(reg2)


## ----auxiliary reg for white test--------------------------------------------------------------
left_hand_white <- resid(reg2)^2
agesq = DoctorVisits$age^2
incomesq = DoctorVisits$income^2

aux_reg_white <- lm(DoctorVisits, formula = left_hand_white ~ I(age^2) + I(income^2) + age*income) 

aux_reg_white %>% stargazer(type = 'text')


## ----white teststatistic manually--------------------------------------------------------------
# test statistic
white_manually <- nrow(DoctorVisits) * summary(aux_reg_white)$r.squared
white_manually

# p-value
pchisq(white_manually, df = 5, lower.tail = FALSE)


## ----white test using bptest-------------------------------------------------------------------
bptest(reg2, ~ income*age + I(income^2) + I(age^2), data = DoctorVisits)


## ----------------------------------------------------------------------------------------------
beta0 <- 4
beta1 <- 7

# sigma is a function in R: use a different name for the variable to avoid confusion 
mysigma <- 0.8 


## ----------------------------------------------------------------------------------------------
# for reproducable sampling
set.seed(1810)

x <- sample(c(0:9), replace = TRUE, size = 10000)
head(x)


## ----shocks------------------------------------------------------------------------------------
epsilon <- rnorm(x, mysigma)
head(epsilon)


## ----sampling y--------------------------------------------------------------------------------
y <- beta0 + beta1 * x + epsilon
head(y)


## ----ols based on sample-----------------------------------------------------------------------
reg_a <- cbind(x, y) %>% data.frame() %>% 
  lm(formula = y ~ x) 

reg_a %>% stargazer(type = 'text')


## ----monte carlo-------------------------------------------------------------------------------
M <- 10000

beta_estimates <- c()

for(i in 1:M) {
  epsilon <- rnorm(x, mysigma)
  y <- beta0 + beta1 * x + epsilon
  monte_carlo_reg <- cbind(x, y) %>% data.frame() %>% 
    lm(formula = y ~ x) 
  
  beta_estimates[i] <- monte_carlo_reg$coefficients[2]
}


## ----monte carlo plot--------------------------------------------------------------------------
hist10000 <- beta_estimates %>% as_tibble() %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  labs(x = "beta1",
       y = "Count",
       title = "Sample Size = 10000") +
  theme_minimal() 
hist10000


## ----variance and mean-------------------------------------------------------------------------
var(beta_estimates)

mean(sqrt(10000)*(reg_a$coefficients[2] - beta1))


## ----sample size 10----------------------------------------------------------------------------
# sample size = 10
x10 <- sample(c(0:9), replace = TRUE, size = 10)

beta_estimates10 <- c()

for(i in 1:M) {
  epsilon <- rnorm(x10, mysigma)
  y <- beta0 + beta1 * x10 + epsilon
  monte_carlo_reg <- cbind(x10, y) %>% data.frame() %>% 
    lm(formula = y ~ x10) 
  
  beta_estimates10[i] <- monte_carlo_reg$coefficients[2]
}

hist10 <- beta_estimates10 %>% as_tibble() %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  labs(x = "beta1",
       y = "Count",
       title = "Sample Size = 10") +
  theme_minimal()

var(beta_estimates10)
mean(sqrt(100)*beta_estimates10 - beta1)

## ----sample size 100---------------------------------------------------------------------------
# sample size = 100
x100 <- sample(c(0:9), replace = TRUE, size = 100)

beta_estimates100 <- c()

for(i in 1:M) {
  epsilon <- rnorm(x100, mysigma)
  y <- beta0 + beta1 * x100 + epsilon
  monte_carlo_reg <- cbind(x100, y) %>% data.frame() %>% 
    lm(formula = y ~ x100) 
  
  beta_estimates100[i] <- monte_carlo_reg$coefficients[2]
}

hist100 <- beta_estimates100 %>% as_tibble() %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  labs(x = "beta1",
       y = "Count",
       title = "Sample Size = 100") +
  theme_minimal()

var(beta_estimates100)
mean(sqrt(100)*beta_estimates100 - beta1)


## ----sample size 1000--------------------------------------------------------------------------
# sample size = 1000
x1000 <- sample(c(0:9), replace = TRUE, size = 1000)

beta_estimates1000 <- c()

for(i in 1:M) {
  epsilon <- rnorm(x1000, mysigma)
  y <- beta0 + beta1 * x1000 + epsilon
  monte_carlo_reg <- cbind(x1000, y) %>% data.frame() %>% 
    lm(formula = y ~ x1000) 
  
  beta_estimates1000[i] <- monte_carlo_reg$coefficients[2]
}

hist1000 <- beta_estimates1000 %>% as_tibble() %>% 
  ggplot() +
  geom_histogram(aes(value)) +
  labs(x = "beta1",
       y = "Count",
       title = "Sample Size = 1000") +
  theme_minimal()

var(beta_estimates1000)
mean(sqrt(100)*beta_estimates1000 - beta1)


## ----histograms--------------------------------------------------------------------------------
grid.arrange(hist10, hist100, hist1000, hist10000, nrow = 2, ncol = 2)

