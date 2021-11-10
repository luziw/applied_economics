## ----setup, include=FALSE-------------------------------------------------------------------------------

library(tidyverse)
library(AER)
library(moderndive)
library(stargazer)
library(gridExtra)  # For arranging multiple grid-based plots on a page, and draw tables.


## ----data DoctorVisits----------------------------------------------------------------------------------
data(DoctorVisits)


## ----reg all--------------------------------------------------------------------------------------------
reg_all <- DoctorVisits %>% 
  lm(formula = visits ~ gender + age + income + illness + reduced + health + 
       private + freepoor + nchronic + lchronic)

get_regression_table(reg_all)
get_regression_summaries(reg_all)


## ----reg_all without nchronic---------------------------------------------------------------------------
reg_1 <- DoctorVisits %>% 
  lm(formula = visits ~ gender + age + income + illness + reduced + health + 
       private + freepoor + lchronic)

stargazer(reg_all, reg_1, type = 'text')


## ----f-test---------------------------------------------------------------------------------------------
H0 <- c("nchronicyes = 0")
f_test_all <- linearHypothesis(reg_all, H0)
f_test_all


## ----model selection, echo = TRUE-----------------------------------------------------------------------
# how many variables are there?
length <- length(colnames(DoctorVisits))

# vector with all regressors 
all_regressors <- colnames(DoctorVisits)[2:length]

# function that drops one variable such that dropping it maximizes the adjusted rsquared  
best_model_one_dropped <- function(input_regressors) {
  full_data <- DoctorVisits %>% select(visits, all_of(input_regressors))
  full_reg <- lm(data = full_data, formula = visits ~ .)
  adj_rsquared <- c()
  
  for(i in 1:length(input_regressors)) {
    regressors <- input_regressors[-i]

    reg_data <- DoctorVisits %>% select(visits, all_of(regressors))
    reg <- lm(data = reg_data, 
              formula = visits ~ .) # ~. fits a model using all available regressors

    adj_rsquared[i] <- summary(reg)$adj.r.squared
    }
  print(adj_rsquared)
  old_adj_rsq = summary(full_reg)$adj.r.squared
  new_adj_rsq = max(adj_rsquared)
  
  print("old adj rsq")
  print(old_adj_rsq)
  print("new adj rsq")
  print(new_adj_rsq)
  
  if (new_adj_rsq > old_adj_rsq) {
  # which model has the largest adjusted rsquared? -> index of that model
  best_model_index <- which.max(adj_rsquared)

  # which variable should be drop?
  drop <- input_regressors[best_model_index]
  print("Dropping variable:") 
  print(drop)

  # which variables are left after we drop the `drop` variable? 
  best_regressors <- input_regressors[-best_model_index]
  
  print("We keep the variables:")
  print(best_regressors)
    return(best_regressors)
  } else {
    print("We keep all variables")
    return(input_regressors)
  }
}
  

old_regressors <- all_regressors
new_regressors <- best_model_one_dropped(all_regressors)

while(length(new_regressors) != length(old_regressors)) {
  old_regressors <- new_regressors
  new_regressors <- best_model_one_dropped(old_regressors)
}
new_regressors



## ----dataset CPS1985------------------------------------------------------------------------------------
data("CPS1985")


## ----conditional mean, echo = TRUE----------------------------------------------------------------------
education_levels <- unique(CPS1985$education) %>% sort()
conditional_means <- c()
condition_counts <- c()

# loop for calculating conditional means, counts
for(i in 1:length(education_levels)) {
  conditional_means[i] = mean(CPS1985$wage[CPS1985$education == education_levels[i]])
  condition_counts[i] = sum(CPS1985$education == education_levels[i])
}

conditional_means


## ----conditional means plot, fig.width=8, fig.height=4, fig.align='center'------------------------------
plot_data <- cbind(education_levels, conditional_means, condition_counts) %>% as_tibble()

plot_data %>% 
  ggplot(aes(x = education_levels, y = conditional_means)) +
  geom_point(size = sqrt(condition_counts), colour = "dimgray") +
  labs(x = "Education Level",
       y = "Mean Wages conditional on `education`") +
  theme_minimal()
  


## ----histogram,  fig.width=3, fig.height=2, fig.align='center'------------------------------------------
CPS1985 %>% 
  ggplot(aes(x = education)) +
  geom_histogram(bins = 17, fill = "dimgray") +
  labs(x = "Educational Level",
       y = "Count") +
  theme_minimal()


## ----scatterplots, fig.width=7, fig.height=5, fig.align='center'----------------------------------------
linlin_plot <- CPS1985 %>% 
  ggplot(aes(x = education, y = wage)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = "seagreen") +
  labs(x = "Education",
       y = "Wage",
       title = "level-level Specification") +
  theme_minimal()

linlog_plot <- CPS1985 %>% 
  ggplot(aes(x = log(education), y = wage)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = "seagreen") +
  labs(x = "log(Education)",
       y = "Wage",
       title = "level-log Specification") +
  theme_minimal()

loglin_plot <- CPS1985 %>% 
  ggplot(aes(x = education, y = log(wage))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = "seagreen") +
  labs(x = "Education",
       y = "log(Wage)",
       title = "log-level Specification") +
  theme_minimal()

loglog_plot <- CPS1985 %>% 
  ggplot(aes(x = log(education), y = log(wage))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm', se = FALSE, colour = "seagreen") +
  labs(x = "log(Education)",
       y = "log(Wage)",
       title = "log-log Specification") +
  theme_minimal()

grid.arrange(linlin_plot, linlog_plot, loglin_plot, loglog_plot, nrow = 2)


## ----different functional forms-------------------------------------------------------------------------
linlin_mod <- CPS1985 %>% 
  lm(formula = wage ~ education)

linlog_mod <- CPS1985 %>% 
  lm(formula = wage ~ log(education))

loglin_mod <- CPS1985 %>% 
  lm(formula = log(wage) ~ education)

loglog_mod <- CPS1985 %>% 
  lm(formula = log(wage) ~ log(education))

regressions = list(linlin_mod, linlog_mod, loglin_mod, loglog_mod)

stargazer(regressions, type = 'text')


# AIC and BIC
aic_bic <- data.frame(
    AIC = rep(0, times = 4), 
    BIC = rep(0, times = 4),
    row.names = c("level-level", "level-log", "log-level", "log-log"))

for (i in c(1:4)) {
    aic_bic[i, 1] <- AIC(regressions[[i]])
    aic_bic[i, 2] <- BIC(regressions[[i]])
}

stargazer(aic_bic, summary = FALSE, type = 'text')




## ----LIE, echo = TRUE-----------------------------------------------------------------------------------
# left-handside
expected_yi <- mean(CPS1985$wage) 

# right-handside: using conditional_means and conditional_counts calculated in section 11a (loop)
EE_yi_conditional_xi <- sum(conditional_means * condition_counts) / sum(condition_counts)

# logical comparison 
expected_yi == EE_yi_conditional_xi





## ---- fig.height=3, fig.width=5, fig.align='center'-----------------------------------------------------
beta0 <- 4
beta1 <- 7

# for reproducable sampling
set.seed(3110)

x <- runif(n = 1000, min = 0, max = 100)

# sigma is a function in R: use a different name for the variable to avoid confusion 
mysigma <- x^2 + x + 800

# Simulate shocks for $\epsilon$ by drawing from a normal distribution with standard deviation $\sigma$ 
epsilon <- rnorm(n = 1000, mean = 0, sd = mysigma)

# Use your values of $\beta_0$ and $\beta_1$ and your sample for $x$ and $\epsilon$ to produce a sample for $y$
y <- beta0 + beta1 * x + epsilon

# plot the data
cbind(x,y) %>% as_tibble %>% 
  ggplot(aes(x=x,y=y)) +
  geom_point(alpha = 0.5) +
  theme_minimal()


## ----fit model------------------------------------------------------------------------------------------
# Compute the OLS estimate of $\beta_1$ based on this sample.
reg_a <- cbind(x, y) %>% data.frame() %>% 
  lm(formula = y ~ x)

reg_a %>% stargazer(type = 'text')


## ----plot heteroskedasticity----------------------------------------------------------------------------
fitted_values <- fitted(reg_a)
residuals <- resid(reg_a)
cbind(fitted_values, residuals) %>% as_tibble() %>%
  ggplot(aes(x = fitted_values, y = residuals)) +
  geom_point(colour = "tomato3") +
  labs(x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()



## ----standard errors, echo = TRUE-----------------------------------------------------------------------
# var-cov matrix under homoskedasticity
varcov_homo <- vcov(reg_a) 

# var-cov matrix under heteroskedasticity
# HC0 is the most commonly used way of correcting for heteroskedasticity 
varcov_hetero <- vcovHC(reg_a, type = 'HC0') 

# standard errors
st_err_homo <- sqrt(diag(varcov_homo))
st_err_homo
st_err_hetero <- sqrt(diag(varcov_hetero))
st_err_hetero


## ----t-statistics, echo = TRUE--------------------------------------------------------------------------
# extract the beta coefficients
beta_hat <- summary(reg_a)$coefficients[,1]
beta_hat

z_scores_homo <- c()
z_scores_hetero <- c()

for(i in 1:2) {
  z_scores_homo[i] <- beta_hat[i] / st_err_homo[i]
  z_scores_hetero[i] <- beta_hat[i] / st_err_hetero[i]
}
z_scores_homo
z_scores_hetero

# p-values
p_values_homo <- c()
p_values_hetero <- c()

for(i in 1:2) {
  p_values_homo[i] <- pnorm(z_scores_homo[i], lower.tail = FALSE)
  p_values_hetero[i] <- pnorm(z_scores_hetero[i], lower.tail = FALSE)
}
p_values_homo
p_values_hetero


## ----breusch pagan test---------------------------------------------------------------------------------
bptest(reg_a)

