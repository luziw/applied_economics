## ----setup, message = FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)  # useful package collection 
library(AER)        # Functions, data sets, etc. for the book Applied Econometrics with R
library(skimr)      # Compact and Flexible Summaries of Data
library(stargazer)  # Well-Formatted Regression and Summary Statistics Tables
library(gridExtra)  # For arranging multiple grid-based plots on a page, and draw tables.
library(moderndive) # Tidyverse-Friendly Introductory Linear Regression


## ----load DoctorVisits--------------------------------------------------------------------------------------------------
data("DoctorVisits")


## ----multiple regression------------------------------------------------------------------------------------------------
reg_dv <- DoctorVisits %>% 
  lm(formula = visits ~ gender + age + income + health + illness + private)

# tidy regression table for an lm() object 
get_regression_table(reg_dv)

# tidy summary statistics of an lm() object
get_regression_summaries(reg_dv)


## ----variance-covariance matrix-----------------------------------------------------------------------------------------
# var-cov matrix under homoskedasticity
varcov_homo <- vcov(reg_dv) 
varcov_homo

# var-cov matrix under heteroskedasticity
# HC0 is the most commonly used way of correcting for heteroskedasticity 
varcov_hetero <- vcovHC(reg_dv, type = 'HC0') 
varcov_hetero


## ----standard errors----------------------------------------------------------------------------------------------------
st_err <- sqrt(diag(varcov_homo))
st_err


## ----confidence intervals-----------------------------------------------------------------------------------------------
# extract the beta coefficients
beta_hat <- summary(reg_dv)$coefficients[,1]
beta_hat

# confidence intervals
lower_bounds <- c()
upper_bounds <- c()

for(i in 1:7) {
  lower_bounds[i] <- beta_hat[i] - 1.96 * st_err[i]
  upper_bounds[i] <- beta_hat[i] + 1.96 * st_err[i]
}

confidence_intervals <- cbind(lower_bounds, upper_bounds) %>% 
  as.data.frame()
colnames(confidence_intervals) <- c("lowerbound", "upperbound")
rownames(confidence_intervals) <- c("intercept", "genderfemale", "age", "income", "health", 
                                    "illness", "privateyes")

confidence_intervals


## ----p-values-----------------------------------------------------------------------------------------------------------
z_scores <- c()

for(i in 1:7) {
  z_scores[i] <- beta_hat[i] / st_err[i]
}
z_scores

# p-values
p_values <- c()

for(i in 1:7) {
  p_values[i] <- pnorm(z_scores[i], lower.tail = FALSE)
}
p_values


## ----f-test by hand-----------------------------------------------------------------------------------------------------
n <- nrow(DoctorVisits)   # number of observations
k <- 7                    # number or regressors + intercept

#' restricted regression - restriction: beta1=beta2=beta3=...=beta6=0
#' => we are left with SSE_R = SST
SST <- sum((DoctorVisits$visits - mean(DoctorVisits$visits))^2)
SSE_R <- SST

SSE_U <- sum(resid(reg_dv)^2)

F_statistic <- ((SSE_R - SSE_U) / (k - 1)) / (SSE_U / (n - k))

#' pf: calculate a p-value from a F-distribution
#' lower.tail = FALSE to get a 2-sided test
p_value_Ftest <- pf(F_statistic, df1 = (k-1), df2 = (n-k), lower.tail = FALSE)
p_value_Ftest


## ----f-test provided by summary()---------------------------------------------------------------------------------------
summary(reg_dv)


## ----robust F-test------------------------------------------------------------------------------------------------------
linearHypothesis(reg_dv, 
                 c("genderfemale = 0", "age = 0", "income = 0", "health = 0",
                   "illness = 0", "privateyes = 0"), 
                 # white.adjust: specify to use a heterscedasticity-corrected coefficient covariance matrix
                 # hc0: one type of heteroskedasticity-correction (used in the lecture and above)
                 white.adjust = "hc0")


## ----regression with all available variables----------------------------------------------------------------------------
reg_all <- DoctorVisits %>% 
  lm(formula = visits ~ gender + age + income + illness + reduced + health + 
       private + freepoor + freerepat + nchronic + lchronic)

get_regression_table(reg_all)


## ----F-test reg_all-----------------------------------------------------------------------------------------------------
null_hypo <- c("nchronicyes = 0","freerepatyes = 0")

# F-test
linearHypothesis(reg_all, null_hypo)


## ----confidence ellipse-------------------------------------------------------------------------------------------------

confidenceEllipse(reg_dv, which.coef = c("age", "health"))


## ----data CPS1985-------------------------------------------------------------------------------------------------------
data("CPS1985")


## ----plot wage against experience, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 3----------------------
plot_we1 <- CPS1985 %>% 
  ggplot() +
  geom_point(aes(x = experience, y = wage)) +
  geom_smooth(aes(x = experience, y = wage), method = 'lm', se = FALSE, color = "seagreen") +
  labs(x = "Experience",
       y = "Wage") +
  theme_minimal()

plot_we2 <- CPS1985 %>% 
  ggplot() +
  geom_point(aes(x = experience, y = log(wage))) +
  geom_smooth(aes(x = experience, y = log(wage)), method = 'lm', se = FALSE, color = "seagreen") +
  labs(x = "Experience",
       y = "log(Wage)") +
  theme_minimal()

grid.arrange(plot_we1, plot_we2, nrow = 1)


## ----linear model vs log-linear model-----------------------------------------------------------------------------------
lin_mod <- CPS1985 %>% 
  lm(formula = wage ~ experience)

loglin_mod <- CPS1985 %>% 
  lm(formula = log(wage) ~ experience)

stargazer(lin_mod, loglin_mod, type = 'text')

# AIC and BIC as a pretty dataframe
aic_bic <- data.frame(
    AIC = rep(0, times = 2), 
    BIC = rep(0, times = 2),
    row.names = c("Linear Model", "Log-Linear Model"))

regressions = list(lin_mod, loglin_mod)

for (i in c(1:2)) {
    aic_bic[i, 1] <- AIC(regressions[[i]])
    aic_bic[i, 2] <- BIC(regressions[[i]])
}

stargazer(aic_bic, summary = FALSE, type = 'text')


## ----polynomial model---------------------------------------------------------------------------------------------------
poly_mod2 <- CPS1985 %>% 
  lm(formula = wage ~ experience + I(experience^2))

poly_mod3 <- CPS1985 %>% 
  lm(formula = wage ~ experience + I(experience^2) + I(experience^3))

stargazer(loglin_mod, poly_mod2, poly_mod3, type = 'text')

# AIC and BIC
aic_bic <- data.frame(
    AIC = rep(0, times = 4), 
    BIC = rep(0, times = 4),
    row.names = c("Linear", "Log-Linear", "Quadratic", "Cubic"))

regressions = list(lin_mod, loglin_mod, poly_mod2, poly_mod3)

for (i in c(1:4)) {
    aic_bic[i, 1] <- AIC(regressions[[i]])
    aic_bic[i, 2] <- BIC(regressions[[i]])
}

stargazer(aic_bic, summary = FALSE, type = 'text')


## ----polynomial mod with additional regressors--------------------------------------------------------------------------
poly_mod_multi <- CPS1985 %>% 
  lm(formula = wage ~ experience + I(experience^2) + education + gender + union)

stargazer(loglin_mod, poly_mod2, poly_mod_multi, type = 'text')

# AIC and BIC
aic_bic <- data.frame(
    AIC = rep(0, times = 5), 
    BIC = rep(0, times = 5),
    row.names = c("Linear", "Log-Linear", 
                  "Quadratic", "Cubic", 
                  "Multiple Polynomial"))

regressions = list(lin_mod, loglin_mod, poly_mod2, poly_mod3, poly_mod_multi)

for (i in c(1:5)) {
    aic_bic[i, 1] <- AIC(regressions[[i]])
    aic_bic[i, 2] <- BIC(regressions[[i]])
}

stargazer(aic_bic, summary = FALSE, type = 'text')


## ----elasiticity--------------------------------------------------------------------------------------------------------
# Are there data points with education = 0?
sum(CPS1985$education == 0)
# -> no, we don't have to worry about that

elasticity <- CPS1985 %>% 
  lm(formula = log(wage) ~ log(education))


## ----semi-elasticity----------------------------------------------------------------------------------------------------
semielasticity <- CPS1985 %>% 
  lm(formula = log(wage) ~ education)

stargazer(elasticity, semielasticity, type = 'text')

# AIC and BIC
aic_bic <- data.frame(
    AIC = rep(0, times = 2), 
    BIC = rep(0, times = 2),
    row.names = c("Elasticity", "Semi-Elasticity"))

regressions = list(elasticity, semielasticity)

for (i in c(1:2)) {
    aic_bic[i, 1] <- AIC(regressions[[i]])
    aic_bic[i, 2] <- BIC(regressions[[i]])
}

stargazer(aic_bic, summary = FALSE, type = 'text')


## ----loglog male and female---------------------------------------------------------------------------------------------
loglog_interaction <- CPS1985 %>% 
  lm(formula = log(wage) ~ log(education)*gender + union*gender)

loglog_male <- CPS1985 %>% 
  filter(gender == "male") %>% 
  lm(formula = log(wage) ~ log(education) + union)

loglog_female <- CPS1985 %>% 
  filter(gender == "female") %>% 
  lm(formula = log(wage) ~ log(education) + union)

stargazer(loglog_interaction, loglog_male, loglog_female, type = 'text')


## ----saturated model----------------------------------------------------------------------------------------------------
saturated <- CPS1985 %>% 
  lm(formula = wage ~ gender + occupation)

stargazer(saturated, type = 'text')


## ----loglog union membership--------------------------------------------------------------------------------------------
CPS1985 <- CPS1985 %>% 
  mutate(education_dummy = ifelse(education > 10, yes = 1, no = 0))

loglog_interaction2 <- CPS1985 %>% 
  lm(formula = log(wage) ~ log(education) + union * education_dummy)

loglog_highedu <- CPS1985 %>% 
  filter(education > 10) %>% 
  lm(formula = log(wage) ~ log(education) + union)

loglog_lowedu <- CPS1985 %>% 
  filter(education <= 10) %>% 
  lm(formula = log(wage) ~ log(education) + union)
  
stargazer(loglog_interaction2, loglog_highedu, loglog_lowedu, type = 'text')

