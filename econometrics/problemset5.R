## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------


library(tidyverse)
library(foreign)
library(skimr)
library(stargazer)
library(sandwich)
library(lmtest)
library(multiwayvcov)


## ----data------------------------------------------------------------------------------------------------------------------------------------------------------
data <- read.dta("main_fame.dta")

data <- data %>%
  mutate(NMW = as.factor(NMW),
         ctreat1 = as.factor(ctreat1),
         pp = as.factor(pp),
         log_avwage = log(avwage)) %>% 
  # dataset the paper used 
  filter(pp == 1)


## ----conditional means, echo = TRUE----------------------------------------------------------------------------------------------------------------------------

conditional_means <- data %>% 
  group_by(ctreat1, NMW) %>% 
  summarize(conditional_mean = mean(log_avwage))

conditional_means


## ----diff-in-diff estimate-------------------------------------------------------------------------------------------------------------------------------------
diff_Treatment <- conditional_means$conditional_mean[4] - conditional_means$conditional_mean[3]
diff_NMW <- conditional_means$conditional_mean[2] - conditional_means$conditional_mean[1]

diff_in_diff <- diff_Treatment - diff_NMW

cbind(diff_Treatment, diff_NMW, diff_in_diff) %>% as_tibble()


## ----diff in diff plot-----------------------------------------------------------------------------------------------------------------------------------------
# vector for label names
treatment_labels <- c(`0` = "Control Group",`1` = "Treatment Group")

conditional_means %>% 
  # group_by(ctreat1) %>% 
  ggplot() +
  geom_point(aes(x = NMW, y = conditional_mean)) +
  geom_line(aes(x = NMW, y = conditional_mean), group = 1) +
  facet_grid(.~ ctreat1, labeller = as_labeller(treatment_labels)) +
  labs(x = "",
       y = "Conditional Mean of log(average wages)") +
  scale_x_discrete(labels=c("0" = "Before NMW", "1" = "After NMW")) +
  theme_minimal() +
  # (Colour) boxes around Control Group and Treatment Group Labels 
  theme(strip.background = element_rect(fill = "white")) 
  


## ----diff in diff using regression-----------------------------------------------------------------------------------------------------------------------------
did_reg <- data %>% 
  lm(formula = log_avwage ~ NMW * ctreat1)

stargazer(did_reg, type = 'text')
# beta of the interaction term is the diff-in-diff estimator


## ----coeftest clustered standard errors, include = TRUE--------------------------------------------------------------------------------------------------------
coeftest(did_reg, vcov = cluster.vcov(did_reg, cluster = data$regno))

# this should work also but does not:
# coeftest(did_reg, vcov = vcovHC(did_reg, cluster = regno))

