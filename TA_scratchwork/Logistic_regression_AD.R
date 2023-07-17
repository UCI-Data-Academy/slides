setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")

library(tidyverse)
AD <- readr::read_csv("./alzheimer_data.csv")

AD <- AD %>% 
  mutate(cog_imp = ifelse(diagnosis == 1 | diagnosis == 2, 1, 0))

table(AD$cog_imp, AD$female)

fit <- glm(formula = cog_imp ~ factor(female), 
           data = AD, family = 'binomial')
fit %>% summary() %>% coefficients


fit2 <- glm(formula = cog_imp ~ age, 
            family='binomial', data = AD)
fit2 %>% summary() %>% coefficients()

AD <- AD %>% 
  mutate(has_AD = ifelse(diagnosis == 2, 1, 0))

fit2 <- glm(has_AD ~ height, family='binomial', data = AD)
fit2 %>% summary() %>% coefficients()

AD %>% 
  ggplot() +
  geom_point(aes(x = height, 
                 y = jitter(has_AD, 0.2) ))

AD <- AD %>% 
  mutate(trouble_bills = ifelse(bills == 0, 0, 1))

fit3 <- glm(trouble_bills ~ age + female + height + weight,
            family='binomial', data = AD)
fit3 %>% summary() %>% coefficients()
