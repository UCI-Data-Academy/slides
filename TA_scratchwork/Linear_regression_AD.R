library(tidyverse)
setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")

AD <- readr::read_csv("alzheimer_data.csv")

AD %>% 
  select(naccicv, female) %>% 
  ggplot() +
  geom_point(aes(x = female, y = naccicv))

AD %>% 
  select(naccicv, female) %>% 
  ggplot() +
  geom_point(aes(x = jitter(female, factor = 0.2),
                 y = naccicv)) +
  coord_cartesian(ylim=c(0,2000))

lm(naccicv ~ female, data = AD)

AD %>% 
  select(naccicv, female) %>% 
  ggplot() +
  geom_point(aes(x = jitter(female, factor = 0.2),
                 y = naccicv)) +
  coord_cartesian(ylim=c(0,2000)) +
  geom_abline(intercept = 1458.4, 
              slope = -142.1, color = "blue")

AD <- AD %>% 
  mutate(old = ifelse(age >= 50, 1, 0))

lm(formula = weight ~ old, data = AD)

AD %>% 
  select(old, weight) %>% 
  ggplot() +
  geom_point(aes(x = jitter(old, factor = 0.2),
                 y = weight)) +
  geom_abline(intercept = 172.565, 
              slope = -5.566, color = "blue")

lm(bpsys ~ hrate, data = AD)

AD %>% 
  select(hrate, bpsys) %>% 
  ggplot() +
  geom_point(aes(x = hrate,
                 y = bpsys)) +
  geom_abline(intercept = 132.59, 
              slope = -0.002246, color = "blue")

lm(bpsys ~ hrate, data = AD) %>% 
  summary() %>% 
  coefficients()


AD <- AD %>% 
  mutate(BMI = weight/height^2)

lm(animals ~ BMI + age, data = AD) %>% 
  summary() %>% 
  coefficients()

AD %>% 
  ggplot(aes(x = BMI)) +
  geom_histogram()
