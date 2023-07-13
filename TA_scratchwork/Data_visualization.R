library(tidyverse)

setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")
AD <- readr::read_csv("alzheimer_data.csv")

AD_subset <- AD %>% select(id,diagnosis,educ)
head(AD_subset)

AD_subset <- AD_subset %>% 
  mutate(diagnosis = as.factor(diagnosis))
summary(AD_subset$diagnosis)

AD_subset %>% 
  ggplot() +
  geom_bar(aes(x = diagnosis)) +
  scale_x_discrete(breaks = c(0, 1, 2), 
                   labels = c("Normal", "MCI", "Dementia"))  +
  labs(x = "Diagnosis", y = "Frequency", 
       title = "Frequency of Diagnoses in the Alzheimer's Data")

AD %>% 
  ggplot() +
  geom_histogram(aes(x = naccicv))

AD %>% 
  ggplot(aes(x = "", y = naccicv)) +
  geom_boxplot() +
  labs(x = "NACCICV", y = "Units", 
       title = "Distribution of Intracranial Volume")

AD %>% 
  ggplot() +
  geom_boxplot(aes(x = "", y = naccicv)) +
  labs(x = "NACCICV", y = "Units", 
       title = "Distribution of Intracranial Volume")

quantile(AD$naccicv)


AD %>% 
  select(height, weight) %>% 
  ggplot() +
  geom_point(aes(x = height, y = weight))

AD %>% 
  ggplot() +
  geom_boxplot(aes(x = factor(diagnosis), y = age))



