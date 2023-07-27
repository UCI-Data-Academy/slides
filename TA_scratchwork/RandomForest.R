setwd("~/Desktop/UCI/Summer 2023/DATA/slides/TA_scratchwork")
AD <- readr::read_csv("../data/alzheimer_data.csv")

library(randomForest)
library(tidyverse)
library(vip)

AD <- AD %>% 
  mutate(diagnosis = as.factor(diagnosis)) %>% 
  mutate(female = as.factor(female)) %>% 
  select(-id)

n_total <- nrow(AD)
n_train <- floor(n_total * 0.7)
set.seed(123)

train_indices <- sample(1:n_total, n_train, 
                        replace = FALSE)

data_train <- AD %>% slice(train_indices)
data_test <- AD %>% slice(-train_indices)

rf_fit <- randomForest(female ~ ., 
                       data = data_train, 
                       proximity = TRUE)

vip(rf_fit)

predictions <- predict(rf_fit, data_test)

accuracy <- sum(predictions == data_test$female) / 
  length(predictions)

plot(rf_fit)

logistic_model <- glm(female ~ age + height + weight, 
                      data = data_train,
                      family = "binomial")

logistic_predictions <- predict(logistic_model, 
                                data_test,
                                type = "response")
logistic_predictions <- ifelse(logistic_predictions >= 0.5, 1, 0)

sum(logistic_predictions == data_test$female) / 
  length(logistic_predictions)


rf_fit <- randomForest(age ~ ., 
                       data = data_train, 
                       proximity = TRUE)
predictions <- predict(rf_fit, data_test)

1 - var(data_test$age - predictions) / var(data_test$age)

vip(rf_fit)

linear_model <- lm(age ~ ., data = data_train)
lm_predictions <- predict(linear_model, newdata = data_test)
summary(linear_model)$r.squared
1 - var(data_test$age - lm_predictions) / var(data_test$age)

