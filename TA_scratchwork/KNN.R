#the directory where your data is located on your computer
setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")

library(tidyverse)
library(class)
AD <- readr::read_csv("alzheimer_data.csv")

#plot some data to get a sense of how points relate to their neighbors
AD %>% 
  select(female, height, weight) %>% 
  mutate(sex = ifelse(female == 0, "Male", "Female")) %>% 
  ggplot() +
  geom_point(aes(x = height, y = weight, color = sex))

#make a smaller dataframe with a few normalized columns
AD_subset <- AD %>% 
  mutate(height_norm = (height - min(height)) / 
           (max(height) - min(height)),
         weight_norm = (weight - min(weight)) / 
           (max(weight) - min(weight)),
         sex = ifelse(female == 0, "Male", "Female")) %>% 
  select(sex, height_norm, weight_norm)

#plot again; it looks the same but axes are rescaled
AD_subset %>% 
  ggplot() +
  geom_point(aes(x = height_norm, 
                 y = weight_norm, 
                 color = sex))

#subset your data
#n_total is the total number of observations in your data
n_total <- nrow(AD_subset)
#n_train is how many observations will be in your training data (70%)
n_train <- floor(n_total * 0.7)
#the sample() function is how you randomly select n_train number of rows to include in training data 
train_indices <- sample(1:n_total, n_train, 
                        replace = FALSE)

#get your data into matrix form for the knn() function
data_train <- AD_subset %>% slice(train_indices) %>% 
  select(height_norm, weight_norm) %>% as.matrix()
data_test <- AD_subset %>% slice(-train_indices) %>% 
  select(height_norm, weight_norm) %>% as.matrix()
train_class <- AD_subset %>% slice(train_indices) %>% 
  select(sex) %>% as.matrix()
test_class <- AD_subset %>% slice(-train_indices) %>% 
  select(sex) %>% as.matrix()

#get predictions using the knn() function
predictions <- knn(train = data_train, 
                   test = data_test, 
                   cl = train_class, k = 5)

#summarize your predictions; the diagonal elements are correctly classified
#the off-diagonal are misclassifications
#Google "confusion matrix" if you don't know what it is
confusion_matrix <- table(predictions, test_class)

#calculate your prediction accuracy
accuracy <- sum(diag(confusion_matrix)) / 
  sum(confusion_matrix)


data.frame(data_test) %>% 
  mutate(correct = ifelse(predictions == test_class, 
                          "correct", "incorrect")) %>% 
  ggplot() +
  geom_point(aes(x = height_norm, y = weight_norm, 
                 color = correct)) +
  scale_colour_manual(values = c("black", "red"))


AD_subset <- AD %>% 
  mutate(has_ad = ifelse(diagnosis == 2, "Yes", "No"), 
         educ_norm = (educ - min(educ)) / (max(educ) - min(educ)),
         age_norm = (age - min(age)) / (max(age) - min(age)),
         bpsys_norm = (bpsys - min(bpsys)) / (max(bpsys) - min(bpsys))) %>% 
  select(has_ad, educ_norm, age_norm, bpsys_norm)
  

AD_subset %>% 
  ggplot() +
  geom_point(aes(x = educ_norm, y = bpsys_norm, color = has_ad))


data_train <- AD_subset %>% slice(train_indices) %>% 
  select(educ_norm, age_norm, bpsys_norm) %>% as.matrix()
data_test <- AD_subset %>% slice(-train_indices) %>% 
  select(educ_norm, age_norm, bpsys_norm) %>% as.matrix()
train_class <- AD_subset %>% slice(train_indices) %>% 
  select(has_ad) %>% as.matrix()
test_class <- AD_subset %>% slice(-train_indices) %>% 
  select(has_ad) %>% as.matrix()

predictions <- knn(train = data_train, 
                   test = data_test, 
                   cl = train_class, k = 5)

confusion_matrix <- table(predictions, test_class)

accuracy <- sum(diag(confusion_matrix)) / 
  sum(confusion_matrix)
