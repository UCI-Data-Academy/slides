library(tidyverse)
AD <- read_csv("./alzheimer_data.csv") 

Walk <- as.data.frame(rbind(
  c("Morning",	"Sunny",	"Warm",	"Yes",	"Mild",	"Strong",	"Yes"),
  c("Evening",	"Rainy",	"Cold",	"No",	"Mild",	"Normal",	"No"),
  c("Morning",	"Sunny",	"Moderate",	"Yes",	"Normal",	"Normal",	"Yes"),
  c("Evening",	"Sunny",	"Cold",	"Yes",	"High",	"Strong",	"Yes")))
colnames(Walk) = c("Time",	"Weather",	"Temperature",	"Company",	"Humidity",	"Wind",	"Goes")

# Loops
walk_columns = colnames(Walk)
for(i in 1:length(walk_columns)){
  print(i)
}

for(i in walk_columns){
  print(i)
}

# Data Wrangling

# first, let's subset the data and choose which categorical variables we want to include and which rows we want to include
AD_subset <- AD %>% 
  select(diagnosis, travel, bills, taxes) %>% 
  mutate(has_ad = ifelse(diagnosis == 2, "AD", "No AD"),
         travel = as.factor(travel), 
         bills = as.factor(bills), 
         taxes = as.factor(taxes)) %>% 
  slice(1:20)

# the attributes
data <- AD_subset[,2:4]
data

# the target
target <- AD_subset %>% 
  pull(has_ad) 
target



# Find-S Algorithm
finds <- function(data, target, positive_factor){
  for (i in 1:length(target)){
    if (target[i] == positive_factor){
      specific_hypothesis = data[i,]
      break
    }
  }
  for (i in 1:length(target)){
    if (target[i] == positive_factor){
      for (j in 1:length(specific_hypothesis)){
        if (data[i,j] != specific_hypothesis[j]){
          specific_hypothesis[j] = '?'
        }
      }
    }
  }
  return(specific_hypothesis)
}

finds(data, target, "AD")

walk_target <- Walk$Goes
walk_data <- Walk[,1:6]
finds(walk_data, walk_target, "Yes")


# Candidate-Elimination Algorithm

cea <- function(data, target, positive_factor){
  specific_h <- as.matrix(data[1,])
  general_h <- matrix(rep("?", length(specific_h)*length(specific_h)), 
                      nrow = length(specific_h),
                      ncol = length(specific_h))
  for (i in 1:length(target)){
    if (target[i] == positive_factor){
      for (j in 1:length(specific_h)){
        if (data[i,j] != specific_h[j]){
          specific_h[j] = "?"
          general_h[j,j] = "?"
        }
      }
    }
    if (target[i] != positive_factor){
      for (j in 1:length(specific_h)){
        if (data[i,j] != specific_h[j]){
          general_h[j,j] = specific_h[j]
        }
        else {
          general_h[j,j] = "?"
        }
      }
    }
  }
  print(specific_h)
  return(general_h)
}

cea(data, target, "AD")

cea(walk_data, walk_target, "Yes")