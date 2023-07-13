#this should be the directory where your alzheimer's data lives
setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")

#read in the data
AD <- readr::read_csv("alzheimer_data.csv")

#you need this package (library)
library(tidyverse)

#look at the dataframe
glimpse(AD)

#get a list of the column names; they both do the same thing
names(AD)
colnames(AD)

#view the data as a spreadsheet
View(AD)

#what are the dimensions of your dataframe
dim(AD)

#subset the columns of the data
select(AD, age, cdrglob)

select(AD, c(age, cdrglob))

AD %>% 
  select(age, cdrglob)

#exclude columns from your data
select(AD, -c(id, female))

AD %>% 
  select(-id, -female)

#get columns whose names satisfy a condition
AD %>% 
  select(ends_with("ort"))

#get a range of rows
AD %>% 
  slice(100:105) %>% 
  View()

#get the rows that satisfy a condition
AD %>% 
  filter(age == 90)

filter(AD, age == 90)

#be careful about logical statements that include NA
#the following will never return anything, even if there are rows with missing age
AD %>% 
  filter(age == NA)

#this is the correct way to filter your data to rows with missing age
#but there aren't any in this carefully currated data
AD %>% 
  filter(is.na(age))

#you can filter your data to only rows without missing values in age
AD %>% 
  filter(!is.na(age))

#an example of a compound codition using &
AD %>% 
  filter(age == 90 & female == 1)

AD %>% 
  filter(age >= 80 & cdrglob == 3.0)

#how many rows satisfy this condition
AD %>% 
  filter(age >= 80 & cdrglob == 3.0) %>% 
  count()

#nrow() will also give you a count
AD %>% 
  filter(age >= 80 & cdrglob == 3.0) %>% 
  nrow()

AD %>% 
  filter(cdrglob > 0 & cdrglob <= 1) %>% 
  nrow()

AD %>% 
  filter(cdrglob >= 2 & female == 1) %>% 
  nrow()

#overwrite your original data with a subset
AD <- AD %>% 
  select(age, diagnosis, female, bpsys, cdrglob)

#add a column to AD called "age_days"
#this won't permanently add it to AD unless you overwrite AD
mutate(AD, age_days = 365*age)

#overwrite AD and then confirm the new column is there
AD <-
  AD %>% 
  mutate(age_days = 365*age)
colnames(AD)

#get the median dementia score for males and females
#the "na.rm = TRUE" removes NA values from the median calculation
#but there aren't any in the data so it doesn't matter
AD %>% 
  group_by(female) %>% 
  summarize(med_cdrglob = median(cdrglob, na.rm = TRUE))

#get the median, mean of dementia score and the count
AD %>% 
  group_by(female) %>% 
  summarize(med_cdrglob = median(cdrglob),
            mean_cdrglob = mean(cdrglob),
            n_cdrglob = n())
