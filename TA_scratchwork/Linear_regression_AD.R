setwd("~/Desktop/UCI/Summer 2023/DATA/slides/data")

library(tidyverse)
AD <- readr::read_csv("./data/alzheimer_data.csv")

typeof(AD$female)
