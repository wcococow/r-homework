library(dplyr)
library(tidyverse)


setwd("D:/edvancer/R project/Retail")

data.test <- read.csv("store_test.csv")
data.train <- read.csv("store_train.csv")

data.test$store <- NA
data.train$data  <- 'train'
data.test$data <- 'test'
data <- rbind(data.train,data.test)

data$country[which(is.na(data$country))] <-5

summary(data)

x <-data %>% filter(is.na(population))
data$population[which(data$Areaname=="Franklin County, ME" & data$countytownname == "Madrid town")] = 173
data$population[which(data$Areaname=="Washington County, ME" & data$countytownname == "Centerville town")] = 26