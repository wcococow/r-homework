library(dplyr)
library(tidyverse)
library(car)
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)

data.test$Price <- NA
data.train$data  <- 'train'
data.test$data <- 'test'
data <- rbind(data.train,data.test)

data <- data %>%
  mutate(Bedroom2 = ifelse(is.na(Bedroom2),Rooms,Bedroom2))


data <- data %>% mutate(Bathroom=ifelse(is.na(Bathroom) & Rooms==1,1,Bathroom))
data <- data %>% mutate(Bathroom=ifelse(is.na(Bathroom) & Rooms==2,1,Bathroom))
data <- data %>% mutate(Bathroom=ifelse(is.na(Bathroom) & Rooms==3,1,Bathroom))
data <- data %>% mutate(Bathroom=ifelse(is.na(Bathroom) & Rooms==4,2,Bathroom))
data <- data %>% mutate(Bathroom=ifelse(is.na(Bathroom) & Rooms==5,3,Bathroom))


data <- data %>% mutate(Car=ifelse(is.na(Car) & Rooms==1,1,Car))
data <- data %>% mutate(Car=ifelse(is.na(Car) & Rooms==2,1,Car))
data <- data %>% mutate(Car=ifelse(is.na(Car) & Rooms==3,2,Car))
data <- data %>% mutate(Car=ifelse(is.na(Car) & Rooms==4,2,Car))
data <- data %>% mutate(Car=ifelse(is.na(Car) & Rooms==5,2,Car))

data <- data %>% select(-BuildingArea)
data <- data %>%
  mutate(Postcodes = as.numeric(Postcode)
  ) %>%
  select(-Suburb,-Address)

data <- CreateDummies(data ,"Postcode",93)
data <- CreateDummies(data ,"Type",2)
data <- CreateDummies(data ,"Method",4)

data$CouncilArea <- replace(data$CouncilArea,data$CouncilArea=="","Other")

data <- CreateDummies(data ,"CouncilArea",19)


glimpse(data)
View(data)

#convert categorical to numeric data
postcode <- table(data$Postcode)
nrow(postcode)

table(data$Bathroom)
table(data$Bedroom2)

sum(data$CouncilArea == "Other")
nrow(table(data$CouncilArea))






x <- data %>% select(Rooms,Bathroom) %>% group_by(Rooms,Bathroom)
x <- as.data.frame(table(x)) %>% arrange(Freq)
sum(is.na(data$BuildingArea))
sum(is.na(data$YearBuilt))
sum(is.na(data$BuildingArea))
nrow(data)

x <- data %>% select(Rooms,Car) %>% group_by(Rooms,Car)
x <- as.data.frame(table(x)) %>% arrange(Freq)
View(x)

x <- data %>% select(Rooms,Landsize) %>% group_by(Rooms,Landsize)
x <- as.data.frame(table(x)) %>% arrange(Freq)
View(x)



table(data$Type)
table(data$Method)




CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}