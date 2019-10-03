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

data <- data %>%
  mutate(Landsize = ifelse(is.na(Landsize),0,Landsize))


year <- data %>% select(Postcodes,YearBuilt) %>% group_by(Postcodes)
year <- as.data.frame(table(year),stringsAsFactors = F) %>% arrange(desc(Freq)) %>% filter(Freq > 0)
year <- year  %>% mutate(YearBuilt = as.numeric(YearBuilt))
str(year$Postcodes)
View(year)



for(i in 1:length(data$YearBuilt)){
  if(is.na(data[i,"YearBuilt"])==T){
    data[i,"YearBuilt"] <- year[which(year$Postcodes==data[i,"Postcodes"]),"YearBuilt"][1]
  }
}




for(i in 1:2){
  if(is.na(s[i,"year"])==T){
    s[i,"year"] <- y[which(y$post==s[1,"post"]),"year"][1]
  }
}










glimpse(data)
View(data)

#convert categorical to numeric data
postcode <- table(data$Postcode)
nrow(postcode)

table(data$Bathroom)
table(data$Bedroom2)

sum(data$CouncilArea == "Other")
nrow(table(data$CouncilArea))




x <- data %>% select(Postcodes,YearBuilt) %>% group_by(Postcodes)
x <- as.data.frame(table(x)) %>% arrange(Freq) %>% filter(Freq > 0)
sum(is.na(data$BuildingArea))
sum(is.na(data$YearBuilt))
sum(is.na(data$BuildingArea))
nrow(data)

x <- data %>% select(Rooms,Car) %>% group_by(Rooms,Car)
x <- as.data.frame(table(x)) %>% arrange(Freq) %>% filter(Freq > 0)
View(x)

x <- data %>% select(Rooms,Landsize) %>% group_by(Rooms,Landsize)
x <- as.data.frame(table(x)) %>% arrange(Freq)
View(x)



table(data$Type)
table(data$Method)
x <- as.data.frame(table(data$YearBuilt)) %>% arrange(desc(F))



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