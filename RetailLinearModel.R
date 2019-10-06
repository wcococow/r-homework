library(dplyr)
library(tidyverse)
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

setwd("D:/edvancer/R project/Retail")

data.test <- read.csv("store_test.csv", stringsAsFactors = F)
data.train <- read.csv("store_train.csv", stringsAsFactors = F)

data.test$store <- NA
data.train$data  <- 'train'
data.test$data <- 'test'
data <- rbind(data.train,data.test)

data$country[which(is.na(data$country))] <-5

summary(data)
str(data)
data$population[which(data$Areaname=="Franklin County, ME" & data$countytownname == "Madrid town")] = 173
data$population[which(data$Areaname=="Washington County, ME" & data$countytownname == "Centerville town")] = 26
#countyname storecode Areaname countytownname state_alpha store_Type
data <- data %>% select(-countyname, -storecode, -Areaname, -countytownname)
data <- CreateDummies(data ,"store_Type",3)
data <- CreateDummies(data ,"state_alpha",53)

data.train=data %>% filter(data=='train') %>% select(-data)
data.test=data %>% filter(data=='test') %>% select(-data,-store)
