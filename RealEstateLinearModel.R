library(dplyr)
library(tidyverse)
library(tidyr)
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
  mutate(Postcodes = as.numeric(Postcode),
         YearBuilt = as.numeric(YearBuilt)
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


for(i in 1:length(data$YearBuilt)){
  if(is.na(data[i,"YearBuilt"])==T){
    data[i,"YearBuilt"] <- year[which(year$Postcodes==data[i,"Postcodes"]),"YearBuilt"][1]
  }
}

data <- data %>%
  mutate(YearBuilt = as.numeric(YearBuilt)
  )


data <- data %>%
  select(-SellerG)

nrow(data_test)

data %>% drop_na()


data_train=data %>% filter(data=='train') %>% select(-data)
data_test=data %>% filter(data=='test') %>% select(-data,-Price)

vif=lm(Price~.,data=data_train)
summary(vif)
sort(vif(vif),decreasing = T)[1:3]
vif=lm(Price~.-Method_S-CouncilArea_Other-Rooms-Postcodes-Postcode_3039-CouncilArea_GlenEira-Postcode_3084-Postcode_3084-CouncilArea_PortPhillip-Postcode_3182-CouncilArea_Darebin-CouncilArea_Manningham-Postcode_3042,data=data_train)



vif=step(vif)
plot(vif,which=1)
plot(vif,which=2)
plot(vif,which=3)
plot(vif,which=4)

predicted=predict(vif,newdata=data_test)
write.csv(predicted,'proper_name.csv',row.names = F)

data %>% select(which(is.na(data)))


glimpse(data)
View(data)

#convert categorical to numeric data
postcode <- table(data$Postcode)
nrow(postcode)

table(data$Bathroom)
table(data$Bedroom2)

sum(data$CouncilArea == "Other")
nrow(table(data.train$CouncilArea))




x <- data %>% select(Postcodes,YearBuilt) %>% group_by(Postcodes)
x <- as.data.frame(table(x)) %>% arrange(Freq) %>% filter(Freq > 0)
sum(is.na(data$BuildingArea))
sum(is.na(data$YearBuilt))
sum(is.na(data$Rooms))
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


############
sum(is.na(data.train$YearBuilt))
h <- data.train %>% filter(Type=='h') %>% select(Price) %>% unlist


t <- data.train %>% filter(Type=='t') %>% select(Price) %>% unlist

mean(h) - mean(t)

data.train %>% select(Postcode) %>% unique %>% nrow
data.train$CouncilArea <- replace(data.train$CouncilArea,data.train$CouncilArea=="","Other")
data.train %>% group_by(CouncilArea) %>% summarise(PriceAvg=mean(Price)) %>% arrange(desc(PriceAvg))



hist(data.train$Distance)

data.train %>% group_by(SellerG) %>% summarise(PriceTotal=sum(Price)) %>% arrange(desc(PriceTotal))
data.train %>% group_by(CouncilArea) %>% summarise(PriceVar=var(Price)) %>% arrange(desc(PriceVar))


