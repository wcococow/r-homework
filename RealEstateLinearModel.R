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

for(col in names(data)){
  
  if(sum(is.na(data[,col]))>0 & !(col %in% c("data","Price"))){
    
    data[is.na(data[,col]),col]=mean(data[data$data=='train',col],na.rm=T)
  }
  
}


data_train=data %>% filter(data=='train') %>% select(-data)
data_test=data %>% filter(data=='test') %>% select(-data,-Price)


any(is.na(data_train))
any(is.na(data_test))

set.seed(2)
s=sample(1:nrow(data_train),0.7*nrow(data_train))
data_train1=data_train[s,]
data_train2=data_train[-s,]

fit=lm(Price~.,data=data_train1)
summary(fit)
sort(vif(fit),decreasing = T)[1:3]
fit=lm(Price~.-Method_S-CouncilArea_Other-Rooms-Postcodes-Postcode_3039-CouncilArea_GlenEira-Postcode_3084-Postcode_3084-CouncilArea_PortPhillip-Postcode_3182-CouncilArea_Darebin-CouncilArea_Manningham-Postcode_3042,data=data_train)
fit=lm(Price~.-Method_S-CouncilArea_Other-Rooms-CouncilArea_GlenEira-Postcode_3039-Postcode_3084-Postcode_3013-Postcode_3182-Postcode_3070-Postcode_3042-CouncilArea_Monash-Postcode_3122-CouncilArea_Manningham-Postcode_3012-CouncilArea_Bayside-Postcode_3011-CouncilArea_Darebin-Postcode_3044-Postcode_3079-CouncilArea_Banyule,data=data_train1)



vif=step(fit)
plot(fit,which=1)
plot(fit,which=2)
plot(fit,which=3)
plot(fit,which=4)

predicted=predict(fit,newdata=data_train2)

RMSE=(predicted-data_train2$Price)**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE

predicted=predict(fit,newdata=data_test)
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


