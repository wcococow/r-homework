library(dplyr)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)
data.test$Price <- 0
data.train$data  <- 'train'
data.test$data <- 'test'
dataold <- rbind(data.train,data.test)

pairs(data[,c("Price","Rooms")])

View(dataold)
summary(data)
#missing data

p <- function(x){
  sum(is.na(x))/length(x)*100
}


apply(data,2,p)
md.pattern(data)
#impute
impute <- mice(dataold[,10:17], m = 3, seed = 123) 

p2 <- complete(impute, 1)
p1 <- dataold[,1:9]
data <- cbind(p1,p2)
head(data)
md.pattern(data)

str(data)
data <- data %>% select(-Suburb,-Address)

data <- CreateDummies(data ,"Postcode",93)
data <- CreateDummies(data ,"Type",2)
data <- CreateDummies(data ,"Method",4)

data$CouncilArea <- replace(data$CouncilArea,data$CouncilArea=="","Other")
data <- CreateDummies(data ,"CouncilArea",19)






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

fitfull=lm(Price~.,data=data_train1)
summary(fit)
sort(vif(fit),decreasing = T)[1:3]
fit=lm(Price~.-Method_S-CouncilArea_Other-Postcode_3039-CouncilArea_Bayside-CouncilArea_Darebin-CouncilArea_Manningham-CouncilArea_HobsonsBay-Postcode_3070-Postcode_3182-Postcode_3042-Postcode_3084-Postcode_3079-CouncilArea_Brimbank-CouncilArea_Banyule-Postcode_3122-Postcode_3068-Postcode_3121,data=data_train1)


fit=step(fit)
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

anova(fit,fitfull)
predicted=predict(fit,newdata=data_test)
write.csv(predicted,'dong_xiaoyuan.csv',row.names = F)

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


