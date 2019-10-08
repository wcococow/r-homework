library(dplyr)
library(timeDate)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
library(caret)

#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)

#remove duplicated rows
cat("The number of duplicated rows are", nrow(data.train) - nrow(unique(data.train)))
data.train <- data.train %>% distinct

#remove outliers
data.train <- data.train %>% filter(data.train$BuildingArea<1000)

#combine train and test data
data.train$data  <- 'train'
data.test$Price <- 1
data.test$data <- 'test'
data <- rbind(data.train,data.test)

#impute remove NA
impute <- mice(data[,10:17], m = 3, seed = 123) 
p2 <- complete(impute, 1)
p1 <- data[,1:9]
data <- cbind(p1,p2)

numeric_var <- names(data)[which(sapply(data, is.numeric))]
#transform skew variables
skew <- sapply(numeric_var,function(x){skewness(data[[x]],na.rm = T)})
skew <- skew[skew > 0.75]
for(x in names(skew)) 
{
  data[[x]] <- log(data[[x]] + 1)
}

#dummy function
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

#drop character variables
data <- data %>% select(-Suburb,-Address,-SellerG)
#create dummy for character variables
data <- CreateDummies(data ,"Postcode",93)
data <- CreateDummies(data ,"Type",2)
data <- CreateDummies(data ,"Method",4)
data$CouncilArea <- replace(data$CouncilArea, data$CouncilArea == "", "Other")
data <- CreateDummies(data ,"CouncilArea",19)

#create train and test data
data_train=data %>% filter(data=='train') %>% select(-data)
data_test=data %>% filter(data=='test') %>% select(-data,-Price)


any(is.na(data_train))
any(is.na(data_test))

#modeling
set.seed(2)
s=sample(1:nrow(data_train),0.7*nrow(data_train))
data_train1=data_train[s,]
data_train2=data_train[-s,]

fit=lm(Price~.-CouncilArea_Other,data=data_train1)
summary(fit)
sort(vif(fit),decreasing = T)[1:3]
fit=lm(Price~.-Method_S-CouncilArea_Other-CouncilArea_GlenEira-CouncilArea_Brimbank-Rooms-CouncilArea_Moreland,data=data_train1)
fit=step(fit)
formula(fit)

plot(fit,which=1)
plot(fit,which=2)
plot(fit,which=3)
plot(fit,which=4)

predicted=predict(fit,newdata=data_train2)

RMSE=(exp(predicted)-exp(data_train2$Price))**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE

predicted=predict(fit,newdata=data_test)
write.csv(exp(predicted),'dong_xiaoyuan.csv',row.names = F)


