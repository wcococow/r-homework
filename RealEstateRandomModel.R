library(dplyr)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
library(cvTools)
library(randomForest)
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)
data.test$Price <- 0
data.train$data  <- 'train'
data.test$data <- 'test'
dataold <- rbind(data.train,data.test)
#impute
impute <- mice(dataold[,10:17], m = 3, seed = 123) 

p2 <- complete(impute, 1)
p1 <- dataold[,1:9]
data <- cbind(p1,p2)
str(data)
data <- data %>% select(-Suburb,-Address,-SellerG)
data$Postcode <- as.character(data$Postcode)
data <- CreateDummies(data ,"Postcode",93)
data <- CreateDummies(data ,"Type",2)
data <- CreateDummies(data ,"Method",4)
data$CouncilArea <- replace(data$CouncilArea,data$CouncilArea=="","Other")
data <- CreateDummies(data ,"CouncilArea",19)


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


data_train=data %>% filter(data=='train') %>% select(-data)
data_test=data %>% filter(data=='test') %>% select(-data,-Price)


any(is.na(data_train))
any(is.na(data_test)) 

set.seed(2)
s=sample(1:nrow(data_train),0.7*nrow(data_train))
data_train1=data_train[s,]
data_train2=data_train[-s,]





## function for getting all possible combinations : expand.grid
params=list(mtry=c(5,10),ntree=c(100,500),
            maxnodes=c(15,20),nodesize=(c(2,5)))

expand.grid(params)

## paramter values that we want to try out

param=list(mtry=c(5,10,15,20,25),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50),
           nodesize=c(1,2,5,10))


## Function for selecting random subset of params

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

## 

num_trials=50
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 50

## cvtuning for regression
## this code might take too long to run
## no need to execute completely in class
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,Price~.,
             data =data_train,
             tuning =params,
             folds = cvFolds(nrow(data_train), K=10, type = "random"),
             seed =2
  )
  score.this=k$cv[,2]
  
  if(score.this<myerror){
    print(params)
    # uncomment the line above to keep track of progress
    myerror=score.this
    print(myerror)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  print('DONE')
  # uncomment the line above to keep track of progress
}


























fit = randomForest(Price ~ ., data = trainf) 

train.predictions = predict(fit, newdata = trainf)

### Make predictions on test and submit 

test.predictions = predict(fit, newdata = testf)

write.csv(test.predictions,file = "file_name.csv", row.names = F)
