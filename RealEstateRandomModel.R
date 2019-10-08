library(dplyr)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
library(cvTools)
library(randomForest)
library(party)
library(caret)
library(timeDate)
#read csv file
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)
#remove duplicated rows
cat("The number of duplicated rows are", nrow(data.train) - nrow(unique(data.train)))
data.train <- data.train %>% distinct
#remove outliers
data.train <- data.train %>% filter(data.train$BuildingArea<1000)

data.train$data  <- 'train'
data.test$Price <- 1
data.test$data <- 'test'
data <- rbind(data.train,data.test)
#impute
impute <- mice(data[,10:17], m = 3, seed = 123) 

p2 <- complete(impute, 1)
p1 <- data[,1:9]
data <- cbind(p1,p2)

numeric_var <- names(data)[which(sapply(data, is.numeric))]
skew <- sapply(numeric_var,function(x){skewness(data[[x]],na.rm = T)})
skew <- skew[skew > 0.75]

for(x in names(skew)) 
{
  data[[x]] <- log(data[[x]] + 1)
}

#drop character variables
data <- data %>% select(-Suburb,-Address,-SellerG)
#transfer to factors
data$Type <- factor(data$Type)
data$Method <- factor(data$Method)
data$CouncilArea <- replace(data$CouncilArea, data$CouncilArea == "", "Other")
data$CouncilArea <- factor(data$CouncilArea)




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


str(data)
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


# myerror=1.870957
best_params<- data.frame(mtry=10,
                       ntree=200,
                       maxnodes=50,
                       nodesize=10)

## Final model with obtained best parameters

View(data_train1)

ld.rf.final <- randomForest(Price~.,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=data_train1)

plot(ld.rf.final)
varImpPlot(ld.rf.final)
print(ld.rf.final)


test.pred=predict(ld.rf.final,newdata = data_train2)

RMSE=(exp(test.pred)-exp(data_train2$Price))**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE


test.pred=predict(ld.rf.final,newdata = data_test)
write.csv(exp(test.pred),"mysubmissiondong.csv",row.names = F)



