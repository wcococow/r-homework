library(dplyr)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
library(gbm)
library(cvTools)
library(timeDate)

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

## -----------------------------------------------------


param=list(interaction.depth=c(1:7),
           n.trees=c(50,100,200,500,700),
           shrinkage=c(.1,.01,.001),
           n.minobsinnode=c(1,2,5,10))

## ------------------------------------------------------------------------
subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}

num_trials=10
my_params=subset_paras(param,num_trials)
# Note: A good value for num_trials is around 10-20% of total possible 
# combination. It doesnt have to be always 10

## ---- this code might take too long to run--------------------------------------------------------------
myerror=9999999

for(i in 1:num_trials){
  print(paste0('starting iteration:',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
 
  k=cvTuning(gbm,Price~.,
             data =data_train1,
             tuning =params,
             args = list(distribution="gaussian"),
             folds = cvFolds(nrow(data_train1), K=10, type = "random"),
             seed =2,
             predictArgs = list(n.trees=params$n.trees)
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

## ----these are the values from a previous run--------------------------------------------------------------
myerror=20.84
best_params=data.frame(interaction.depth=2,
                       n.trees=700,
                       shrinkage=0.1,
                       n.minobsnode=5)

## ------------------------------------------------------------------------
myerror

## ------------------------------------------------------------------------
best_params

## ------------------------------------------------------------------------
bs.gbm.final=gbm(Price~.-CouncilArea_Other,data=data_train1,
                 n.trees = best_params$n.trees,
                 n.minobsinnode = best_params$n.minobsnode,
                 shrinkage = best_params$shrinkage,
                 interaction.depth = best_params$interaction.depth,
                 distribution = "gaussian")


predicted=predict(bs.gbm.final,newdata=data_train2,n.trees = best_params$n.trees)

RMSE=(exp(predicted)-exp(data_train2$Price))**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE



test.pred=predict(bs.gbm.final,newdata=data_test,n.trees = best_params$n.trees)

write.csv(exp(test.pred),"mysubmissionboost.csv",row.names = F)
























sort(vif(fit),decreasing = T)[1:3]
fit=lm(Price~.-Method_S-CouncilArea_Other-Rooms,data=data_train1)


fit=step(fit)
formula(fit)
plot(fit,which=1)
plot(fit,which=2)
plot(fit,which=3)
plot(fit,which=4)

predicted=predict(fit,newdata=data_train2)
cor(data_train2$Price,exp(predicted))

RMSE=(exp(predicted)-exp(data_train2$Price))**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE


predicted=predict(fit,newdata=data_test)
write.csv(exp(predicted),'dong_xiaoyuan2.csv',row.names = F)

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


