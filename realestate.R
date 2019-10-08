library(dplyr)
library(timeDate)
library(tidyverse)
library(tidyr)
library(car)
library(mice)
library(timeDate)
library('ggthemes')
library('corrplot')
library('gridExtra')
library(randomForest)
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data.train <- read.csv("housing_train.csv", stringsAsFactors = F)
data.test <- read.csv("housing_test.csv", stringsAsFactors = F)
#remove duplicated rows
cat("The number of duplicated rows are", nrow(data.train) - nrow(unique(data.train)))
data.train <- data.train %>% distinct

#data visual
data.train=data %>% filter(data=='train') %>% select(-data)

cat_var <- names(data.train)[which(sapply(data.train, is.character))]
cat_var<- cat_var[1:6]
numeric_var <- names(data.train)[which(sapply(data.train, is.numeric))]

train1_cat <- data.train[cat_var]

train1_var <- data.train[numeric_var]

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]],Price = data_in$Price)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(train1_cat, fun = plotHist, ii = 5:6, ncol = 2)
ggplot(data.train, aes(x = CouncilArea, y = Price)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous() 

doPlots(train1_var, fun = plotDen, ii = 1:4, ncol = 2)
doPlots(train1_var, fun = plotDen, ii = 5:10, ncol = 2)

correlations <- cor(na.omit(train1_var))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations, method="square")

summary(data.train$Price)
quantile(data.train$Price)
ggplot(data.train,aes(y=Price,x=BuildingArea))+geom_point()
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


skew <- sapply(numeric_var,function(x){skewness(data[[x]],na.rm = T)})
skew <- skew[skew > 0.75]

for(x in names(skew)) 
{
  data[[x]] <- log(data[[x]] + 1)
}

data <- data %>% select(-Suburb,-Address)

table(data$SellerG)


str(data)
data <- data %>%
  select(-SellerG)

data$Type <- factor(data$Type)
data$Method <- factor(data$Method)
data$CouncilArea <- factor(data$CouncilArea)

data_train=data %>% filter(data=='train') %>% select(-data)
data_test=data %>% filter(data=='test') %>% select(-data,-Price)

summary(data_train)

any(is.na(data_train))
any(is.na(data_test))


set.seed(2)
s=sample(1:nrow(data_train),0.7*nrow(data_train))
data_train1=data_train[s,]
data_train2=data_train[-s,]

## Final model with obtained best parameters
best_params<- data.frame(mtry=10,
                         ntree=200,
                         maxnodes=50,
                         nodesize=10)
ld.rf.final <- randomForest(Price~.,
                            mtry=best_params$mtry,
                            ntree=best_params$ntree,
                            maxnodes=best_params$maxnodes,
                            nodesize=best_params$nodesize,
                            data=data_train1)


importance <- importance(ld.rf.final)
varImpPlot(ld.rf.final)

test.pred=predict(ld.rf.final,newdata = data_train2)

RMSE=(exp(test.pred)-exp(data_train2$Price))**2 %>%
  mean() %>%
  sqrt()
RMSE
212467/RMSE

test.pred=predict(ld.rf.final,newdata = data_test)
write.csv(test.pred,"mysubmission.csv",row.names = F)





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
