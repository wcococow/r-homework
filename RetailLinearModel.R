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


x <- data.train %>%  
  mutate(price=sales0+sales1+sales2+sales3+sales4)
sum(x$price)
nrow(table(data.train$Areaname))


outlier_search <- function(fnlwgt,rang){
  s <- quantile(fnlwgt,c(0.25,0.75))
  inq <- IQR(fnlwgt)
  low <- s[1]-rang*inq
  high <- s[2]+rang*inq
  print("Outlier Limits For fnlwgt are")
  print(paste(low,high))
  s <- fnlwgt[fnlwgt >high | fnlwgt < low]
  print("Number of outliers according to these limits for fnlwgt:")
  print(length(s))
  
}
x <- x  %>% group_by(store_Type) %>% summarise(vp=var(price)) 
  

outlier_search(x$price,1.5)



#countyname storecode Areaname countytownname state_alpha store_Type
data <- data %>% select(-countyname, -storecode, -Areaname, -countytownname)
data <- CreateDummies(data ,"store_Type",3)
data <- CreateDummies(data ,"state_alpha",53)

data.train=data %>% filter(data=='train') %>% select(-data)
data.test=data %>% filter(data=='test') %>% select(-data,-store)
