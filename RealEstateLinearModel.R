library(dplyr)
library(tidyverse)
library(car)
#read csv file
setwd("D:/edvancer/R project/Real Estate")
data <- read.csv("OnlineNewsPopularity.csv", stringsAsFactors = F)
glimpse(data)
sum(is.na(data))
#convert categorical to numeric data
table(data$weekday)
data <- data %>%
  mutate(wd.friday = as.numeric(weekday == "Friday"),
         wd.monday = as.numeric(weekday == "Monday"),
         wd.sunday = as.numeric(weekday == "Sunday"),
         wd.thursday = as.numeric(weekday == "Thursday"),
         wd.tuesday = as.numeric(weekday == "Tuesday"),
         wd.wednesday = as.numeric(weekday == "Wednesday")
  ) %>%
  select(-weekday)

table(data$data_channel)
data <- data %>%
  mutate(
    dc.bus = as.numeric(data_channel=="business"),
    dc.ent=as.numeric(data_channel=="entertainment"),
    dc.life=as.numeric(data_channel=="lifestyle"),
    dc.socmed=as.numeric(data_channel=="socmed"),
    dc.tech=as.numeric(data_channel=="tech"),
    dc_world=as.numeric(data_channel=="world")
  ) %>%
  select(-data_channel)

words<-c("google","twitter","facebook","apple",
         "video","new","app","iphone","social","world","2014")
for(word in words){
  data[,paste0("word_",word)]=NA
  for(i in 1:nrow(data)){
    data[,paste0("word_",word)][i]=as.numeric(length(grep(word,data$url[i]))>0)
  }
}
data=data[,-1]

#build model
set.seed(2)
s <- sample(1:nrow(data), 0.8*nrow(data))
data_train <- data[s,]
data_test <- data[-s,]
fit <- lm(shares~. , data=data_train)
sort(vif(fit), decreasing = T)
#drop High VIF till VIF < 5
fit<-lm(shares~.-LDA_03,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min-kw_avg_avg,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min-kw_avg_avg-rate_negative_words,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min-kw_avg_avg-rate_negative_words-avg_negative_polarity,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min-kw_avg_avg-rate_negative_words-avg_negative_polarity-dc_world,data=data_train)
sort(vif(fit),decreasing = T)[1:10]
fit<-lm(shares~.-LDA_03-n_unique_tokens-n_non_stop_words-self_reference_avg_sharess-rate_positive_words-kw_avg_min-kw_avg_avg-rate_negative_words-avg_negative_polarity-dc_world-global_sentiment_polarity,data=data_train)
sort(vif(fit),decreasing = T)[1:10]

data_train <-data_train %>%
  select(-LDA_03,
         -n_unique_tokens,
         -n_non_stop_words,
         -self_reference_avg_sharess,
         -rate_positive_words,
         -kw_avg_min,
         -kw_avg_avg,
         -rate_negative_words,
         -avg_negative_polarity,
         -dc_world,
         -global_sentiment_polarity)

fit<-lm(shares~.,data=data_train)
summary(fit)

#check AIC value
fit<-step(fit)

summary(fit)
#drop p value > 0.1

fit <- lm(formula = shares ~ n_tokens_title + num_keywords + kw_min_min + 
            kw_min_avg + kw_max_avg + self_reference_min_shares + LDA_02 + 
            LDA_04 + global_subjectivity + global_rate_positive_words + 
            min_positive_polarity + max_negative_polarity + abs_title_subjectivity + 
            nnsw_share + ntt_share + mnp_share + nv_shares + minnp_shares + 
            nsh_shares + num_im_shares + num_hrefs_shares + sp_feat1 + 
            wd.thursday + dc.ent + word_facebook + word_apple + word_app + 
            word_iphone + word_2014, data = data_train)


fit <- lm(formula = shares ~ num_keywords + kw_min_min + 
            kw_min_avg + kw_max_avg + self_reference_min_shares + LDA_02 + 
            LDA_04 + global_subjectivity + global_rate_positive_words + 
            min_positive_polarity + max_negative_polarity + abs_title_subjectivity + 
            nnsw_share + ntt_share + mnp_share + nv_shares + minnp_shares + 
            nsh_shares + num_im_shares + num_hrefs_shares + sp_feat1 + 
            wd.thursday + dc.ent + word_facebook + word_apple + word_app + 
            word_iphone + word_2014, data = data_train)

fit <- lm(formula = shares ~ num_keywords + kw_min_min + 
            kw_min_avg + kw_max_avg + self_reference_min_shares + LDA_02 + 
            LDA_04 + global_subjectivity + global_rate_positive_words + 
            min_positive_polarity  + abs_title_subjectivity + 
            nnsw_share + ntt_share + mnp_share + nv_shares + minnp_shares + 
            nsh_shares + num_im_shares + num_hrefs_shares + sp_feat1 + 
            wd.thursday + dc.ent + word_facebook + word_apple + word_app + 
            word_iphone + word_2014, data = data_train)

fit <- lm(formula = shares ~ num_keywords + kw_min_min + 
            kw_min_avg + kw_max_avg + self_reference_min_shares + LDA_02 + 
            LDA_04 + global_subjectivity + global_rate_positive_words + 
            min_positive_polarity  + abs_title_subjectivity + 
            nnsw_share + ntt_share + mnp_share + nv_shares + minnp_shares + 
            nsh_shares + num_im_shares + num_hrefs_shares + sp_feat1 + 
            wd.thursday + dc.ent  + word_apple + word_app + 
            word_iphone + word_2014, data = data_train)

fit <- lm(formula = shares ~ num_keywords  + 
            kw_min_avg + kw_max_avg + self_reference_min_shares + LDA_02 + 
            LDA_04 + global_subjectivity + global_rate_positive_words + 
            min_positive_polarity  + abs_title_subjectivity + 
            nnsw_share + ntt_share + mnp_share + nv_shares  + 
            nsh_shares + num_im_shares + num_hrefs_shares + sp_feat1 + 
            wd.thursday + dc.ent  + word_apple  , data = data_train)


plot(fit, which=1)
plot(fit,which=2)
hist(fit$residuals,breaks=1000)
plot(fit,which=3)
plot(fit,which=4)
nrow(data_train)
View(data_train)
x <- data_train[rownames(data_train) %in% c(23136,26577),]

data_train <- anti_join(data_train,x)

predicted=predict(fit,newdata=data_test)
RMSE=(predicted-data_test$shares)**2 %>%
  mean() %>%
  sqrt()
Score =212467/RMSE 