library(leaps)
library(glmnet)
library(ISLR)
library(mclust)
library(caret)
library(randomForest)
library(stringr)

##########fix date and time
temp <- read.csv("training.csv")

#variables are in form of "4/17/2020 10:38"
#exclude years
year <- str_extract_all(temp$PublishedDate, "(?<=\\/)\\d*(?=\\s)") %>% unlist() %>% as.numeric()
summary(year)

#date only from APril to Sept
month <- str_extract_all(temp$PublishedDate, "^\\d*(?=\\/)") %>% unlist() %>% as.numeric()
summary(month)
date <- str_extract_all(temp$PublishedDate, "(?<=\\/)\\d*(?=\\/)") %>% unlist() %>% as.numeric()
summary(day)
day <- date
for(i in 1:nrow(temp)){
  if(month[i] == 5)
    day[i] <- day[i]+30
  if(month[i] == 6)
    day[i] <- day[i]+30+31
  if(month[i] == 7)
    day[i] <- day[i]+30+31+30
  if(month[i] == 8)
    day[i] <- day[i]+30+31+30+31
  if(month[i] == 9)
    day[i] <- day[i]+30+31+30+31+31
}
temp$day <- day
temp$month <- month
temp$date <- date

#time, hour and minute into decimals
hour <- str_extract_all(temp$PublishedDate, "(?<=\\s)\\d*(?=\\:)") %>% unlist() %>% as.numeric()
minute <- str_extract_all(temp$PublishedDate, "(?<=\\:)\\d*$") %>% unlist() %>% as.numeric()
temp$pTime <- hour + minute/60

weekday <- rep("NA", nrow(temp))
#apr.1 is wed
for(i in 1:nrow(temp)){
  if(day[i]%%7 == 1)
    weekday[i] <- "Wed"
  if(day[i]%%7 == 2)
    weekday[i] <- "Thu"
  if(day[i]%%7 == 3)
    weekday[i] <- "Fri"
  if(day[i]%%7 == 4)
    weekday[i] <- "Sat"
  if(day[i]%%7 == 5)
    weekday[i] <- "Sun"
  if(day[i]%%7 == 6)
    weekday[i] <- "Mon"
  if(day[i]%%7 == 0)
    weekday[i] <- "Tue"
}
temp$weekday <- weekday
train <- temp


#-------------Fix test data
temp <- read.csv("test.csv")

#variables are in form of "4/17/2020 10:38"
#exclude years
year <- str_extract_all(temp$PublishedDate, "(?<=\\/)\\d*(?=\\s)") %>% unlist() %>% as.numeric()
summary(year)

#date only from APril to Sept
month <- str_extract_all(temp$PublishedDate, "^\\d*(?=\\/)") %>% unlist() %>% as.numeric()
summary(month)
date <- str_extract_all(temp$PublishedDate, "(?<=\\/)\\d*(?=\\/)") %>% unlist() %>% as.numeric()
summary(day)
day <- date
for(i in 1:nrow(temp)){
  if(month[i] == 5)
    day[i] <- day[i]+30
  if(month[i] == 6)
    day[i] <- day[i]+30+31
  if(month[i] == 7)
    day[i] <- day[i]+30+31+30
  if(month[i] == 8)
    day[i] <- day[i]+30+31+30+31
  if(month[i] == 9)
    day[i] <- day[i]+30+31+30+31+31
}
temp$day <- day
temp$month <- month
temp$date <- date

#time, hour and minute into decimals
hour <- str_extract_all(temp$PublishedDate, "(?<=\\s)\\d*(?=\\:)") %>% unlist() %>% as.numeric()
minute <- str_extract_all(temp$PublishedDate, "(?<=\\:)\\d*$") %>% unlist() %>% as.numeric()
temp$pTime <- hour + minute/60

weekday <- rep("NA", nrow(temp))
#apr.1 is wed
for(i in 1:nrow(temp)){
  if(day[i]%%7 == 1)
    weekday[i] <- "Wed"
  if(day[i]%%7 == 2)
    weekday[i] <- "Thu"
  if(day[i]%%7 == 3)
    weekday[i] <- "Fri"
  if(day[i]%%7 == 4)
    weekday[i] <- "Sat"
  if(day[i]%%7 == 5)
    weekday[i] <- "Sun"
  if(day[i]%%7 == 6)
    weekday[i] <- "Mon"
  if(day[i]%%7 == 0)
    weekday[i] <- "Tue"
}
temp$weekday <- weekday
test <- temp


#---------------Data Processing
train <- train[,-c(1,2)]

X <- train[,-263]

corr <- cor(X, X$growth_2_6)

zero_pred<-c()
for (i in 1:dim(X)[2]){
  if(is.na(corr[i])){
    print(i)
    print(sum(X[,i]))
    print("NA")
    zero_pred<-c(zero_pred,i)
  }
  else if(abs(corr[i])>0.5){
    print(i)
  }
}

X <- X[,-zero_pred]

high_cor_index<-c()

for(i in 1:dim(X)[2]){
  for (j in i:246){
    t<-cor(X[,i],X[,j])
    if(t>=0.8 && i!=j){
      print(i)
      print(j)
      print("found")
      high_cor_index<-c(high_cor_index,j)
    }
  }
}

high_cor_index<-unique(high_cor_index)

X<-X[,-high_cor_index]

rsub <- regsubsets(growth_2_6~., data = X, nbest = 1, nvmax = 100,
                   intercept = TRUE, method = "backward",
                   really.big = T)

sumBS <- summary(rsub)
XX<-cbind((X[,(sumBS$which)[72,]]), train$weekday)

#---------------------training-------------------
oob_train_control <- trainControl(method="oob", 
                                  savePredictions = TRUE)


recommended.mtry <- floor(33)
tunegrid <- expand.grid(mtry=recommended.mtry)
set.seed(904971914)
# using the whole set 
forestfit.whole <- train(growth_2_6~., 
                         data = XX, method = 'rf', importance = FALSE,
                         trControl = oob_train_control, tuneGrid = tunegrid) 


pred_train<-predict(forestfit.whole, newdata=XX)

#-------------------------------------------------
colnames(test)[264] <- "train$weekday"
pred_test<-predict(forestfit.whole, newdata=test) 

result2<-data.frame(id=test[,1],growth_2_6 = pred_test)

write.csv(result2,"try2.csv",row.names =F)



























