library(leaps)
library(glmnet)
library(ISLR)
library(mclust)
library(caret)
library(randomForest)
library(stringr)

#Fix publish time - In this part, month, date, time, and weekday of each submission is calculated
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

#---------------preprocessing----------------------------
timestrain <- train[,261:265]   #save the time related predictors for later
X <- train[,3:260]
Y <- train$growth_2_6
corr<-cor(X,X$growth_2_6)

#find and get rid of zero correlated predictors
zero_pred<-c()
for (i in 1:dim(X)[2]){
  if(is.na(corr[i])){
    zero_pred<-c(zero_pred,i)
  }
}
X<-X[,-zero_pred]

#get rid of highly correlated predictors
high_cor_index<-c()
for(i in 1:dim(X)[2]){
  for (j in i:dim(X)[2]){
    t<-cor(X[,i],X[,j])
    if(t>=0.8 && i!=j){
      high_cor_index<-c(high_cor_index,j)
    }
  }
}
high_cor_index<-unique(high_cor_index)
X<-X[,-high_cor_index]
#X <- cbind(X, timestrain)

#Use regular subsets to find potential useful predictors for this problem
rsub <- regsubsets(growth_2_6~., data = X, nbest = 1, nvmax = 100,
                   intercept = TRUE, method = "backward",
                   really.big = T)
sumBS <- summary(rsub)

renew<-((sumBS$which)[72,])[2:length(sumBS$which[72,])]

XX<-(X[,(sumBS$which)[72,]])
XX<-(X[,renew])



set.seed(904971914)
low_sig_pred<-c()
corr2<-cor(XX,XX$growth_2_6)
sum(abs(corr2)<0.09)

for(i in 1:ncol(XX)){
  if(abs(corr2[i])<0.09){
    low_sig_pred<-c(low_sig_pred,i)
  }
}

XX <- cbind(XX[,-low_sig_pred], timestrain)


#---------------------training-------------------
oob_train_control <- trainControl(method="oob", 
                                  savePredictions = TRUE)


recommended.mtry <- floor(34)
recommended.mtry <- floor(38)

tunegrid <- expand.grid(mtry=recommended.mtry)
# using the whole set 
forestfit.whole <- train(growth_2_6~., 
                         data = XX, method = 'rf', importance = FALSE,
                         trControl = oob_train_control, tuneGrid = tunegrid) 


#--------------------Predicting
pred_test<-predict(forestfit.whole, newdata=test) 

result2<-data.frame(id=test[,1],growth_2_6 = pred_test)

#--------------------------compare-------------------------
write.csv(result2,"tryrua2.csv",row.names =F)









X[,1:10]









