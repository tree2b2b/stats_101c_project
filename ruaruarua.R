library(leaps)
library(glmnet)
library(ISLR)
library(mclust)
library(caret)
library(randomForest)


#---------------preprocessing----------------------------
raw_train<-read.csv("training.csv")
raw_test<-read.csv("test.csv")
train<-raw_train[,2:ncol(raw_train)]
test<-raw_test[,2:ncol(raw_test)]


X<-train[,2:ncol(train)]
y<-train$growth_2_6
corr<-cor(X,X$growth_2_6)
extra_kw_train<-read.csv("fixedtrain.csv")
extra_kw_test<-read.csv("fixedtest.csv")
time_train<-extra_kw_train[,(ncol(extra_kw_train)-4):ncol(extra_kw_train)]
time_test<-extra_kw_test[,(ncol(extra_kw_test)-4):ncol(extra_kw_test)]



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


X<-X[,-zero_pred]


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
XX<-(X[,(sumBS$which)[72,]])



set.seed(904971914)

i <- 1:dim(XX)[1] 
train_size = as.integer(nrow(XX)*0.7)
i_train <- sample(i, train_size, replace = FALSE)

X_train <-XX[i_train,]
X_val <-XX[-i_train,]
time_train1<-time_train[i_train,]
time_val<-time_train[-i_train,]

y_train<-X_train$growth_2_6
y_val<-X_val$growth_2_6

low_sig_pred<-c()
corr2<-cor(X_train,X_train$growth_2_6)
sum(abs(corr2)<0.09)

for(i in 1:ncol(X_train)){
  if(abs(corr2[i])<0.09){
    low_sig_pred<-c(low_sig_pred,i)
  }
  
}


X_train<-X_train[,-low_sig_pred]
X_train<-cbind(X_train,time_train1)

X_val<-X_val[,-low_sig_pred]
X_val<-cbind(X_val,time_val)

XX<-XX[,-low_sig_pred]
XX<-cbind(XX,time_train)



#---------------------training-------------------
oob_train_control <- trainControl(method="oob", 
                                  savePredictions = TRUE)


recommended.mtry <- floor(33)
tunegrid <- expand.grid(mtry=recommended.mtry)
#set.seed(904971914)
forestfit.m <- train(growth_2_6~., 
                     data = X_train, method = 'rf', importance = FALSE,
                     trControl = oob_train_control, tuneGrid = tunegrid) 

pred_train<-predict(forestfit.m, newdata=X_train)

sqrt(mean((y_train-pred_train)^2))

pred_val<-predict(forestfit.m, newdata=X_val)

sqrt(mean((y_val-pred_val)^2))



# using the whole set 
forestfit.whole <- train(growth_2_6~., 
                         data = XX, method = 'rf', importance = FALSE,
                         trControl = oob_train_control, tuneGrid = tunegrid) 


pred_train<-predict(forestfit.whole, newdata=XX)

sqrt(mean((Y-pred_train)^2))

#-------------------------------------------------

X_test<-test[,2:ncol(test)]
X_test<-X_test[,-zero_pred]
X_test<-X_test[,-high_cor_index]



X_test<-cbind(X_test,time_test)
pred_test<-predict(forestfit.whole, newdata=X_test) 

result2<-data.frame(id=raw_test[,1],growth_2_6 = pred_test)

#--------------------------compare-------------------------
benchmark_146<-read.csv("rf_146_benchmark.csv")
rf_33<-read.csv("rf_33_variables.csv")
rfr1<-read.csv("rfr1.csv")


sqrt(mean((benchmark_146$growth_2_6-pred_test)^2))
sqrt(mean((rf_33$growth_2_6-pred_test)^2))
sqrt(mean((rfr1$growth_2_6-pred_test)^2))




write.csv(result2,"bagging_33_variable_rua.csv",row.names =F)

#write.csv(XX,"33_variable_training.csv")


library(randomForest)
fit.RF <- randomForest(growth_2_6~., 
                             data = XX, mtry = 38,
                             ntree = 500, importance = TRUE) 
# This shows the Variable Importance V1, and the correct one
# according to the experts.
varImpPlot(fit.RF, type = 2, scale = F)















