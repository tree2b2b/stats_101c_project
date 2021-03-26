library(car)
library(glmnet)
library(pls)
library(caret)
library(MASS)
library(tree)
library(randomForest)

train <- read.csv("fixedtrain.csv")

set.seed(199)
trind <- sample(nrow(train),5000)
tr <- train[trind, ]
te <- train[-trind, ]
tr <- tr[,-c(1,2)]
te <- te[,-c(1,2)]

####1#####simple linear regression
#1.678296
m1 <- lm(growth_2_6 ~ ., data = tr)
pred <- predict(m1, te)
sqrt(mean((te$growth_2_6-pred)^2))

##########ridge
#1.672623
x <- model.matrix(growth_2_6~., tr[,-2])[,-1]
y <- tr$growth_2_6
testx <- model.matrix(growth_2_6~., te[,-2])[,-1]
testy <- te$growth_2_6
i.exp <- seq(10, -2, length = 100)
grid <- 10^i.exp
set.seed(199)
m2 <- cv.glmnet(x, y, family = "gaussian", alpha = 0, lambda = grid, standardize = TRUE)
plot(m2)
best.lambda <- m2$lambda.min
pred <- predict(m2, newx = testx, s = best.lambda)
sqrt(mean((te$growth_2_6-pred)^2))

########lasso
#1.667152
x <- model.matrix(growth_2_6~., tr[,-2])[,-1]
y <- tr$growth_2_6
testx <- model.matrix(growth_2_6~., te[,-2])[,-1]
testy <- te$growth_2_6
i.exp <- seq(10, -2, length = 100)
grid <- 10^i.exp
set.seed(199)
m3 <- cv.glmnet(x, y, family = "gaussian", alpha = 1, lambda = grid, standardize = TRUE)
best.lambda <- m3$lambda.min
pred <- predict(m3, newx = testx, s = best.lambda)
sqrt(mean((te$growth_2_6-pred)^2))

#######PCR
#1.666093
m4 <- pcr(growth_2_6~., data = tr[,-2], validation = "CV")  ####scale有问题 么有scale
summary(m4)
validationplot(m4, val.type = "MSEP", main = "MSEP")
#M = 112
pred <- predict(m4, te, ncomp = 112)
sqrt(mean((te$growth_2_6-pred)^2))

######PLS
#1.670277
m5 <- plsr(growth_2_6~., data = tr[, -2], validation = "CV")
summary(m5)
#M = 48
pred <- predict(m5, te, ncomp = 48)
sqrt(mean((te$growth_2_6-pred)^2))

####regression tree
#1.86155
set.seed(199)
m6 <- tree(growth_2_6~., data = tr[,-2])
summary(m6)
pred <- predict(m6, te)
sqrt(mean((te$growth_2_6-pred)^2))


#################Bagging/rf
#1.745059
set.seed(199)
m7 <- randomForest(growth_2_6~., data = tr, mtry = 10)
pred <- predict(m7, newdata = te)
sqrt(mean((te$growth_2_6-pred)^2))

#bagging
#1.521873
set.seed(199)
m8 <- randomForest(growth_2_6~., data = tr, mtry = 262)
pred <- predict(m8, newdata = te)
sqrt(mean((te$growth_2_6-pred)^2))


#Random Search
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, search = "random")
set.seed(199)
mtry <- floor(sqrt(ncol(tr)))
m9 <- train(growth_2_6~., tr, method = "rf", tuneLength = 50, trControl = control)




# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

set.seed(199)
train(growth_2_6~., data = tr, method = "rf")

expand.grid(mtry = 262)
trainControl()
