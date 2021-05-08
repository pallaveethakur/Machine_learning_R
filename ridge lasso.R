library(glmnet)
library(caret)
library(psych)

#data
getwd()
toyota <- read.csv(file.choose())
View(toyota)
str(toyota)
pairs.panels(toyota[-c(1,2,5,6,8,10,11,12,15,19:38 )],cex=2)
#this gives us correlation for integers.if all numeric independent var are highly correlated,it gives multicollinearity problem.
#collinearity leads to overfitting.


corolla <- toyota[-c(1:2,5:6,8,10:12,15,19:38)]

#to avoid overfitting,we need
#Ridge <- shrinks coeff to non-zero to prevent overfit,but keeps all variables.
#lasso <- shrinks regression coeff with some srunk to zero.thus it also helps in feature selection.
#elastic net regression <- mix of ridge and lasso

#data partition

set.seed(222)
coral <- sample(2,nrow(corolla),replace=T,prob=c(0.7,0.3))
train <- corolla[coral==1,]
test <- corolla[coral==2,]

#custom control parameters
custom <- trainControl(method="repeatedcv",number=10
                       ,repeats=5,verboseIter = T)
#linearmodel
set.seed(1234)
lm <- train(Price~.,data=train,method='lm',trControl=custom)
lm$results
#MAE is very high and it should be lower.
lm
summary(lm)
plot(lm$finalModel)

#ridge regression
set.seed(1234)
ridge <- train(Price~.,data=train,method='glmnet',tuneGrid=expand.grid(alpha=0,lambda=seq(0.0001,3,length=10)),trCtrl=custom)
ridge
plot(ridge,xvar="lambda",label=T)
plot(varImp(ridge,scale=T))

#lasso regression
set.seed(1234)
lasso <- train(Price~.,data=train,method='glmnet',tuneGrid=expand.grid(alpha=1,
                                                                      lambda=seq(0.0001,3,length=10)),trCtrl=custom)
lasso
plot(lasso)
plot(varImp(lasso,scale=T))

#compare models
models <- list(Ridge=ridge,Lasso=lasso)
res <- resamples(models)

summary(res)

#best model
ridge$bestTune
lasso$bestTune

#since rmse and mae are lesser in lasso,so lasso model is final




