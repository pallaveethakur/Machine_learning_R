#credit card assignment logistics regression
#classify whether application accepted or not
getwd()
setwd("/Users/navidhithakur/Desktop/EXCELR/assignments/logistic")
cc <- read.csv("creditcard.csv")
View(cc)
attach(cc)


#EDA

str(cc)
sum(is.na(cc))#0 na values
sum(duplicated(cc))#0 duplicates
dim(cc)
ncol(cc)
colnames(cc)
summary(cc)
cc$card <- ifelse(cc$card=='yes',1,0)
cc$owner <- ifelse(cc$owner=='yes',1,0)
cc$selfemp <- ifelse(cc$selfemp=='yes',1,0)
library(dplyr)
glimpse(cc)
str(cc)
hist(cc$card)
# number of accepted cards is much more.
hist(cc$income)
hist(cc$age)
boxplot(cc$age)#above 60 is considered outliers.also 30 is median age..
library(corrplot)
library(RColorBrewer)
m <- cor(cc)
corrplot(m,type="upper",col=brewer.pal(n=8,name = "RdYlBu"))
cor(cc,y=cc$card)
# months,dep,selfemp,reports have negative correlation

#splitting data
library(caTools)
set.seed(123)
cc.train <- sample.split(cc,SplitRatio = 0.70)
glimpse(cc.train)
trainset <- subset(cc,cc.train=="TRUE")
testset <- subset(cc,cc.train=="FALSE")
table(trainset$card)
table(testset$card)

#fitting models
cc.train.model <- glm(card~.,data=trainset,family=binomial,maxit=100)
cc.train.model
exp(coef(cc.train.model))
summary(cc.train.model)

cc.train.model2 <- glm(card~age+income+share+expenditure+reports+dependents,family=binomial,data=trainset,maxit=100)
summary(cc.train.model2)

cc.train.model3 <- glm(card~reports+dependents,famil=binomial,data=trainset)
summary(cc.train.model3)



cc.train.model4 <- glm(card~reports+dependents+age+income,family=binomial,data=trainset)
summary(cc.train.model4)


#AIC value of 3rd and 4th models are high.

#predictions

prediction3 <- predict(cc.train.model3,newdata=testset[-2],type="response")#using 3rd cc train model
prediction3
table(testset$card,prediction3>0.5)
table(prediction3>0.5)

prediction4 <- predict(cc.train.model4,newdata=testset[-2],type="response")#using 4th cc train model
prediction4
table(testset$card,prediction4>0.5)
table(prediction4>0.5)




conf <-  table(testset$card,prediction4>0.5)
conf

conf2 <- table(testset$card,prediction3>0.5)
conf2

#0 1nd 1 will be change to true and false for model accuracy.
dimnames(conf)[[1]]=c("FALSE","TRUE")
conf

#model accuracy


library(caret)

sensitive <- sensitivity(conf)
specificity <- specificity(conf)


sensitive2 <- sensitivity(conf_matrix)
specificity2 <- specificity(conf)
#since threshold 0.8 is not getting good results..we'll go with 0.5.
Accuracy <- sum(diag(conf)/sum(conf))
error <- 1-Accuracy

#ROC Curve
library(ROCR)
library(pROC)




plot.roc(testset$card,prediction3)
par(pty="s")#to get side lines removed
plot.roc(testset$card,prediction3,legacy.axes=TRUE,print.auc=TRUE)#TO Get value in x axis from 0-1,legacy.axes used

plot.roc(testset$card,prediction4,print.auc=TRUE,legacy.axes=TRUE)





#prediction4 or cc.train.model 4 is final.as it has better auc.

