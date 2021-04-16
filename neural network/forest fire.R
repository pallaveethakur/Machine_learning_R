#NEURAL NETWORK ASSIGNMENT

#Build a Neural Network model for forest to predict profit 

getwd()
setwd("/Users/navidhithakur/Desktop/EXCELR/final assignments/neuralnet")
library(readr)
forest <- read.csv("forestfires.csv")
View(forest)
str(forest)

#Normalize the data
Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}
Norm(c(1,5,6,8,-9))
View(forest)
str(forest)
forest$month <- as.numeric(as.factor(forest$month))
forest$day<- as.numeric(as.factor(forest$day))
forest$size_category <- as.numeric(as.factor(forest$size_category))


View(forest)
table(forest$size)
summary(forest)

#apply Norm function to the "forest" data set.
forest_N <- as.data.frame(lapply(forest,Norm))
View(forest_N)
str(forest_N)
summary(forest_N)
#data partition of area column
library(caret)
Traininglocal <- createDataPartition(forest_N$area, p=.70, list=F)
Train <- forest_N[Traininglocal,]
Test <- forest_N[-Traininglocal,]
View(Train)
View(Test)
library(nnet)
library(neuralnet)
Model_1 <- neuralnet(area~., data = Test)
str(Model_1)
plot(Model_1)
Predict_1 <- compute(Model_1, Test[1:30])
Area_burned <- Predict_1$net.result
Predict_1$neurons
cor(Area_burned, Test$area)
