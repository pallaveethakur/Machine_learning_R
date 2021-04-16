#Build a Neural Network model for 50_startups data to predict profit 


startups <- read.csv(file.choose())
View(startups)
str(startups)
summary(startups)
ncol(startups)
attach(startups)

#changing factor to numeric
library(dplyr)
library(magrittr)
startups %<>% mutate_if(is.factor,as.numeric)
str(startups)



#Normalization of data by create a custom norm function

Norm <- function(x){return((x-min(x))/(max(x)-min(x)))}

#testing of Norm function
Norm(c(1,5,-9,-859647))

startups_N <- as.data.frame(lapply(startups, Norm))
View(startups_N)
summary(startups_N)

#create data partition
library(readr)
library(caret)
Traininglocal <- createDataPartition(startups_N$Profit, p=0.70, list = F)
## "Test data" ##
Train <- startups_N[Traininglocal,]

## "Train data " ##
Test <- startups_N[-Traininglocal,]


### Model building 
library(neuralnet)
library(nnet)

Model_1 <- neuralnet(Profit~., data = Train)
plot(Model_1) ## step is 270
Pred <- compute(Model_1, Test[1:4])
Pred_profit <- Pred$net.result
Pred$neurons
cor(Pred_profit, Test$Profit) ## co-relation is good here is 97%

# neural network visualization --------------------------------------------
colnames(startups)
n <- neuralnet(Profit~.,data=Train,hidden=c(2,2),linear.output = FALSE,lifesign='full',rep=1)
plot(n,col.hidden='darkgreen',col.hidden.synapse='darkgreen',show.weights=FALSE,fill='red')


