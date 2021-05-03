#Implement a KNN model to classify the animals in to categorie

## Lets explore the Data set##
library(readr)
Zoo <- read.csv(file.choose())
View(Zoo)
summary(Zoo)
table(Zoo$type)
str(Zoo)

Zoo$type <- factor(Zoo$type, levels = c('1', '2','3','4','5', '6','7'), labels=c('category1','category2', 'category3','category4', 'category5', 'category6', 'category7'))
str(Zoo)
View(Zoo)
Zoo <- Zoo[-1]
View(Zoo)
#######################Lets start EDA part of Zoo Data set#########################
##standard deviation ##
sd(Zoo$hair)
sd(Zoo$feathers)
sd(Zoo$eggs)
sd(Zoo$milk)
sd(Zoo$airborne)
sd(Zoo$aquatic)
sd(Zoo$predator)
sd(Zoo$toothed)
sd(Zoo$domestic)

#### variance #####
var(Zoo[1:16])

#######3rd and 4th Business Moment################
library(moments)
###########skewness##########
skewness(Zoo[1:16])
############kurtosis ########
kurtosis(Zoo[1:16])

############barplot#########
barplot(Zoo$hair)
barplot(Zoo$feathers)
barplot(Zoo$eggs)
barplot(Zoo$milk)
barplot(Zoo$airborne)
barplot(Zoo$aquatic)
barplot(Zoo$predator)
barplot(Zoo$toothed)
barplot(Zoo$domestic)

###############boxplot##########
boxplot(Zoo[1:16])

########histogram######

hist(Zoo$hair)
hist(Zoo$feathers)
hist(Zoo$eggs)
hist(Zoo$milk)
hist(Zoo$airborne)
hist(Zoo$aquatic)
hist(Zoo$predator)
hist(Zoo$toothed)
hist(Zoo$domestic)

######################### Lets start the "Model building" ###########################
norm <- function(x){
  return((x-min(x))/max(x)-min(x))
}
####Testing of norm function ##########
norm(c(1,2,3,4,5))
summary(Zoo)
############# Applying the Norm fucntion to the data set ########
Zoo_n <- as.data.frame(lapply(Zoo[1:16],norm)) 

#norm function to normalize the data
summary(Zoo_n)
#############Combining the two data sets #######
Moderndata <- cbind(Zoo_n,Zoo[17])
View(Moderndata)
###### Data partition by Using "caret package" #########

#installpackages("caret")
library(caret)
training <- createDataPartition(Moderndata$type,p=.70,list = F)
##### TEST data ######
Train <- Moderndata[training,]
#View(Train)
######### Test data ###
Test <- Moderndata[-training,]
#View(Test)
################ Main Model formation Begin #######
#install.packages("class")
library(class)
#building a model
KnnModel <- knn(Train[1:16], Test[1:16], Train$type, k=3)
mean(KnnModel==Test$type) ###accuracy is 96%
#install.packages("gmodels")
library(gmodels)
CrossTable(KnnModel,Test$type)
#install.packages("e1071")
library(e1071)
confusionMatrix(KnnModel,Test$type)
######### Begging method use to increase the Model accuracy
train <- NULL
test <- NULL
for (i in 1:10) 
{
  train <- knn(Train[1:16], Test[1:16], Train$type, k=i)
  test <- c(test,mean(train==Test$type))
  
  
}
#final model
KNNFInal_model <- knn(Train[1:16],Test[1:16],Train$type,k=5)
mean(KNNFInal_model==Test$type) ## Accuracy 96%
