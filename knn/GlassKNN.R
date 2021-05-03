#Prepare a model for glass classification using KNN

#Data Description:

#  RI : refractive index

#Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)

#Mg: Magnesium

#AI: Aluminum

#Si: Silicon

#K:Potassium

#Ca: Calcium

#Ba: Barium

#Fe: Iron

#Type: Type of glass: (class attribute)
#1 -- building_windows_float_processed
#2 --building_windows_non_float_processed
#3 --vehicle_windows_float_processed
#4 --vehicle_windows_non_float_processed (none in this database)
#5 --containers
#6 --tableware
#7 --headlamps

################ Lets Explore the Model ############################
#install.packages("readr")
library(readr)

glass <- read.csv(file.choose())
View(glass)
dim(glass)
table(glass$Type)
str(glass)
glass$Type <- factor(glass$Type, levels = c('1','2','3','5','6','7'), labels=c('building_windows_float_processed','building_windows_non_float_processed','vehicle_windows_float_processed','containers','tableware','headlamps'))
str(glass)
summary(glass)

#################Lets start the EDA of Glass data sets#################
sd(glass$RI)
sd(glass$Na)
sd(glass$Mg)
sd(glass$Al)
sd(glass$Si)
sd(glass$K)
sd(glass$Ca)
sd(glass$Ba)
sd(glass$Fe)

######Variance ############
var(glass)

#####3rd and 4th Business Moment#######
library(moments)
###skewness##
skewness(glass[1:9])
#######Kurtosis######
kurtosis(glass[1:9])

#############Histogram#############
hist(glass$RI)
hist(glass$Na)
hist(glass$Mg)
hist(glass$Al)
hist(glass$Si)
hist(glass$K)
hist(glass$Ca)
hist(glass$Ba)
hist(glass$Fe)

#############Barplot##############

barplot(glass$RI)
barplot(glass$Na)
barplot(glass$Mg)
barplot(glass$Al)
barplot(glass$Si)
barplot(glass$K)
barplot(glass$Ca)
barplot(glass$Ba)
barplot(glass$Fe)

#################### Lets start the Model building ###################

#normailize the data
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#testing of norm data
norm(c(1,2,3,8))

#normalizling the data
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
norm(glass_n$RI)

# combiing the two data frame
Moderndata <- cbind(glass_n,glass[10])
View(Moderndata)

#partion of data by using caret package

#install.packages("caret")
library(caret)
Training <- createDataPartition(Moderndata$Type, p=.70, list=F)
#######Train data###
Train <- Moderndata[Training,]
#View(Train)
#############Test data#########
Test <- Moderndata[-Training,]
View(Test)
# building a model with the help of class package
#install.packages("class")
library(class)
knn1 <- knn(Train[1:9],Test[1:9], Train$Type,k=3)
mean(knn1==Test$Type)

Train_acc <- NULL
Test_acc <- NULL

for (i in 1:20) {
  Train_acc <-knn(Train[1:9],Test[1:9], Train$Type,k=i)
  Test_acc <- c(Test_acc,mean(Train_acc==Test$Type))  
  
  
}

#building a final model of Glass

KNNFInal_model <- knn(Train[1:9],Test[1:9], Train$Type,k=6)
mean(KNNFInal_model==Test$Type)

## Test_acc in this line we can see at what point we get highest k value. then we put 
## That value in KNNFinal_model

###############################Thank you#############################################