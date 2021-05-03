#Perform clustering 
#(Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters.
#Data Description:

#The file EastWestAirlinescontains information on passengers who belong to an 
#airlineâ???Ts frequent flier program. For each passenger the data include 
#information on their mileage history and on different ways they accrued or spent
#miles in the last year. The goal is to try to identify clusters of passengers 
#that have similar characteristics for the purpose of targeting different 
#segments for different types of mileage offers

#ID --Unique ID

#Balance--Number of miles eligible for award travel

#Qual_mile--Number of miles counted as qualifying for Topflight status

#cc1_miles -- Number of miles earned with freq. flyer credit card in the past 12 months:
#cc2_miles -- Number of miles earned with Rewards credit card in the past 12 months:
#cc3_miles -- Number of miles earned with Small Business credit card in the past 12 months:

#1 = under 5,000
#2 = 5,000 - 10,000
#3 = 10,001 - 25,000
#4 = 25,001 - 50,000
#5 = over 50,000

#Bonus_miles--Number of miles earned from non-flight bonus transactions in the past 12 months

#Bonus_trans--Number of non-flight bonus transactions in the past 12 months

#Flight_miles_12mo--Number of flight miles in the past 12 months

#Flight_trans_12--Number of flight transactions in the past 12 months

#Days_since_enrolled--Number of days since enrolled in flier program

#Award--whether that person had award f


getwd()
setwd("/Users/navidhithakur/Desktop/EXCELR/clustering")
airlines <- readxl::read_xlsx("data airlines.xlsx")     
airlines

head(airlines)
tail(airlines)
colnames(airlines)
library(dplyr)
glimpse(airlines)
str(airlines)#all  are numeric variables
sum(is.na(airlines))#no missing values
sum(duplicated(airlines))#no duplicates



#data normalization
library(caret)
prep <- preProcess(airlines[,2:11],method=c("center","scale"))
normalized <- predict(prep,airlines[,2:11])
head(normalized)
summary(normalized)

#kmeans clustering
#wss plot to choose optimum no of clusters

wssplot <- function(data,nc=15,seed=1234)
{
  wss <- (nrow(normalized)-1)*sum(apply(normalized, 2, var))		 # Determine number of clusters by scree-plot 
          for (i in 2:12) wss[i] = sum(kmeans(normalized, centers=i)$withinss)
          plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
          title(main = "K-Means Clustering Scree-Plot")
}



c <- wssplot(normalized,nc=20,seed=123)#2-4 clusters

#kmeans
km <- kmeans(normalized,6)
km2 <- kmeans(normalized,8)
km3 <- kmeans(normalized,4)
km4 <- kmeans(normalized,3)

table(km4)




#cluster analysis
library(ggplot2)
install.packages("ggfortify")
library(ggfortify)
autoplot(km,normalized,frame=TRUE)
#as the plots are overlapping
autoplot(km2,normalized,frame=TRUE)
autoplot(km3,normalized,frame=TRUE)
autoplot(km2,airlines,frame=TRUE)
autoplot(km4,normalized,frame=TRUE)

#cluster center
km4$centers
#since none of the values are overlapping hence 3 clusters seems best fit for data.








