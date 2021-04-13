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
airlines <- readxl::read_xlsx("east west airlines.xlsx")     
airlines

head(airlines)
tail(airlines)
colnames(airlines)
library(dplyr)
glimpse(airlines)
str(airlines)#all  are numeric variables
sum(is.na(airlines))#no missing values
sum(duplicated(airlines))#no duplicates
sum(duplicated(airlines))


library(ggplot2)
plot <- ggplot(data=airlines,aes(x=Balance,y=Days_since_enroll))+geom_point()
plot
plot1 <- ggplot(airlines,mapping=aes(x=Flight_miles_12mo,y=Flight_trans_12,color=Balance))+geom_point()  
plot1
#0-10000 flight miles have maximum customers
#days since enrolled and balance miles lies more betwn 0-5lac.

#data normalization
library(caret)
prep <- preProcess(airlines[,2:11],method=c("center","scale"))
normalized <- predict(prep,airlines[,2:11])
head(normalized)
summary(normalized)

#Measuring Distance 
# data has  dummy  variables in cc and awards.
#euclidean,method=ward

distance <- dist(airlines[2:11],method="euclidean")
head(distance)
hclust1 <- hclust(distance,method="ward.D2")
plot(hclust1)

groups <- cutree(hclust1,k=5)
rect.hclust(hclust1,border="red",k=5)

#method 2
dis <- dist(airlines[2:11],method="manhattan")
clustermiles <- hclust(dis,method="ward.D")
plot(clustermiles)
clusterg <- cutree(clustermiles,k=5)
#percentage of passengers in each cluster 
tapply(airlines$`Award?`,clusterg,mean)#4th cluster has highest passengers with awards
tapply(airlines$cc1_miles,clusterg,mean)#4th cluster has highest passengers with cc1
tapply(airlines$cc2_miles,clusterg,mean)#4th cluster has highest cc2 miles
tapply(airlines$cc3_miles,clusterg,mean)#4th has highest cc3 miles
tapply(airlines$Balance,clusterg,mean)
tapply(airlines$Days_since_enroll,clusterg,mean)
tapply(airlines$Flight_miles_12mo,clusterg,mean)
#from the above,we can see that among all clusters,4th has highest in every
#so we cantarget passengers in cluster 4 for more offers.

subset(airlines,Balance=="41354")#checking which cluster has 41354
clusterg[3]
#hence it belongs to cluster 2.
#checking what other ids are in same cluster 2
cluster2 <- subset(airlines,clusterg==2)
cluster2$'ID#'[1:20]
#Hence we can target cluster2 ids with promotional offers..similarly with other cluster depending on their miles.







