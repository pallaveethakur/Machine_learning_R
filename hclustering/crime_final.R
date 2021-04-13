#Perform Clustering for the crime data and identify the number of clusters formed and draw inferences.

#Data Description:
 # Murder -- Muder rates in different places of United States
#Assualt- Assualt rate in different places of United States
#UrbanPop - urban population in different places of United States
#Rape - Rape rate in different places of United States
getwd()
setwd('/Users/navidhithakur/Desktop/EXCELR/clustering')
crime <- read.csv("crime_data.csv")
str(crime)#data consists of mixed variables
head(crime)
tail(crime)
crime$X <- as.character(x=crime$X)
#changing column variables as rownames for first variable
rownames(crime) <- c(crime$X)
head(crime)
newdata <- crime[,-1]
head(newdata)
z <- newdata

sum(is.na(z))
z <- na.omit(z)


pairs(z)#creating a scatter plot for quick analysis


library(ggplot2)
plot <- ggplot(data=z,aes(x=UrbanPop,y=Murder))+geom_point()
plot1 <- ggplot(z,mapping=aes(x=Assault,y=Murder,color=Murder))+geom_point()  
plot1
#assault and murder are growing exponentially
plot
#with plot it is observed that as the urban pop growing,crime rate is high.

#Data normalization to avoid bias
m <- apply(z,2,mean)#margin=2 to apply over columns
s <- apply(z,2,sd)
z <- scale(z,m,s)
z

#calculating distance(Euclidean,manhattan,minkowski,cosine,correlation)
#Using Euclidean distance
Euclid_dist <- dist(z)
x <- as.matrix(Euclid_dist)[1:6,1:6]#View the euclidean distance measurements
head(round(x,digits=3))#to round values till 3 decimals.

summary(Euclid_dist)
print(Euclid_dist,digits=3)


#plotting distance
#install.packages("factoextra")
library(factoextra)
fviz_dist(Euclid_dist,show_labels = TRUE,order=TRUE)
#with the plot,we can observe similar objects are close to each other.

#Hierarchial Clustering
#method=agglomerative method to be used
#        (ward.d,ward,d2,single,complete,average,centroid)

require(stats)
hclustr <- hclust(d=Euclid_dist,method = "complete")
 plot(x=hclustr,hang=-1)
 
 #for enhanced visualization
 fviz_dend(x=hclustr,cex=0.7,lwd=0.7)

 #assigning colors to dendrogram
 library(grDevices)
palette()

fviz_dend(x=hclustr,cex=0.7,lwd=0.7,k=4,k_colors=c("red","blue","green","magenta"))

fviz_dend(x=hclustr,cex=0.7,lwd=0.7,k=4,k_colors="jco")
#to draw rectangle
fviz_dend(x=hclustr,cex=0.7,lwd=0.7,k=4,k_colors="jco",rect=TRUE,rect_border="jco",rect_fill=TRUE,horiz = TRUE)
#install.packages("igraph")
library(igraph)
fviz_dend(x=hclustr,cex=0.7,lwd=0.7,k=4,k_colors="jco",
          rect=TRUE,rect_border="jco",rect_fill=TRUE,type="phylogenic",repel=TRUE)

#Model 2
#Manhattan distance,method=centroid
manhattan <- dist(z,method="manhattan")
manhattan
hclustr.2 <- hclust(d=manhattan,method = "complete")
plot(x=hclustr.2,hang=-1)




