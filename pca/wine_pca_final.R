#PCA - Wine Data set

#Perform Principal component analysis and perform clustering using first 
#3 principal component scores (both heirarchial and k mean clustering(scree plot or
#elbow curve) and obtain optimum number of clusters and check whether we have 
#obtained same number of clusters with the original data 
#(class column we have ignored at the begining who shows it has 3 clusters)df

#install.packages("readr")
library(readr)

wine <- read.csv(file.choose())
View(wine)
summary(wine)
str(wine)

sum(is.na(wine))
attach(wine)

cor(wine)

# Model Building#
PCA<-princomp(wine, cor = TRUE, scores = TRUE, covmat = NULL)
summary(PCA)
#str(PCA)

plot(PCA) 
# Comp.1 having highest importance (highest variance)

biplot(PCA)
PCA$loadings
PCA$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# Considering top 3 principal component scores and binding them with wine
PCA_final1<-cbind(wine,PCA$scores[,1:3])
View(PCA_final1)

#Hierarchical Clustering#

# Preparing data for clustering (considering only PCA scores;
#as they represent the entire data)
clus_data<-PCA_final1[,15:17]

# Normalizing the data 
normalized_data<-scale(clus_data) # Scale function is used to normalize data
View(normalized_data)

# Distance matrix
d <- dist(normalized_data, method = "manhattan") 
d

# Model Building
fit <- hclust(d, method="complete")

# Display dendrogram
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters

rect.hclust(fit, k=5, border="red")

Wine_hierarchical<-as.matrix(groups)
table(Wine_hierarchical)

final1 <- data.frame(clus_data, Wine_hierarchical)
final <- cbind(clus_data, Wine_hierarchical)
View(final1)

#explore setcolorder for repositioning the columns in R

library(data.table)
setcolorder(final1,c("Wine_hierarchical"))
View(final1)

#K-Means Clustering#

library(plyr)
km <- kmeans(clus_data,5) #kmeans clustering
str(km)


library(animation)
km <- kmeans.ani(clus_data, 5)

#Elbow curve & k ~ sqrt(n/2) to decide the k value

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:3) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:3, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(main = "K-Means Clustering Scree-Plot")

# Selecting K for kmeans clustering using kselection

library(kselection)
k <- kselection(clus_data, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k


# Model Building
fit <- kmeans(normalized_data, 5) # 5 cluster solution
str(fit)
table(fit$cluster)
Wine_kmeans<- data.frame(clus_data, fit$cluster) # append cluster membership
View(Wine_kmeans)
library(data.table)
setcolorder(Wine_kmeans, neworder = c("fit.cluster"))
View(Wine_kmeans)
aggregate(clus_data, by=list(fit$cluster), FUN=mean)

