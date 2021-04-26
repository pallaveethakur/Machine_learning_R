library(cluster)

wine<-read.csv("wine.csv")
View(wine)
View(wine[-1]) 

wine_1 <- wine[-1]
attach(wine_1)
cor(wine_1)
pca<-princomp(wine_1, cor = TRUE, scores = TRUE, covmat = NULL)

summary(pca)

loadings(pca)

plot(pca)
biplot(pca)
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type="b")

pca$scores[,1:3]   ## top 3 pca score

mydata<-cbind(wine,pca$scores[,1:3])
View(mydata)

# Preparing data for clustering (considering only PCA scores;
#as they represent the entire data)
clus_data<-mydata[,15:17]
#View(clus_data)

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean")

# Clustering the data using hclust function --> Hierarchical
fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram
rect.hclust(fit1, k=7, border="red")
grouped<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_11<-as.matrix(grouped) # cluster numbering 

table(membership_11)

final_1<-cbind(membership_11,wine) # binding column wise with orginal data
View(final_1)
View(aggregate(final_1[,-c(2,9:11)],by=list(membership_11),FUN=mean)) # Inferences can be

write.csv(final_1,file="wine1.csv",row.names = F,col.names = F)
getwd()

###### K-Means Clustering :

library(plyr)
library(kselection)

k <- kselection(clus_data, parallel = TRUE, k_threshold = 0.9, max_centers=12)
k     #### finds 3 clusters

library(animation)
km <- kmeans.ani(clus_data, 5)

normalized_data<-scale(mydata[,15:17])
k.wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
for (i in 1:7) k.wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)

### ELBOW plot 

plot(1:7, k.wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")  ###elbow plot 
title(sub = "K-Means Clustering Scree-Plot")
fitwine <- kmeans(normalized_data,7) # K-means clustering with 7 clusters of sizes 43, 23, 11, 16, 38, 34, 13
#fitwine

finalwine<- data.frame(fitwine$cluster,mydata) # append cluster membership
View(finalwine)
aggregate(clus_data, by=list(fitwine$cluster), FUN=mean)
table(fitwine$cluster)
