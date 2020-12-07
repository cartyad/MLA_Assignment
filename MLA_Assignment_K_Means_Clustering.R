################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

library(ellipse)
library(MASS)
library(dplyr)

################################################################################################################################################
#K-Means Clustering
#############################################################################################################################################

WSS <- rep(0,30)
KMD<-heart_data[, c(1,3,4,6,8)]

for(i in 1:30)
{
  WSS[i] <- sum(kmeans(KMD, centers = i)$withinss)
}
plot(WSS)
k <- 3
cl2 <- kmeans(KMD,centers = 3)
cl2
table(cl2$cluster)
cl2$centers
cl2$withinss

#Plot the Clusters and their centroids
plot(KMD, col = cl2$cluster)
points(cl2$centers, col=1:k, pch=8, cex=5)

#Distance between cluster centroids
dist(cl2$centers)

#To calculate the 'Average Distance from Cluster Centroid 1' enter:
g1 <- faithful[which(cl2$cluster==1),]
ng1 <- cl2$size[1]
total1 <- sum(as.matrix(dist(rbind(g1, cl2$centers[1,])))[ng1+1,])
ave1 <- total1/ng1
ave1

#To calculate the 'Average Distance from Cluster Centroid 2' enter:
g2 <- faithful[which(cl2$cluster==2),]
ng2 <- cl2$size[2]
total2 <- sum(as.matrix(dist(rbind(g2, cl2$centers[2,])))[ng2+1,])
ave2 <- total2/ng2
ave2

################################################################################################################################################
#K-Means Clustering Scaled
#############################################################################################################################################

WSS <- rep(0,30)
#KMD<-scale(heart_data[, c(1,3,4,6,8,9)])
#KMD<-cbind(KMD,heart_data[,c(2,5,7)])
KMD<-heart_data[,1:9]
KMD
for(i in 1:30)
{
  WSS[i] <- sum(kmeans(KMD, centers = i)$withinss)
}
plot(WSS)
k <- 5
cl2 <- kmeans(KMD,centers = 5)
cl2

classDivision = sapply(unique(cl2$cluster), function(g)heart_data$Class[cl2$cluster ==g])
classDivision

g1 = c()
g1 = classDivision[[1]]
length(g1)
g2 = c()
g2 = classDivision[[2]]
length(g2)
g3 = c()
g3 = classDivision[[3]]
length(g3)
g4 = c()
g4 = classDivision[[4]]
g4
length(g4)
g5 = c()
g5 = classDivision[[5]]
length(g5)
count1s = 0
count2s = 0
i =0
for (i in 1:length(g3)){
  if(g3[i] ==1)
  {
    count1s = count1s + 1
  }
  else
  {
    count2s = count2s +1
  }
}
count2s/ (count1s + count2s)


print(cl2)
round(apply(KMD, 2, mean), 2)

cl2$cluster
table(cl2$cluster)
cl2$centers
cl2$withinss
group_5 = cutree(cl2,5)
print(cl2)
round(apply(KMD, 2, mean), 2)
#Plot the Clusters and their centroids
plot(KMD, col = cl2$cluster)
points(cl2$centers, col=1:k, pch=8, cex=5)

#Distance between cluster centroids
dist(cl2$centers)

#To calculate the 'Average Distance from Cluster Centroid 1' enter:
g1 <- faithful[which(cl2$cluster==1),]
ng1 <- cl2$size[1]
total1 <- sum(as.matrix(dist(rbind(g1, cl2$centers[1,])))[ng1+1,])
ave1 <- total1/ng1
ave1

#To calculate the 'Average Distance from Cluster Centroid 2' enter:
g2 <- faithful[which(cl2$cluster==2),]
ng2 <- cl2$size[2]
total2 <- sum(as.matrix(dist(rbind(g2, cl2$centers[2,])))[ng2+1,])
ave2 <- total2/ng2
ave2
 
