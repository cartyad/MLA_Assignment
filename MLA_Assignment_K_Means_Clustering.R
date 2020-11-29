################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

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
