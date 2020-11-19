#MLA Assignment
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

#PRINCIPLE COMPONANTS ANALYSIS:
?prcomp
HRD <- prcomp(heart_data[, 1:9])
HRD
print(HRD)
summary(HRD)
?round
round(HRD$rotation, 2)
round(HRD$sdev, 2)
plot(HRD, main = "Heart Data")
plot(HRD, main = "Heart Data", type = "l")
fisher_var_explain <- (HRD$sdev^2) / (sum(HRD$sdev^2))
plot(fisher_var_explain, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)
#PROJECTING THE DATA ONTO THE PRINCIPLE COMPONENTS
?predict
newdata <- predict(HRD)
head(newdata, n = 10)

###NOT FINISHED###
#VISUALISING THE RESULTS
plot(myts[, 1], myts[, 2], col=myts[, 5])
  legend(6.5, 4.5, legend = levels(iris[, 5]), col = c(1, 2, 3), pch = 1)
pairs(iris[,1:4],col=iris[,5])
pairs(newiris,col=iris[,5])


################################################################################################################################################
#HIERARCHICAL CLUSTERING:
heart_data_clustering<-heart_data[,c(1,3,4,6)]
HRD_dis <- dist(heart_data_clustering, method="euclidian")
HRD_dis_mat <- as.matrix(HRD_dis)

#Good
clust1 <- hclust(HRD_dis, method = "complete")
plot(clust1)
abline(h = (mean(clust1$height)+(3*sd(clust1$height))), lty=2, col=2)
clust1_label <- cutree(clust1, h=(mean(clust1$height)+(3*sd(clust1$height))))
palette(rainbow(10))
plot(heart_data[,2], heart_data[,3], col = clust1_label)
pairs(heart_data_clustering, col = clust1_label)

#Useless
clust2 <- hclust(HRD_dis, method = "single")
plot(clust2)
abline(h = (mean(clust2$height)+(3*sd(clust2$height))), lty=2, col=2)

#Good
clust3 <- hclust(HRD_dis, method = "average")
plot(clust3)
abline(h = (mean(clust3$height)+(3*sd(clust3$height))), lty=2, col=2)

