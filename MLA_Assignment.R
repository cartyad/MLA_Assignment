################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

################################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS:
################################################################################################################################################
HRD <- prcomp(heart_data[, c(1,3,4,6,8)])
HRD
print(HRD)
summary(HRD)
round(HRD$rotation, 2)
round(HRD$sdev, 2)
plot(HRD, main = "Heart Data")
plot(HRD, main = "Heart Data", type = "l")
fisher_var_explain <- (HRD$sdev^2) / (sum(HRD$sdev^2))
plot(fisher_var_explain, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)
#PROJECTING THE DATA ONTO THE PRINCIPLE COMPONENTS
newdata <- predict(HRD)
head(newdata, n = 10)
biplot(HRD, scale=0, cex=.5)

###NOT FINISHED###
#VISUALISING THE RESULT10
plot(heart_data[, 4], heart_data[, 6], col=heart_data[, 10])
  legend(6.5, 4.5, legend = levels(iris[, 5]), col = c(1, 2, 3), pch = 1)
pairs(heart_data[, c(1,3,4,6,8)],col=heart_data[,10])
pairs(newdata,col=heart_data[,10])

plot(newdata[,1], newdata[,2], type="n", xlab="PC1", ylab="PC2")
text(newdata[,1], newdata[,2], labels=substr(heart_data[,5],1,2), col=as.integer(heart_data[,5]))

#PCA USING CORRELATION
eigen(cor(heart_data[, c(1,3,4,6,8)]))
scaled<-prcomp(heart_data[, c(1,3,4,6,8)], scale=TRUE)
scaled
plot(scaled, main = "Heart Data")
plot(scaled, main = "Heart Data", type = "l")
fisher_var_explain1 <- (scaled$sdev^2) / (sum(scaled$sdev^2))
plot(fisher_var_explain1, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)

diag(cov(heart_data[, c(1,3,4,6,8)]))
c(var(heart_data[, 1]), var(heart_data[,3]), var(heart_data[, 4]), var(heart_data[, 6]))
newScaled<-predict(scaled)
head(newScaled, n = 10)
pairs(newScaled,col=heart_data[,10])

################################################################################################################################################
#HIERARCHICAL CLUSTERING:
################################################################################################################################################
#ALL Data
heart_data_clustering<-heart_data[,c(1,3,4,6)]

#Standardising
#heart_data_clust<-heart_data[,c(1,3,4,6)]
#heart_data_clustering <-scale(heart_data_clust, center = TRUE, scale = TRUE)

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

#Good
clust3 <- hclust(HRD_dis, method = "average")
plot(clust3)
abline(h = (mean(clust3$height)+(3*sd(clust3$height))), lty=2, col=2)
clust3_label <- cutree(clust3, h=(mean(clust3$height)+(3*sd(clust3$height))))
palette(rainbow(10))
plot(heart_data[,2], heart_data[,3], col = clust3_label)
pairs(heart_data_clustering, col = clust3_label)
################################################################################################################################################
################################################################################################################################################
#Male Data Only
library(dplyr)
male_data <- heart_data %>% filter(Sex==1)
male_heart_data_clustering<-male_data[,c(1,3,4,6)]
male_HRD_dis <- dist(male_heart_data_clustering, method="euclidian")
male_HRD_dis_mat <- as.matrix(male_HRD_dis)

#Good
clust1M <- hclust(male_HRD_dis, method = "complete")
plot(clust1M)
abline(h = (mean(clust1M$height)+(3*sd(clust1M$height))), lty=2, col=2)
clust1M_label <- cutree(clust1M, h=(mean(clust1M$height)+(3*sd(clust1M$height))))
palette(rainbow(10))
plot(male_data[,2], male_data[,3], col = clust1M_label)
pairs(male_heart_data_clustering, col = clust1M_label)

#Good
clust3M <- hclust(male_HRD_dis, method = "average")
plot(clust3M)
abline(h = (mean(clust3M$height)+(3*sd(clust3M$height))), lty=2, col=2)
clust3M_label <- cutree(clust3M, h=(mean(clust3M$height)+(3*sd(clust3M$height))))
palette(rainbow(10))
plot(male_data[,2], male_data[,3], col = clust3M_label)
pairs(male_heart_data_clustering, col = clust3M_label)

################################################################################################################################################
#Female Data Only
library(dplyr)
female_data <- heart_data %>% filter(Sex==2)
female_heart_data_clustering<-female_data[,c(1,3,4,6)]
female_HRD_dis <- dist(female_heart_data_clustering, method="euclidian")
female_HRD_dis_mat <- as.matrix(female_HRD_dis)

#Good
clust1F <- hclust(female_HRD_dis, method = "complete")
plot(clust1F)
abline(h = (mean(clust1F$height)+(3*sd(clust1F$height))), lty=2, col=2)
clust1F_label <- cutree(clust1F, h=(mean(clust1F$height)+(3*sd(clust1F$height))))
palette(rainbow(10))
plot(female_data[,2], female_data[,3], col = clust1F_label)
pairs(female_heart_data_clustering, col = clust1F_label)

#Good
clust3F <- hclust(female_HRD_dis, method = "average")
plot(clust3F)
abline(h = (mean(clust3F$height)+(3*sd(clust3F$height))), lty=2, col=2)
clust3F_label <- cutree(clust3F, h=(mean(clust3F$height)+(3*sd(clust3F$height))))
palette(rainbow(10))
plot(female_data[,2], female_data[,3], col = clust3F_label)
pairs(female_heart_data_clustering, col = clust3F_label)
################################################################################################################################################
################################################################################################################################################
#Exercise Induced
library(dplyr)
EI_data <- heart_data %>% filter(ExerciseInduced==1)
EI_heart_data_clustering<-EI_data[,c(1,3,4,6)]
EI_HRD_dis <- dist(EI_heart_data_clustering, method="euclidian")
EI_HRD_dis_mat <- as.matrix(EI_HRD_dis)

#Good
clust1EI <- hclust(EI_HRD_dis, method = "complete")
plot(clust1EI)
abline(h = (mean(clust1EI$height)+(3*sd(clust1EI$height))), lty=2, col=2)
clust1EI_label <- cutree(clust1EI, h=(mean(clust1EI$height)+(3*sd(clust1EI$height))))
palette(rainbow(10))
plot(EI_data[,2], EI_data[,3], col = clust1EI_label)
pairs(EI_heart_data_clustering, col = clust1EI_label)

#Bad
clust3EI <- hclust(EI_HRD_dis, method = "average")
plot(clust3EI)
abline(h = (mean(clust3EI$height)+(3*sd(clust3EI$height))), lty=2, col=2)
clust3EI_label <- cutree(clust3EI, h=(mean(clust3EI$height)+(3*sd(clust3EI$height))))
palette(rainbow(10))
plot(EI_data[,2], EI_data[,3], col = clust3EI_label)
pairs(EI_heart_data_clustering, col = clust3EI_label)

################################################################################################################################################
#Not Exercise Induced ~ Not Very Useful
library(dplyr)
EIN_data <- heart_data %>% filter(ExerciseInduced==2)
EIN_heart_data_clustering<-EIN_data[,c(1,3,4,6)]
EIN_HRD_dis <- dist(EIN_heart_data_clustering, method="euclidian")
EIN_HRD_dis_mat <- as.matrix(EIN_HRD_dis)

#Good OF THE BAD
clust1EIN <- hclust(EIN_HRD_dis, method = "complete")
plot(clust1EIN)
abline(h = (mean(clust1EIN$height)+(3*sd(clust1EIN$height))), lty=2, col=2)
clust1EIN_label <- cutree(clust1EIN, h=(mean(clust1EIN$height)+(3*sd(clust1EIN$height))))
palette(rainbow(10))
plot(EIN_data[,2], EIN_data[,3], col = clust1EIN_label)
pairs(EIN_heart_data_clustering, col = clust1EIN_label)

#Useless
clust2EIN <- hclust(EIN_HRD_dis, method = "single")
plot(clust2EIN)
abline(h = (mean(clust2EIN$height)+(3*sd(clust2EIN$height))), lty=2, col=2)
clust2EIN_label <- cutree(clust2EIN, h=(mean(clust2EIN$height)+(3*sd(clust2EIN$height))))
palette(rainbow(10))
plot(EIN_data[,2], EIN_data[,3], col = clust2EIN_label)
pairs(EIN_heart_data_clustering, col = clust2EIN_label)

#Bad
clust3EIN <- hclust(EIN_HRD_dis, method = "average")
plot(clust3EIN)
abline(h = (mean(clust3EIN$height)+(3*sd(clust3EIN$height))), lty=2, col=2)
clust3EIN_label <- cutree(clust3EIN, h=(mean(clust3EIN$height)+(3*sd(clust3EIN$height))))
palette(rainbow(10))
plot(EIN_data[,2], EIN_data[,3], col = clust3EIN_label)
pairs(EIN_heart_data_clustering, col = clust3EIN_label)

################################################################################################################################################
################################################################################################################################################
#Fasting Data Only
library(dplyr)
fast_data <- heart_data %>% filter(FastingBloodSugar==1)
fast_heart_data_clustering<-fast_data[,c(1,3,4,6)]
fast_HRD_dis <- dist(fast_heart_data_clustering, method="euclidian")
fast_HRD_dis_mat <- as.matrix(fast_HRD_dis)

#Good
clust1Fast <- hclust(fast_HRD_dis, method = "complete")
plot(clust1Fast)
abline(h = (mean(clust1Fast$height)+(3*sd(clust1Fast$height))), lty=2, col=2)
clust1Fast_label <- cutree(clust1Fast, h=(mean(clust1Fast$height)+(3*sd(clust1Fast$height))))
palette(rainbow(10))
plot(fast_data[,2], fast_data[,3], col = clust1Fast_label)
pairs(fast_heart_data_clustering, col = clust1Fast_label)

#Useless
clust2Fast <- hclust(fast_HRD_dis, method = "single")
plot(clust2Fast)
abline(h = (mean(clust2Fast$height)+(3*sd(clust2Fast$height))), lty=2, col=2)
clust2Fast_label <- cutree(clust2Fast, h=(mean(clust2Fast$height)+(3*sd(clust2Fast$height))))
palette(rainbow(10))
plot(fast_data[,2], fast_data[,3], col = clust2Fast_label)
pairs(fast_heart_data_clustering, col = clust2Fast_label)

#Bad
clust3Fast <- hclust(fast_HRD_dis, method = "average")
plot(clust3Fast)
abline(h = (mean(clust3Fast$height)+(3*sd(clust3Fast$height))), lty=2, col=2)
clust3Fast_label <- cutree(clust3Fast, h=(mean(clust3Fast$height)+(3*sd(clust3Fast$height))))
palette(rainbow(10))
plot(fast_data[,2], fast_data[,3], col = clust3Fast_label)
pairs(fast_heart_data_clustering, col = clust3Fast_label)

################################################################################################################################################
#Fasting Data Only
library(dplyr)
nfast_data <- heart_data %>% filter(FastingBloodSugar==2)
nfast_heart_data_clustering<-nfast_data[,c(1,3,4,6)]
nfast_HRD_dis <- dist(nfast_heart_data_clustering, method="euclidian")
nfast_HRD_dis_mat <- as.matrix(nfast_HRD_dis)

#Good
clust1nFast <- hclust(nfast_HRD_dis, method = "complete")
plot(clust1nFast)
abline(h = (mean(clust1nFast$height)+(3*sd(clust1nFast$height))), lty=2, col=2)
clust1nFast_label <- cutree(clust1nFast, h=(mean(clust1nFast$height)+(3*sd(clust1nFast$height))))
palette(rainbow(10))
plot(nfast_data[,2], nfast_data[,3], col = clust1nFast_label)
pairs(nfast_heart_data_clustering, col = clust1nFast_label)

#Useless
clust2nFast <- hclust(nfast_HRD_dis, method = "single")
plot(clust2nFast)
abline(h = (mean(clust2nFast$height)+(3*sd(clust2nFast$height))), lty=2, col=2)
clust2nFast_label <- cutree(clust2nFast, h=(mean(clust2nFast$height)+(3*sd(clust2nFast$height))))
palette(rainbow(10))
plot(nfast_data[,2], nfast_data[,3], col = clust2nFast_label)
pairs(nfast_heart_data_clustering, col = clust2nFast_label)

#Bad
clust3nFast <- hclust(nfast_HRD_dis, method = "average")
plot(clust3nFast)
abline(h = (mean(clust3nFast$height)+(3*sd(clust3nFast$height))), lty=2, col=2)
clust3nFast_label <- cutree(clust3nFast, h=(mean(clust3nFast$height)+(3*sd(clust3nFast$height))))
palette(rainbow(10))
plot(nfast_data[,2], nfast_data[,3], col = clust3nFast_label)
pairs(nfast_heart_data_clustering, col = clust3nFast_label)

################################################################################################################################################
#Linear Discriminant Analysis:
################################################################################################################################################


library(ellipse)
library(MASS)
library(dplyr)
class1data<-heart_data %>% filter(Class==1)
class2data<-heart_data %>% filter(Class==2)
dim(class1data)
dim(class2data)
plot(heart_data[,c(1,3,4,6)], col = as.factor(heart_data[,10]))
#plot(salmon[,c(2,3)], col = as.factor(salmon[, 1]), xlim=c(50,190), ylim=c(290,530))
#lines(ellipse(cov(salmon[c(1:50), c(2, 3)]), centre = colMeans(salmon[c(1:50), c(2, 3)]),level = c(0.5)))
#lines(ellipse(cov(salmon[c(51:100), c(2, 3)]), centre = colMeans(salmon[c(51:100), c(2, 3)]), level = 0.5), col = 2)

#Before we use LDA, we need to split the data into training, and test sets. (Why don't we need a validation set?)
#For this example we will try an 80:20 split.
strain <- heart_data[c(1:150), ]
stest <- heart_data[c(151:270),]

train <- rbind(class1data[1:75,],class2data[1:60,])
test <- rbind(class1data[76:150,],class2data[61:120,])


#We can then train our classifier:
lsol <- lda(train[, c(1,3,4,6,8,9)], grouping = train[,10])
#The above trains the classifier on the second and third columns of the training data strain using the classifier
#information in the first column strain[, 1]

#If you enter the following, you will be returned with a list of summary information concerning the computation:
lsol$prior
lsol$means

#To estimate the covariance matrices for both subsets of salmon data, enter the following:
n_class_1 <- length(class1data)
n_class_2 <- length(class2data)
single_cov_num <- ((n_class_1 - 1) * cov (class1data) + (n_class_2 - 1) * cov(class2data) )
single_cov <- single_cov_num / ( length(train[, 10]) - 2)
single_cov

#As well as providing information about prior probalilities and group means, calling lsol directly provides
#information regarding the coefficients of linear discriminants (use lsol$scaling to call this directly):
lsol

lsol$scaling

#predict(lsol, c(120, 380))

#To automatically predict the test data set enter:
predict(lsol, test[, c(1,3,4,6)])
################ Not Finished ################

#Cross-Validation
#an alternative technique for measuring the performance of the model is to perform cross-validation
lsol_cv <- lda(heart_data[,c(1,3,4,6)], grouping = heart_data[, 10], CV = TRUE)
lsol_cv
plot(heart_data[, c(1,3,4,6)], col = as.factor(heart_data[, 10]), pch = as.numeric(lsol_cv$class))


#Quadratic Discriminant Analysis
#difference between QDA and LDA is that the former permits each group distribution to have its own covariance 
#matrix, whilst the latter assumes a common covariance matrix for all group distributions
qsol <- qda(train[, c(1,3,4,6)], grouping = train[, 10])
predict(qsol, test[, c(1,3,4,6)])
#Again you will notice in an 80:20 training:testing split we have achieved 100% correct classification
qsol_cv <- qda(heart_data[,c(1,3,4,6)], grouping = heart_data[, 10], CV = TRUE)
plot(heart_data[, c(1,3,4,6)], col = as.factor(heart_data[, 10]), pch = as.numeric(qsol_cv$class))
plot(heart_data[, c(1,3)], col = as.factor(heart_data[, 10]), pch = as.numeric(qsol_cv$class))
plot(heart_data[, c(1,4)], col = as.factor(heart_data[, 10]), pch = as.numeric(qsol_cv$class))
plot(heart_data[, c(1,6)], col = as.factor(heart_data[, 10]), pch = as.numeric(qsol_cv$class))

#To find the covariances for the two groups enter the following:
cov (alaska_salmon)
cov (canada_salmon)

################ Not Finished ################




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
#K Nearest Neighbours Normal
#############################################################################################################################################

library(MASS)
#install.packages("FNN")
library(FNN)
library(dplyr)

#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

#Continuous Variables
contVar<-heart_data[, c(1,3,4,6,9)]

#for testing, and 1/3 for validation
index_train <- c(1:90) #First third
index_test <- c(91:180) #Second third
index_valid <- c(181:270) #Final third

train <- contVar[index_train, ]
test <- contVar[index_test, ]
valid <- contVar[index_valid, ]

#Now we have the training and the test data, we are able to run knn. We begin with a value of k = 3:
result <- knn(train, test, cl = heart_data[index_train, 10], k=11)
result
plot(test[,], col = as.factor(index_test), pch = as.numeric(result))

#The inner command table(result, simT[index_test,3]) creates a table comparing the classification
#assigned to the test data by the k-nearest neighbour classifier against the true classification of the data
class_agree <- table(result, heart_data[index_test,10])
class_agree

#The diag function selects the diagonal elements of the table, which is the number of points where knearest 
#neighbour classification agrees with the true classification
sum_agree <- sum(diag(class_agree))
sum_agree

#remainder of the code ensures that the output that is returned is the percentage misclassification.
(nrow(test) - sum_agree) / nrow(test)

#To see how the k-nearest neighbour classification performs as a function of k we can write a for loop:
kmax <- 50
k <- 1:kmax
p <- rep(0, kmax)
ntest <- nrow(test)
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax)
{
  result <- knn(train, test, cl = heart_data[index_train, 10], k = i)
  class_agree <- table(result, heart_data[index_test,10])
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary
dim(k_summary)
k_summary[1:10, ]
plot(k_summary)
######## NOT FINISHED ###################
minVal<-min(k_summary[,2])
minVal

minValueRow <- k_summary[k_summary[,2]==minVal, 1]
minValK<-minValueRow
minValK[1]

#Now we have the training and the test data, we are able to run knn. We begin with a value of k = 3:
resultValid1 <- knn(train, valid, cl = heart_data[index_train, 10], k=minValK[1])
resultValid1

#install.packages("sp")
library(sp) 
knn.graph(resultValid1)
plot(valid[, ], col = as.factor(index_valid), pch = as.numeric(resultValid1))

#The inner command table(result, simT[index_test,3]) creates a table comparing the classification
#assigned to the test data by the k-nearest neighbour classifier against the true classification of the data
class_agree <- table(resultValid1, heart_data[index_valid,10])
class_agree

#The diag function selects the diagonal elements of the table, which is the number of points where knearest 
#neighbour classification agrees with the true classification
sum_agree <- sum(diag(class_agree))
sum_agree


#remainder of the code ensures that the output that is returned is the percentage misclassification.
(nrow(test) - sum_agree) / nrow(test)



################################################################################################################################################
#K Nearest Neighbours Scaled
#############################################################################################################################################

library(MASS)
#install.packages("FNN")
library(FNN)
library(dplyr)

#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")

summary(heart_data)
dim(heart_data)

#Continuous Variables
contVar<-heart_data[, c(1,3,4,6,9)]
contVar<-scale(contVar)

#for testing, and 1/3 for validation
index_train <- c(1:90) #First third
index_test <- c(91:180) #Second third
index_valid <- c(181:270) #Final third

train <- contVar[index_train, ]
test <- contVar[index_test, ]
valid <- contVar[index_valid, ]

#Now we have the training and the test data, we are able to run knn. We begin with a value of k = 3:
result <- knn(train, test, cl = heart_data[index_train, 10], k=11)
result
plot(test[,], col = as.factor(index_test), pch = as.numeric(result))

#The inner command table(result, simT[index_test,3]) creates a table comparing the classification
#assigned to the test data by the k-nearest neighbour classifier against the true classification of the data
class_agree <- table(result, heart_data[index_test,10])
class_agree

#The diag function selects the diagonal elements of the table, which is the number of points where knearest 
#neighbour classification agrees with the true classification
sum_agree <- sum(diag(class_agree))
sum_agree

#remainder of the code ensures that the output that is returned is the percentage misclassification.
(nrow(test) - sum_agree) / nrow(test)

#To see how the k-nearest neighbour classification performs as a function of k we can write a for loop:
kmax <- 50
k <- 1:kmax
p <- rep(0, kmax)
ntest <- nrow(test)
k_summary <- cbind(k, p)
colnames(k_summary) <- c("k","% misclassified")
for(i in 1:kmax)
{
  result <- knn(train, test, cl = heart_data[index_train, 10], k = i)
  class_agree <- table(result, heart_data[index_test,10])
  sum_agree <- sum(diag(class_agree))
  k_summary[i, 2] <- (ntest - sum_agree) / ntest
}
k_summary
dim(k_summary)
k_summary[1:10, ]
plot(k_summary)
######## NOT FINISHED ###################
minVal<-min(k_summary[,2])
minVal

minValueRow <- k_summary[k_summary[,2]==minVal, 1]
minValK<-minValueRow
minValK[1]

#Now we have the training and the test data, we are able to run knn. We begin with a value of k = 3:
resultValid1 <- knn(train, valid, cl = heart_data[index_train, 10], k=minValK[1])
resultValid1

#install.packages("sp")
library(sp) 
knn.graph(resultValid1)
plot(valid[, ], col = as.factor(index_valid), pch = as.numeric(resultValid1))

#The inner command table(result, simT[index_test,3]) creates a table comparing the classification
#assigned to the test data by the k-nearest neighbour classifier against the true classification of the data
class_agree <- table(resultValid1, heart_data[index_valid,10])
class_agree

#The diag function selects the diagonal elements of the table, which is the number of points where knearest 
#neighbour classification agrees with the true classification
sum_agree <- sum(diag(class_agree))
sum_agree


#remainder of the code ensures that the output that is returned is the percentage misclassification.
(nrow(test) - sum_agree) / nrow(test)

require(class)
?contour
