################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)


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
