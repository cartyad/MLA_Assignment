################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

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



