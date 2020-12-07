################################################################################################################################################
#MLA Assignment
###################################################==#############################################################################################
#Read csv file
heart_data<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heart_data
summary(heart_data)
dim(heart_data)

################################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS: Non-Standardised Cont Only
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

################################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS: Standardised Cont Only
################################################################################################################################################

#PCA USING CORRELATION
eigen(cor(heart_data[, c(1,3,4,6)]))
scaled<-prcomp(heart_data[, c(1,3,4,6)], scale=TRUE)
round(scaled$rotation, 2)
plot(scaled, main = "Heart Data")
plot(scaled, main = "Heart Data", type = "l")
fisher_var_explain1 <- (scaled$sdev^2) / (sum(scaled$sdev^2))
plot(fisher_var_explain1, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)

diag(cov(heart_data[, c(1,3,4,6)]))
c(var(heart_data[, 1]), var(heart_data[,3]), var(heart_data[, 4]), var(heart_data[, 6]))
newScaled<-predict(scaled)
head(newScaled, n = 10)
pairs(newScaled,col=heart_data[,10])


###############################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS: Non-Standardised All Variables
################################################################################################################################################

#PCA USING CORRELATION
eigen(cor(heart_data[, c(1,2,3,4,5,6,7,8,9)]))
allVars<-prcomp(heart_data[, c(1,2,3,4,5,6,7,8,9)])
round(allVars$rotation, 2)
plot(allVars, main = "Heart Data")
plot(allVars, main = "Heart Data", type = "l")
fisher_var_explain1 <- (allVars$sdev^2) / (sum(allVars$sdev^2))
plot(fisher_var_explain1, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)

diag(cov(heart_data[, c(1,2,3,4,5,6,7,8,9)]))
c(var(heart_data[, 1]), var(heart_data[,3]), var(heart_data[, 4]), var(heart_data[, 6]))
newallVars<-predict(allVars)
head(newallVars, n = 10)
pairs(newallVars,col=heart_data[,10])

################################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS: Non-Standardised All Variables
################################################################################################################################################

#PCA USING CORRELATION
eigen(cor(heart_data[, c(1,2,3,4,5,6,7,8,9,10)]))
allVars<-prcomp(heart_data[, c(1,2,3,4,5,6,7,8,9,10)])
summary(allVars)
round(allVars$rotation, 2)
plot(allVars, main = "Heart Data")
plot(allVars, main = "Heart Data", type = "l")
fisher_var_explain1 <- (allVars$sdev^2) / (sum(allVars$sdev^2))
plot(fisher_var_explain1, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)

diag(cov(heart_data[, c(1,2,3,4,5,6,7,8,9,10)]))
c(var(heart_data[, 1]), var(heart_data[,3]), var(heart_data[, 4]), var(heart_data[, 6]))
newallVars<-predict(allVars)
head(newallVars, n = 10)
pairs(newallVars,col=heart_data[,10])


################################################################################################################################################
#PRINCIPLE COMPONANTS ANALYSIS: Non-Standardised All Variables
################################################################################################################################################

#PCA USING CORRELATION
eigen(cor(heart_data[, c(1,2,3,4,5,6,7,8,9)]))
scaledVars<- scale(heart_data[,c(1,3,4,6,9)])
allVars<- cbind(scaledVars,heart_data[,c(2,5,7,8,10)])

allVars<-prcomp(allVars, scale=TRUE)
summary(allVars)
round(allVars$rotation, 2)
plot(allVars, main = "Heart Data")
plot(allVars, main = "Heart Data", type = "l")
fisher_var_explain1 <- (allVars$sdev^2) / (sum(allVars$sdev^2))
plot(fisher_var_explain1, type = "b", main = "Heart Data:", 
     xlab = "No. of components", ylab = "Proportion of variance explained", xaxt = "n")
axis(1, at = 1:9)

diag(cov(heart_data[, c(1,2,3,4,5,6,7,8,9,10)]))
c(var(heart_data[, 1]), var(heart_data[,3]), var(heart_data[, 4]), var(heart_data[, 6]))
newallVars<-predict(allVars)
head(newallVars, n = 10)
pairs(newallVars,col=heart_data[,10])

