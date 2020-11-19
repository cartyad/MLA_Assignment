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
################################################################################################################################################
