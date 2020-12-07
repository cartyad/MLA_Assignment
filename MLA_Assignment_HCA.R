################################################################################################################################################
#MLA Assignment
################################################################################################################################################
#Read csv file
heartData<-read.csv("C:/Users/carty/Documents/Third Year MSISS/MLA Assignment/heart_data.csv")
heartData
summary(heartData)
dim(heartData)

#install.packages("factoextra")
library(flexclust)
library(factoextra)
#install.packages("caret")
library(caret)
library(devtools)
library(MASS)
#install.packages("ggbiplot")
require(ggbiplot)
#install.packages("ggplot2")
require(ggplot2)
#install.packages("binaryLogic")
library(binaryLogic)
require(binaryLogic)
library(dplyr)

################################################################################################################################################
#HIERARCHICAL CLUSTERING:
################################################################################################################################################
binaryConverted = function(dataSet)
{
  i=0
  binaryVariables= matrix(nrow=270, ncol=3)
  for(i in 1:270)
  {
    if(dataSet[i,2]==1)
    {
      binaryVariables[i,1]=0
    }
    else
    {
      binaryVariables[i,1]=1
    }
    
    if(dataSet[i,5]==1)
    {
      binaryVariables[i,2]=0
    }
    else
    {
      binaryVariables[i,2]=1
    }
    
    if(dataSet[i,7]==1)
    {
      binaryVariables[i,3]=0
    }
    else
    {
      binaryVariables[i,3]=1
    }
  }
  return(binaryVariables)
}


binaryMatrix =matrix(nrows=270,ncols=3)
binaryMatrix = binaryConverted(heartData)
binaryMatrix
binary= dist(binaryMatrix, method="binary")
cont=dist(heartData[,c(1,3,4,6,8,9)], method="manhattan")
install.packages("combinedist")
distanceFused=((3/9)*binary) +((6/9)*cont)
distanceFused
clustAll=hclust(distanceFused,method="complete")
plot(clustAll,xlab="Heart Data: Binary and Continuous" )
height_mean=mean(clustAll$height)
height_sd = sd(clustAll$height)
cut_off=height_mean+(3*height_sd)
dend=plot(clustAll,hang=-1)
abline(h=cut_off, lty=2, col=2)
abline(h=cut_off+7, lty=2, col=1)
rect.hclust(clustAll,k=5,border=2:5)


clust_label = cutree(clustAll, k = 5)
clust_label
classDivision = sapply(unique(clust_label), function(g)heartData$Class[clust_label ==g])
classDivision
counts = sapply(2:6,function(ncl)table(cutree(clustAll,ncl)))
names(counts) = 2:6
counts
fviz_cluster(list(data = distanceFused, cluster = clust_label))
group_5 = cutree(clustAll,5)
table(group_4)
aggregate(standard_heart,list(group_4),median)
aggregate(heartDataCont,list(group_4),median)
round(aggregate(heartData,list(group_5),mean),2)
round(apply(heartData, 2, mean), 2)
summary(heartData)#.mean
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
for (i in 1:length(g5)){
  if(g5[i] ==1)
  {
    count1s = count1s + 1
  }
  else
  {
    count2s = count2s +1
  }
}
count2s/ (count1s + count2s)

palette(rainbow(10))
plot(heart_data[,2], heart_data[,3], col = clust3_label)
pairs(heart_data_clustering, col = clust3_label)

