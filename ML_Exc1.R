# Workshop 1 ML

set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]<-x[1:25,1]+2
x[1:25,2]<-x[1:25,2]-2
plot(x,pch=16)

km.out<-kmeans(x,centers=2,nstart=1) #run with two clusters
km.out$cluster #a vector specifying which cluster each row belongs to
names(km.out) #all the different outputs from kmeans
km.out$totss #the sum of squares without clustering
km.out$tot.withinss #the sum of squares with this clustering
km.out$withinss #the sum of squares within each cluster
km.out$centers #Matrix with the center coordinates
plot(x,col=km.out$cluster+1,pch=16) #plot the poiunts colorfed by cluster
points(km.out$centers,col=2:3,pch=3) #add the cluster centers

km.out<-kmeans(x,centers=2,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:3,pch=3)
km.out$tot.withinss

set.seed(4)
km.out<-kmeans(x,centers=3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out<-kmeans(x,centers=4,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
km.out$tot.withinss

x<-matrix(rnorm(50*3),ncol=2)
x[1:25,1]<-x[1:25,1]+2
x[1:25,2]<-x[1:25,2]-2
x[50+1:25,1]<-x[50+1:25,1]+2
x[50+1:25,2]<-x[50+1:25,2]+2
km.out<-kmeans(x,3,nstart=20)
plot(x,col=km.out$cluster+1,pch=16)
points(km.out$centers,col=2:4,pch=3)

install.packages("dplyr")
library(dplyr)
install.packages("mdsr")
library(mdsr)

names(WorldCities)
dim(WorldCities)


