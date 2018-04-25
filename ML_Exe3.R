# Ana Maria Sandoval
# Machine Learning Exercise 3
# Robust Clustering with PAM, Hierarchical Clustering

find.package("cluster")
library(cluster)

find.package("ISLR")
install.packages("ISLR")
update.packages()
library(ISLR)


# 1. PAM Clustering

help(USArrests)
summary(USArrests)
head(USArrests)
colnames(USArrests)

row.names(USArrests)<-state.abb
pairs(USArrests)

# PAM clustering with 4 clusters
pam.out<-pam(USArrests,k=4)
clusplot(pam.out,labels=3)


# silhouette width over 0.4 is good
?silhouette
sp<-silhouette(pam.out); sp
plot(sp,col=1:4)
mean(sp[,"sil_width"])
abline(v=mean(sp[,"sil_width"]))

# The mean silhouette width can be used to assess which is the best number of clusters
avesw.vec<-rep(NA,7)
for(i in 2:7)
  avesw.vec[i]<-mean(silhouette(pam(USArrests,k=i))[,"sil_width"])
plot(1:7,avesw.vec,type="b",ylim=c(0,0.6))
avesw.vec # cual tiene el largest mean silhouette width? 2???

# Rerun the PAM algorithm with the optimal number of clusters
pam.out<-pam(USArrests,k=2); pam.out
clusplot(pam.out,labels=3)
sp<-silhouette(pam.out)
plot(sp,col=1:2)
abline(v=mean(sp[,"sil_width"]))


# Compare the optimal PAM clustering with the K-Means result
km.out<-kmeans(USArrests,centers=2,nstart=20); km.out # se debe hacer antes prcomp?
table(km.out$cluster,pam.out$clustering)
km.out$cluster == pam.out$clustering

# silhouette using kmeans as input
sp<-silhouette(km.out$cluster, dist(USArrests))
plot(sp,col=1:2)

# 2 Carry out a similar analysis to Exercise 1 using PAM to see if outliers were influencing the results. 
# Hint: in clustplot use labels=0 as the row names are the row numbers which are not informative.

decathlon <- read.csv2("/Users/tata/Downloads/Zehnkampf2017Hamburg.csv",
                       stringsAsFactors = FALSE, fileEncoding = "ISO-8859-1", header = TRUE, sep = ";")[-c(1:3,seq(10,28,2))]

# transform times to seconds
hilf <- decathlon$Zeit.400m
decathlon$Zeit.400m <- 60*as.numeric(substr(hilf,1,2)) +
  as.numeric(gsub(",",".",substr(hilf,4,nchar(hilf))))
hilf <- decathlon$Zeit.1500m
decathlon$Zeit.1500m <- 60*as.numeric(substr(hilf,1,2)) +
  as.numeric(gsub(",",".",substr(hilf,4,nchar(hilf))))
colnames(decathlon) <- c("YearOfBirth", "Class", "Points",
                         "Day1", "Day2", "Time.100m", "LongJump", "ShotPut",
                         "HighJump", "Time.400m", "Hurdles", "Discus", "PoleVault",
                         "JavelinThrow", "Time.1500m")
#remove any competitors without complete data, and select just the event results
temp <- apply(decathlon,1,function(x) sum(is.na(x)))
clustermat<-decathlon[temp==0,6:15]
pairs(clustermat)

# PAM applyied to Decathlon data
clustermat_scaled <- scale(clustermat); clustermat_scaled
pam.out<-pam(clustermat_scaled , k=3)
clusplot(pam.out,labels=0)

# Rerun the PAM algorithm with the optimal number of clusters
sp<-silhouette(pam.out)
plot(sp,col=1:2)
abline(v=mean(sp[,"sil_width"])) # hay varios valores negativos en el Silhouet

# The mean silhouette width can be used to assess which is the best number of clusters
avesw.vec<-rep(NA,7)
for(i in 2:7)
  avesw.vec[i]<-mean(silhouette(pam(clustermat_scaled,k=i))[,"sil_width"])
plot(1:7,avesw.vec,type="b",ylim=c(0,0.6))
avesw.vec # cual tiene el largest mean silhouette width? todas son muy malas menos de 0.4



#### con y sin escalar cual es el mejor????



# PAM applyied to Decathlon data
clustermat_scaled <- scale(clustermat); clustermat_scaled
pam.out<-pam(clustermat , k=3)
clusplot(pam.out,labels=0)

# Rerun the PAM algorithm with the optimal number of clusters
sp<-silhouette(pam.out)
plot(sp,col=1:2)
abline(v=mean(sp[,"sil_width"])) # hay varios valores negativos en el Silhouet

# The mean silhouette width can be used to assess which is the best number of clusters
avesw.vec<-rep(NA,7)
for(i in 2:7)
  avesw.vec[i]<-mean(silhouette(pam(clustermat,k=i))[,"sil_width"])
plot(1:7,avesw.vec,type="b",ylim=c(0,0.6))
avesw.vec # cual tiene el largest mean silhouette width? todas son muy malas menos de 0.4
