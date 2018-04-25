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

################################################################################################

# Lab 3: NCI60 Data Example pag 407

nci.labs=NCI60$labs 
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# perform PCA on the data after scaling the variables

pr.out=prcomp(nci.data, scale=TRUE)

Cols=function(vec){
   cols=rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))]) }

# plot the principal component score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
       xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,
       xlab="Z1",ylab="Z3")

# We can obtain a summary of the proportion of variance explained (PVE) of the first few principal 
# components using the summary() method for a prcomp object

summary(pr.out)

# plot the variance explained by the first few principal components
plot(pr.out) # Note that the height of each bar in the bar plot is given by squaring the corresponding 
             # element of pr.out$sdev

# PVE of each principal component and PVE of each principal component as a scree plot is more informative

pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
       col =" blue ")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="
       Principal Component ", col =" brown3 ")

# PVE can also be computed like this: 
summary(pr.out)$importance[2,]

# the elements of cumsum(pve) are given by:

summary(pr.out)$importance[3,]

# PAM
# scale genes
sd.data=scale(nci.data)

par(mfrow=c(3,1))
data.dist=dist(sd.data) ; data.dist
plot(hclust(data.dist), labels=nci.labs, main="Complete
       Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
       main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
       main="Single Linkage", xlab="", sub="",ylab="")

# use complete linkage hierarchical cluster- ing for the analysis
# cut the dendrogram at the height that will yield a particular number of clusters

hc.out=hclust(dist(sd.data)) 
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

# plot the cut on the dendrogram that produces these four clusters

par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")

# Printing the output of hclust gives a useful brief summary of the object
hc.out

# The same but using kmeans
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters ,hc.clusters ) # the results are very different for PAM vs Kmeans

# Rather than performing hierarchical clustering on the entire data matrix, we can simply 
# perform hierarchical clustering on the first few principal component score vectors, as follows

hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First
       Five Score Vectors ")
table(cutree(hc.out,4), nci.labs)


################################################################################################
# Homework

Element <- c(1:6)
X1 <- c(4, 5, 6, 10, 14, 15)
X2 <- c(2, 5, 11, 2, 7, 9)
dat <- cbind(Element, X1, X2); dat
round(x, digits = 0)

round(dist(dat), digits = 1)


