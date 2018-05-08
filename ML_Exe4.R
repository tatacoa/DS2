# Ana Maria Sandoval
# Machine Learning Exercise 4
# Overview of EM clustering and a Practical Cluster Analysis of the PIOMAS data

setwd("~/DS2")

# Exercise 1 EM clustering

library(EMCluster)

help(USArrests)
summary(USArrests)
head(USArrests)
colnames(USArrests)

# Run a K-means clustering with 4 clusters and plot the clusters for 
# first 2 principal comonents

pr.out<-prcomp(USArrests,scale=TRUE)
km.out<-kmeans(USArrests,centers=4,nstart=20); km.out 
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1:2],labels=state.abb,col=km.out$cluster)

# EMCluster package -> set up an initial solution using init.EM 
# and then run the EM algorithm to get the optimal solution using 
# the emcluster function
# In the third step we obtain the probabilities of the optimal solution 
# by running one more e.step

emobj<- init.EM(USArrests, nclass = 4)
emclobj <- emcluster(USArrests, emobj, assign.class = TRUE)
emprobs <- round(e.step(USArrests, emobj = emclobj)$Gamma,3)

# When the data have more than two variables, the default plot for an EM-object 
# (with class emret) is A parallel coordinates plot:
plotem(emobj,USArrests,lwd=2)

# plot the 1st 2 principal components again and compare this with the K-Means plot
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1:2],labels=state.abb,col=emclobj$class)

# ook at each K-means cluster in more detail by returning the EM-algorithm probabilities
round(emprobs[km.out$cluster==1,],3)
round(emprobs[km.out$cluster==2,],3)
round(emprobs[km.out$cluster==3,],3)
round(emprobs[km.out$cluster==4,],3)

# Exercise 2 PIOMAS data
##data

PIOMAS <- read.table("../Downloads/PIOMAS.txt",header=FALSE,row.names=1)
names(PIOMAS)<-month.abb
dim(PIOMAS)
PIOMAS<-PIOMAS[-40,]
year<-as.numeric(row.names(PIOMAS))

# One line for each year is plotted using the function matplot
matplot(t(PIOMAS),type="l",xlab="Month",ylab="Sea Ice Volume")

# The mean values for each year can be plotted.
yave<-apply(PIOMAS,1,mean)
plot(year,yave,ylab="Sea Ice Volume")


# Start by looking at K-means clustering
# Choose a suitable value for K and look at the results graphically

### aca toca aplicar kmeans??? 
plot(year,yave,ylab="Sea Ice Volume",col="blue")
matplot(t(PIOMAS),type="l",xlab="Month",ylab="Sea Ice Volume",col="Blue")






