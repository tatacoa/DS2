# Ana Maria Sandoval
# Machine Learning Exercise 2
# Higher Dimensional K-means, PCA

install.packages("rgl")
library("rgl")

set.seed=(10)
x=matrix(rnorm(75*3),ncol=3); x
x[1:25,1]=x[1:25,1]+5; x
x[51:75,2]=x[51:75,2]-6; x
truth<-rep(1:3,c(25,25,25)); truth
pairs(x,col=truth+1)
plot3d(x)


# Run K-means with 3 clusters
kmeans3 <- kmeans(x, centers = 3, nstart=20); kmeans3

#Compare the output clusters with the known clusters
table(kmeans3$cluster, truth)

#Colour the clusters and add the cluster means
plot3d(x,col=kmeans3$cluster+1)
plot3d(kmeans3$centers,add=TRUE,col=2:4,type="s")

#Repeat the above using just two clusters. 
#How can you describe the two clusters in one sentence?
# the clusters seems good to me!
kmeans2 <- kmeans(x, centers = 2, nstart=20); kmeans2
plot3d(x,col=kmeans2$cluster+1)
plot3d(kmeans2$centers,add=TRUE,col=2:4,type="s")

#Generate a new matrix called y with 10 columns instead of 3 
y=matrix(rnorm(75*10),ncol=10); dim(y)
y[1:25,1]=y[1:25,1]+5; y
y[51:75,2]=y[51:75,2]-6; y

#define the clusters in exactly the same way as above
truthy<-rep(1:3,c(25,25,25)); truthy
pairs(y,col=truth+1)
plot3d(y)

#Run kmeans with 3 centres on x and on y
kmeansy3 <- kmeans(y, centers = 3, nstart=20); kmeansy3
plot3d(y,col=kmeansy3$cluster+1)
plot3d(kmeansy3$centers,add=TRUE,col=2:4,type="s")

#Use the R function table to compare how many rows have been 
#correctly and incorrectly assigned
table(kmeansy3$cluster, truthy)

##############################################################

help(USArrests)
summary(USArrests)
head(USArrests)

# Obtain names of the four variables in USArrests
colnames(USArrests)

# For each variable obtain the mean and the standard deviation
summary(USArrests)

# Use prcomp to get the principal components for this dataset
?prcomp
pr.out <- prcomp(USArrests, scale = TRUE)

# What is the difference between pr.out$scale and pr.out$sd?
pr.out$scale; # centrando las variables???
pr.out$sd # Standart deviations

# Create the scree plot and cumulative variance plot plots as in James.
# The first two Principal components explain 87% of the variance, 
# so we will use just the first two

summary(pr.out)
plot(pr.out$sdev^2, type = "b")
abline(h=1)

#biplot
biplot(pr.out,xlabs=state.abb)

# Run K-means on the PCA data with 2 clusters and plot the results
km.out<-kmeans(pr.out$x,centers=2,nstart=20)
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out$cluster)

# How many states are in each cluster: 20 and 30
table(km.out$cluster)

# Adapt the code blow to obtain the within sum of squares using 1 to 10
# clusters plotting the results. At which value of k is there an "Elbow"?

wss.vec<-rep(NA,10)

for(k in 1:10){
  km.outs<-kmeans(pr.out$x[,1:2],centers=k,nstart=20)
  wss.vec[k]<-km.outs$tot.withinss
}
plot(wss.vec,type="b")

# Using the "best" number plot the principal components coloured by 
# cluster. With just 4 variables a pairs plot is also worthwhile

km.out4 <-kmeans(pr.out$x[,1:2],centers=4,nstart=20)
plot(pr.out$x[,1:2],type="n")
text(pr.out$x[,1],pr.out$x[,2],labels=state.abb,col=km.out4$cluster)











prcomp(~ Murder + Assault + Rape, data = USArrests, scale = TRUE)
plot(prcomp(USArrests))
summary(prcomp(USArrests, scale = TRUE))
biplot(prcomp(USArrests, scale = TRUE))



#
