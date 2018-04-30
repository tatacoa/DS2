# Ana Maria Sandoval Jimenez
# Data Visualization Exercise 1
# PCA

library(stats)
library(ggplot2)

data("eurodist")
as.matrix(eurodist)[1:5,1:5]
mds = cmdscale(eurodist); mds # reduce dimension from 21 to 2
par(mfrow=c(1,2))
boxplot(dist(mds) - eurodist, horizontal = TRUE) # dist gives a dist matrix back
boxplot(eurodist, horizontal = TRUE)
dev.off()

# Difference for mds and Eurodist
# Huge: Geneva-Cologne, Rome-Athens
(dist(mds) - eurodist)[abs(dist(mds) - eurodist)>400]
as.matrix(eurodist)["Geneva","Cologne"] - as.matrix(dist(mds))["Geneva","Cologne"]

# Plot cmd
plot(mds)
plot(mds, type = 'n')
plot(mds, cex=0, axes=FALSE, asp=1, xlab="", ylab="")
text(mds, labels(eurodist), cex = 0.7, col = 'red', xpd=NA)
# Plot using ggplot2
dat = as.data.frame(mds)
lab = rownames(mds)
p <- ggplot(dat, aes(V1, -V2, label = lab ))
p + geom_text(check_overlap = TRUE) # Avoid overlaps

# Plot Geolocalization
load("GeoEurocities.RData")
plot(geodat$lon, geodat$lat, asp=1, cex=0)
text(geodat$lon, geodat$lat, label=geodat$city, cex=0.7)

# reflection in vertical direction seems useful
mdsreflected <- mds%*%diag(c(1,-1)) # reflection in vertical direction
mds%*%diag(c(1,-1)) == c(mds[,1], -mds[,2])

plot(mdsreflected, cex=0, axes=FALSE, asp=1, xlab="", ylab="")
text(mdsreflected, labels(eurodist), cex=0.8, xpd=NA)
text(mds[,1], -mds[,2], labels(eurodist), cex = 0.7, col = 'red', xpd=NA)

# Rotation
rot <- function(theta) 
  cbind(c(cos(theta), sin(theta)), 
        c(-sin(theta), cos(theta)))

mdsRot <- mdsreflected%*%rot(15*pi/8); mdsRot
plot(mdsRot, cex=0, axes=FALSE, asp=1, xlab="", ylab="")
text(mdsRot, labels(eurodist), cex=0.8, xpd=NA)

# use scale to overlay the two plots
## scale geo data (for obtaining mean and standard deviation)
scalegeo <- scale(geodat[,c("lon","lat")]); scalegeo
## bring mds results to the same mean and variance as geo data
classical <- scale(mdsRot)%*%diag(attr(scalegeo, "scaled:scale")) + matrix(attr(scalegeo, "scaled:center"),nrow=21, ncol=2,byrow=TRUE)
classical
mdsscale <- scale(mdsRot)

## create overlay plot
plot(geodat[,c("lon","lat")],xlim=c(-15,30),ylim=c(30,65))
points(classical,col="red")
segments(geodat[,"lon"],geodat[,"lat"], classical[,1], classical[,2], col="red")


#### Task 2: Method exploration with iris data

data(iris)
dim(iris)
iris[,1:5]
names(iris)
summary(iris)
plot(iris)
plot(iris, col=iris$Species)

par(mfrow=c(2,2))
plot(iris$Petal.Length)
boxplot(iris$Petal.Length~iris$Species)
boxplot(iris)
hist(iris$Petal.Length[1:50])
dev.off()

# cmdscale
irisCMD <- cmdscale(dist(iris[,1:4]))
cor(irisCMD, iris[,1:4])

## dim1: long and wide Petal, long sepal
## dim2: wide sepal, also a little longer; unrelated to Petal
plot(irisCMD, main="classical", pch=as.numeric(iris$Species), col=as.numeric(iris$Species))
legend("top",pch=1:3, col=1:3,legend=levels(iris$Species))


## all species have varying sepal widths, 
## setosa has distinctly smaller dimensions than the other two, 
## virginica has slightly larger dimensions than versicolor

## iterate over different runs of the SOM
require(kohonen)


## for iris data, not really necessary
old <- cur <- Inf
dat <- scale(iris[,-5])
## iterative improvement of SOM
for (i in 1:100){
  erg <- som(dat, grid=somgrid(8,3,"hexagonal"), rlen=1000)
  cur <- sum(erg$distances)
  if (cur<old){
    erg2 <- erg
    old <- cur
  }
}

plot.somgrid <- function (x, xlim, ylim, ...) 
{
  if (missing(xlim)) 
    xlim <- c(0, max(x$pts[, 1]) + min(x$pts[, 1]))
  if (missing(ylim)) 
    ylim <- c(max(x$pts[, 2]) + min(x$pts[, 2]), 0)
  plot(xlim, ylim, axes = FALSE, type = "n", xlab = "", 
       ylab = "", asp=1, ...)
}
assignInNamespace("plot.somgrid",
                  plot.somgrid, "kohonen")

som2pts <- function(x){
  stopifnot("kohonen" %in% class(x))
  x$grid$pts[x$unit.classif,]
}

som_out <- som2pts(erg2)

### layout
mm <- rbind(c(1,2),c(3,4))
widths <- c(1,1); heights <- c(3,2)
layout(mm, widths=widths, heights=heights)
### color palette
require(RColorBrewer)