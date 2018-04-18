# Ana Maria Sandoval Jimenez
# Data Visualization Exercise 1
# PCA

library(stats)
library(ggplot2)

data("eurodist")
as.matrix(eurodist)[1:5,1:5]
mds = cmdscale(eurodist); mds # reduce dimension from 21 to 2

# Plot cmd
plot(mds)
plot(mds, type = 'n')
text(mds[,1], -mds[,2], labels(eurodist), cex = 0.7, col = 'red')


# Plot Geolocalization
plot(geodat$lon,geodat$lat, type = 'n')
text(geodat$lon,geodat$lat, labels = geodat$city, cex = 0.7)

# Overlay cmd and geolocalization
# use scale to overlay the two plots

mds_scale <- scale(mds)
geodat_scale <- scale(cbind(geodat$lon,geodat$lat))

plot(mds, type = 'n', xlab = "Lon", ylab = "Lat", xlim=range(c(mds[,1],geodat$lon)),
     ylim=range(c(mds[,2],geodat$lat)))
text(mds[,1], -mds[,2], labels(eurodist), cex = 0.7, col = 'red')

par(new=TRUE)
plot(geodat$lon,geodat$lat, type = 'n', axes = FALSE, bty = "n", xlab = "", ylab = "", 
     xlim=range(cbind(mds[,1],geodat$lon)),
     ylim=range(cbind(mds[,2],geodat$lat)))
text(geodat$lon,geodat$lat, labels = geodat$city, cex = 0.7)

# Plot using ggplot2
dat = as.data.frame(mds)
lab = rownames(mds)
p <- ggplot(dat, aes(V1, -V2, label = lab ))
p + geom_text(check_overlap = TRUE) # Avoid overlaps

#### Task 2: Method exploration with iris data

data(iris)
dim(iris)
iris[,1:5]
names(iris)
summary(iris)
plot(iris)
plot(iris, col=iris$Species)

par(mfrow=c(1,2))
plot(iris$Petal.Length)
boxplot(iris$Petal.Length~iris$Species)
boxplot(iris)
hist(iris$Petal.Length[1:50])
