# Excersice 1 Visualization

library(stats)
library(ggplot2)

data("eurodist")
as.matrix(eurodist)[1:5,1:5]
mds = cmdscale(eurodist); mds # reduce dimension from 21 to 2

plot(mds)
plot(mds, type = 'n')
text(mds[,1], mds[,2], labels(eurodist))

ggplot(as.data.frame(mds, aes(V1, -V2, label = rownames(mds))) +
         geom_text(check_overlap = TRUE) + theme_minimal() +
         xlab(''), ylab('') + scale_y_continuous(breaks = NULL), 
       scale_x_continuous(breaks = NULL))


dat = as.data.frame(mds)
lab = rownames(mds)

p <- ggplot(dat, aes(V1, -V2, label = lab ))
p + geom_text()
# Avoid overlaps
p + geom_text(check_overlap = TRUE)


