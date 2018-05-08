# @Tata

# Load data
lab <- read.csv2("LabMeasurements-Color-Card.csv")
master <- read.csv2("MasterColorCard.csv")

# create an empty matrix to store the deltae values, the matrix is 546x66
# 546 row -> total of color cards (13x42)
# the first two columns are the row  and column in a sheet (6x7)
# the deltae for each card is stored rowwise row = card

deltae <- matrix(NA,nrow = 546, ncol=66, byrow = TRUE)
# put the values for the first two columns (indicates de card position in a sheet)
deltae[,1] <- lab[,1]; deltae[,1]
deltae[,2] <- lab[,2]; deltae[,c(1,2)]


# function to compute a deltae between two colors (color distance)
foo <- function (x,y,l,a,b) {
  m <- master[which(master$Crow ==  x & master$Ccol == y),]
  ml <- m$L
  ma <- m$a
  mb <- m$b
  del <- sqrt((ml - l)^2 + (ma - a)^2 + (mb - b)^2)
  del
}
foo(1,1,55.5869,0.1352,2.1851)
foo(1,1,51.641,0.169,3.054)

# take a row (single card and compute color distance deltae)
x <- lab[1,]; x
cardmat <- matrix(x[,-c(1:2)],nrow=64,ncol=3,byrow=TRUE); cardmat
colnames(cardmat) = c("L","a","b")
cardmat

# for loop to compute the color distance (deltae) of a card ...
v <- {}
for (i in 1:64) {
  cardmat[i,] -> mata
  master[i,] -> matb
  del <- sqrt((mata$L - matb$L)^2 + (mata$a - matb$a)^2 + (mata$b - matb$b)^2)
  v[i] <- del
}
v


####################################################################################
# loop for all the distances

# Load data
lab <- read.csv2("LabMeasurements-Color-Card.csv")
master <- read.csv2("MasterColorCard.csv")

# create an empty matrix to store the deltae values, the matrix is 546x66
# 546 row -> total of color cards (13x42)
# the first two columns are the row  and column in a sheet (6x7)
# the deltae for each card is stored rowwise row = card

deltae <- matrix(NA,nrow = 546, ncol=66, byrow = TRUE)
# put the values for the first two columns (indicates de card position in a sheet)
deltae[,1] <- lab[,1]; deltae[,1]
deltae[,2] <- lab[,2]; deltae[,c(1,2)]
dim(deltae); dim(lab)

# colnames(deltae)
colnames(deltae) <- c("Row", "Column", c(11:18, 21:28, 31:38, 41:48, 51:58, 61:68, 71:78, 81:88))
colnames(deltae)



distance_single <- {}
distance_row <- {}
for (i in 1:length(row.names(lab))) {
    cardmatrix <- matrix(lab[i,-c(1:2)],nrow=64,ncol=3,byrow=TRUE)
    colnames(cardmatrix) = c("L","a","b")
    for (j in 1:64) {
        cardmatrix[j,] -> matrixA
        master[j,] -> matrixB
        distance_single <- sqrt((matrixA$L - matrixB$L)^2 + (matrixA$a - matrixB$a)^2 + (matrixA$b - matrixB$b)^2)
        distance_row[j] <- distance_single
    }
    deltae[i,-c(1:2)] <- distance_row
}

head(deltae) 
              

