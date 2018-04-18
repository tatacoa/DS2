data <- read.csv2('Marketing.csv')

Sales <- c(1585, 1819, 1647, 1496, 921, 1278, 1810, 1987, 1612, 1413)
Price <- c(12.50,10.00, 9.95, 11.50, 12.00, 10.00, 8.00, 9.00, 9.50, 12.50)

# 1)
dat <- as.data.frame(cbind(Sales, Price));
plot(x=dat$Price, y=dat$Sales)
# There is a negative correlation - the lower the price, the higher the sales.
lm.obj <- lm(Sales ~ Price, data=dat); lm.obj
lm.obj$coefficients

as.numeric(lm.obj$coefficients[1])
class(lm.obj$coefficients[1])

abline(lm.obj)
cor(dat$Sales, dat$Price)

# (c) Is there any connection between rX Y and β1 ?
# Ans: Yes, they are both negative.

# d
pred <- 2832.8734 + (-121.5887) * 13
pred

myfun1 <- function(x, a){
  r <- a*sin(x)
  return(r)
}
myfun1(pi/2,2)

my.lin.reg <- function(x, y) {
  if (length(x) != length(y))
    stop('Lengths of x and y are not the same')
  
  # scatter plot
  plot(x, y)
  
  lm.obj <- lm(y ~ x)
  
  abline(lm.obj)

  intercept <- as.numeric(lm.obj$coefficients[1])
  gradient <- as.numeric(lm.obj$coefficients[2])
  
  # print(intercept + (gradient * x))
  rss <- sum((y - (intercept + gradient * x))^2)
  
  return(rss)
}

my.lin.reg(Price, Sales)
