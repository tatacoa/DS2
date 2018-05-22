# Ana Maria Sandoval Jimenez
# Machine Learning Workshop 6
# Cross-Validation and Bootstrapping

library(boot)
library("ISLR")

# Model evaluation using cross-validation

?Auto
pairs(Auto)
attach(Auto)
hist(mpg)
names(Auto)
detach(Auto)
search()

# Quadratic regression model

plot(mpg~horsepower,data=Auto)
lm.hp =lm(mpg~horsepower ,data=Auto)
abline(lm.hp)
lm.quad.hp =lm(mpg~horsepower+I(horsepower^2) ,data=Auto)
summary(lm.quad.hp)

#define the predictor function and call it fq

fq<-function(x)
  lm.quad.hp$coefficients[1]+lm.quad.hp$coefficients[2]*x+
  lm.quad.hp$coefficients[3]*x^2
curve(fq,40,230,add=TRUE)


# Lab 5.3 Cross-Validation and the Bootstrap
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Seventh%20Printing.pdf
# pag 190 -194

# validation set aproach

# divide the data into 2 sets
set.seed(1)
train=sample (392,196)

# fit the training set using lm()
lm.fit=lm(mpg ~ horsepower ,data=Auto ,subset=train)

# use predict() to estimate the response for 
# all 392 observations
# use mean() to calculate MSE
# -train select obs that are not in train
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
# the estimate test MSE fit is 26,14
# poly() estimate test error
lm.fit2=lm(mpg ~ poly(horsepower ,2),data=Auto , subset=train)
mean((mpg - predict (lm.fit2 ,Auto ))[- train]^2)
lm.fit3=lm(mpg ~ poly(horsepower ,3),data=Auto , subset=train)
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)

# if we choose a dif training set, we obtain diff errors on the validation set
set.seed(2)
train=sample (392,196)
lm.fit=lm(mpg ~ horsepower ,subset=train) # linear
mean((mpg -predict (lm.fit ,Auto))[-train ]^2)
lm.fit2=lm(mpg ~ poly(horsepower ,2),data=Auto , subset=train) # quadratic
mean((mpg -predict (lm.fit2 ,Auto ))[- train]^2)
lm.fit3=lm(mpg ~ poly(horsepower ,3),data=Auto , subset=train) # cubic
mean((mpg -predict (lm.fit3 ,Auto ))[- train]^2)

# a model that predict mpg using a quadratic functions of horsepower
# performs better

#### Leave-One-Out Cross-Validation
glm.fit=glm(mpg ~ horsepower ,data=Auto)
coef(glm.fit)

lm.fit=lm(mpg ~ horsepower ,data=Auto)
coef(lm.fit)
# lm() and gml() yield identical linear reg models
# cv.glm() the two numbers in the delta vector -> cross validation results
cv.err=cv.glm(Auto ,glm.fit)
cv.err$delta # both are similar correspond to LOOCV

# repeat for polinomials from 1 to 5
cv.error=rep(0,5)
for (i in 1:5){
   glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
   cv.error[i]=cv.glm(Auto ,glm.fit)$delta [1]
}
cv.error # usig quadratic minimized MSE, higher polinomials dont...

###### k-Fold Cross-Validation

# cv.glm() can be use to implement k-fold CV
set.seed(17)
cv.error.10=rep(0 ,10)
cv.error.20=rep(0 ,10)
for (i in 1:10){
   glm.fit=glm(mpg ~ poly(horsepower ,i),data=Auto)
   cv.error.10[i]=cv.glm(Auto ,glm.fit ,K=10)$delta[1]
   cv.error.20[i]=cv.glm(Auto ,glm.fit ,K=10)$delta[2]
   }
cv.error.10 # faster computation time
cv.error.20
# first delta  value standart k-fold cv estimate
# second delta biased corrected version

###### The Bootsrap

#Create the original sample of length 75
xsamp<-rexp(75, rate = c(0.01, 0.05)); xsamp #choose a parameter value somewhere between 0.01 to 0.05
hist(xsamp,breaks=10,main="Histogram of sample",xlab="x")
mean(xsamp) #mean of original sample
#take one resample (length 75 with replacement)
#and calculate the mean of the resample
resamp<-sample(xsamp,75,replace=T)
mean(resamp)

B<-100
#100 Bootstrap resamples
bsmean<-rep(NA,B)
for(i in 1:B){
  resamp<-sample(xsamp,75,replace=T)
  bsmean[i]<-mean(resamp) #store the resampled mean for this iteration
}
hist(bsmean,breaks=15,main="Histogram of the bootstrapped means",xlab="mean(x)")
var(bsmean) #bootstrap estimate of the variance of our estimator
quantile(bsmean,c(0.025,0.975)) #95% Conf int
#how do you get a 90% CI?
#add the conf int to the histogram
abline(v=quantile(bsmean,c(0.025,0.975)),col=2)

# Estimating the Accuracy of a Linear Regression Model
boot.fn=function (data ,index)
   return(coef(lm(mpg ~ horsepower ,data=data , subset=index)))
boot.fn(Auto ,1:392)
# the boot.fn func can be used to  

coninue pag 195


