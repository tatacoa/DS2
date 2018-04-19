# Ana Maria Sandoval Jimenez
# Exercise 1 Regresion
# Linear Models


# exc 5
speed = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
braking = c(18, 26, 33, 40, 46, 59, 72, 85, 97, 120, 141)

mean(speed); mean(braking)
var(speed); var(braking)
cov(speed,braking)
plot(speed,braking)
cor(speed,braking)
l <- lm(speed ~ braking)
l$coefficients
plot(speed,braking)
abline(l)

data = read.csv2("/Users/tatacoa/Downloads/Miete2003.csv")
summary(data)



# Estimate the model for wohnflaeche and nettomiete
lm2 <- lm(data$wohnflaeche ~ data$nettomiete)
# calculate the coefficients
lm2$coefficients
# draw scatteplots and regression lines
plot(data$wohnflaeche ~ data$nettomiete)
abline(lm2, col= "red")
# determine R-squared.
summary(lm2)


# exc 6
# consider nettomiete and wohnlage, vizualice them
plot(data$wohnlage, data$nettomiete)
boxplot(nettomiete~wohnlage, data=data)
boxplot(data$nettomiete ~ data$wohnlage, data=data)

# consider bad.kacheln and geh.kueche
plot(data$bad.kacheln, data$geh.kueche)
table(data$bad.kacheln, data$geh.kueche)


# find the correlation between two binary variables
library("psych")
phi(table(data$bad.kacheln, data$geh.kueche))

# use chi-squared test on binary variabes
chisq.test(table(data$bad.kacheln, data$geh.kueche))

