# Exercise 1 Regresion

# exc 5
speed = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70)
braking = c(18, 26, 33, 40, 46, 59, 72, 85, 97, 120, 141)

mean(speed); mean(braking)
var(speed); var(braking)
cov(speed,braking)
plot(speed,braking)
cor(speed,braking)
lm(speed ~ braking)
