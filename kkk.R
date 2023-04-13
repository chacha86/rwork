v1 <- c(80, 70, 70, rep(60, 3), 50, 50, 40)

v1 <- seq(55, 65, length=200)
v2 <- seq(40, 80, length=200)
v3 <- seq(30, 90, length=200)

m <- mean(v1)
std <- sd(v1)

m2 <- mean(v2)
std2 <- sd(v2)

m3 <- mean(v3)
std3 <- sd(v3)

cat(m, std)
cat(m2, std2)
cat(m3, std3)

plot(v1, dnorm(v1, mean=m, sd=std), type='l') 
plot(v2, dnorm(v2, mean=m, sd=std), type='l', main="Normal distribution, X~N(0,1)") 
plot(v3, dnorm(v3, mean=m, sd=std), type='l', main="Normal distribution, X~N(0,1)") 

library(ggplot2)

dt1 <- data.frame(x=v1, y=dnorm(v1, mean=m, sd=std))
dt2 <- data.frame(x=v2, y=dnorm(v2, mean=m2, sd=std2))
dt3 <- data.frame(x=v3, y=dnorm(v3, mean=m3, sd=std3))

ggplot() +
  geom_line(data=dt1, aes(x=v1, y=dnorm(v1, mean=m, sd=std)), color="red", size=2) +
  geom_line(data=dt2, aes(x=v2, y=dnorm(v2, mean=m2, sd=std2)), color="blue", size=2) +
  geom_line(data=dt3, aes(x=v3, y=dnorm(v3, mean=m3, sd=std3)), color="green", size=2)
