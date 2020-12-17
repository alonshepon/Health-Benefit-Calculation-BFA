




# Distribution 1
meanlog1 <- 5.932304
sdlog1 <- 0.4752817
mu1 <- exp(meanlog1 + sdlog1^2/2)


x <- 1:5000
y1 <- dlnorm(x=1:5000-1000, meanlog=meanlog1, sdlog=sdlog1)
plot(x, y1, type="l")

mu2 <- 500
scalar <- mu2/mu1

y2 <- dlnorm(x=1:5000, meanlog=meanlog1*scalar, sdlog=sdlog1)
lines(x, y2, color="red")
