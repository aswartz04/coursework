#the following finds 7 averages each of varying randomly generated samples from N(10,25)
#the average from the random sample is compared to the true mean by plotting each average
#it is expected that higher sample sizes will give more precise averages
set.seed(8846)
y1 <- rnorm(100, 10, 25)
mean1 <- mean(y1)
error1 <- mean1-10

y2 <- rnorm(500, 10, 25)
mean2 <- mean(y2)
error2 <- mean2-10

y3 <- rnorm(1000, 10, 25)
mean3 <- mean(y3)
error3 <- mean3-10

y4 <- rnorm(5000, 10, 25)
mean4 <- mean(y4)
error4 <- mean4-10

y5 <- rnorm(10000, 10, 25)
mean5 <- mean(y5)
error5 <- mean5-10

y6 <- rnorm(50000, 10, 25)
mean6 <- mean(y6)
error6 <- mean6-10

y7 <- rnorm(100000, 10, 25)
mean7 <- mean(y7)
error7 <- mean7-10

x <- c(mean1, mean2, mean3, mean4, mean5, mean6, mean7)
z <- c(100, 500, 1000, 5000, 10000, 50000, 100000)

plot(z, x, xlab = "sample size", ylab = "sample mean")

abline(a = 10, b = 0, col = "red")
