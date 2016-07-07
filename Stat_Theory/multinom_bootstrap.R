#this program demonstrates the use of bootstrap for paramter estimation using a multinomial distribution
#the total counts are divided into cells, each having different probabilities of landing inside them
#a multinomial distribution is defined by one parameter, theta, which is used to find cell probabilities
#theta is the parameter to be estimated

#loop iterations
m <- 1000
#total count across all cells
n <- 3839

#cell probabilities
p <- c(0.5089,0.2411,0.2411,0.0089)

#initialize parameter found by bootstrap
theta.star <- numeric(m)

#bootstrap procedure
for(i in 1:m){
		y <- rmultinom(1,n,p)
		#the MLE for theta was found to be quadratic and so the following coefficients are used to compute it
		a <- y[1,]+y[2,]+y[3,]+y[4,]
		b <- -(y[1,]-2*y[2,]-2*y[3,]+y[4,])
		c <- -2*y[4,]
		theta.star[i] <- (-b+sqrt(b^2-4*a*c))/(2*a)
	}
cut <- seq(0.01,0.07,0.001)

hist(theta.star,breaks=cut,prob=T)

#the histogram should be cenetered around this estimated value for the parameter
theta.star.mean <- mean(theta.star)
theta.star.mean

theta.star.sd <- sd(theta.star)
theta.star.sd

#95% confidence intervals
theta.LCL <- quantile(theta.star,0.025)
theta.LCL
theta.UCL <- quantile(theta.star,0.975)
theta.UCL