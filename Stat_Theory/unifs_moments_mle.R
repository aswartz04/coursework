#the two techniques used for parameter estimation, method of moments and maximum likelihood are compared
moments <- c()
MLE_b <- c()

for (i in 1:50){
		unifdata <- runif(25, 0, 12)

		moments <- c(moments, sqrt(24*mean(unifdata)))

		MLE_b <- c(MLE_b, max(unifdata[1:25]))
}
#the upper bound is 12 and the MLE will center on this value
#the MoM will be left skewed in that it is bound by the upper bound as it depends on mean of a uniform distribution
par(mfrow=c(1,2))
hist(moments)
hist(MLE_b)