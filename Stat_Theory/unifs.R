#a randomly generated sample from U(3,12) is found of size n=400
#the mean, var, and MLE for each parameter is computed and compared with theoretical values
means <- c()
var <- c()
MLE_a <- c()
MLE_b <- c()

for (i in 1:25){
		unifdata <- runif(400, 3, 12)


		means <- c(means, mean(unifdata))

		var <- c(var, var(unifdata))

		MLE_a <- c(MLE_a, min(unifdata[1:400]))

		MLE_b <- c(MLE_b, max(unifdata[1:400]))
}
#E(X) = 7.5
mean
#VAR(X) = 6.75
#MLE for lower bound is min(X) = 3
MLE_a
#MLE for upper bound is max(X) = 12
MLE_b