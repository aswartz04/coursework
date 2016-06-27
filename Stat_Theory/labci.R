#Using a 95% confidence interval, the mean of 10 values simulated from N(0,2) are tested to see if they fall
#within the expected range.  Since there are 1000 iterations, 950 of them are expected to fall within it.
yes <- 0

for (i in 1:1000){

		r_sample <- rnorm(10, 0, 2)
		findmean <- mean(r_sample)
		findconfidlow <- findmean-1.96*(2/sqrt(10))
		findconfidhigh <- findmean+1.96*(2/sqrt(10))
			if ((findconfidlow<0) & (findconfidhigh>0)) yes <- yes+1 

		}
yes


#Similar to the previous simulation, values are now randomly generated from the t-distribution with df=9.
#As before, it is expected that 950 of the 1000 iterations fall within the 95% confidence interval.
yes <- 0

for (i in 1:1000){

		r_sample <- rt(10, 9)
		findmean <- mean(r_sample)
		findsd <- sd(r_sample)
		findconfidlow <- findmean-2.26*(findsd/sqrt(10))
		findconfidhigh <- findmean+2.26*(findsd/sqrt(10))
			if ((findconfidlow<0) & (findconfidhigh>0)) yes <- yes+1 

		}
yes