
#random infected in matrix
MAT <- matrix(0, 10, 10)

n <- 3

row1 <- sample(1:10, n, repl=T)
col1 <- sample(1:10, n, repl=T)

for(i in 1:n){
		MAT[row1[i],col1[i]] <- MAT[row1[i],col1[i]] + 1
	}

MAT

#my version infected in matrix
x <- rep(0, 100)

n <- 3

infect <- sample(1:100, n, repl=T)

for(i in 1:n){
		x[infect[i]] <- x[infect[i]] + 1
	}
x

#my code with one loop
set.seed(103)
gen <- 3    #generations
n <- 5    #first infected
infect <- 12 #reproduction rate of disease
lam <- 0.8
loc <- matrix(round(runif(2*n, min=0, max=100),0), nrow=n, ncol=2)

for(i in 1:gen){
		ycoord <- loc[i, 2]
		xcoord <- loc[i, 1]
		spread <- rexp(infect*n, lam)
		newinfect <- cbind(xcoord + round(spread, 0), 
                                     ycoord + round(spread, 0))
		loc <- rbind(loc, newinfect)
		n <- length(loc[,1])
	}

plot(loc, xlim=c(0, 100), ylim=c(0, 100), col="darkgreen")


#two loop way
loc <- matrix(round(runif(2*n, min=0, max=10),0), nrow=n, ncol=2)
for(i in 1:gen){
	for(j in 1:n){
			ycoord <- loc[j, 1]
			xcoord <- loc[j, 2]
			spread <- rexp(infect, lam)
			newinfect <- cbind(xcoord + round(spread, 0), 
                                     ycoord + round(spread, 0))
			loc <- rbind(loc, newinfect)
		}
		n <- length(loc[,1])
	}
plot(loc, xlim=c(0, 10), ylim=c(0, 10), col="darkgreen")



  coord <- matrix(round(runif(n=2*5, min=0, max=100),2),nrow=5, ncol=2) 
 

#rounded spread of disease
disperse2 <- function(nstart, nnew, ngen, exppar){
  coord <- matrix(round(runif(n=2*nstart, min=0, max=100),0),
             nrow=nstart, ncol=2)  
  ntot <- nstart
  for(i in 1:ngen){
      for(j in 1:ntot){
        tempx <- coord[i,1]
        tempy <- coord[i,2]
        newdir <- runif(n=nnew, min=-pi, max=pi)
        newdist <- rexp(n=nnew, rate=exppar)
        newx <- tempx + round(newdist * sin(newdir),0)
        newy <- tempy + round(newdist * cos(newdir),0)
        newcoord <- cbind(newx,newy)
        coord <- rbind(coord,newcoord)
      }
   ntot <- length(coord[,1])
   }
coord
}

tmat <- disperse2(nstart=6, nnew=2.5, ngen=3, exppar=0.5)
plot(tmat, xlim=c(0,100), ylim=c(0,100), xlab="East-West",
     ylab="North-South", col="Red")



#test version with one loop
set.seed(1)
nstart <- 5
nnew <- 12
ngen <- 3
exppar <- 0.8

 coord <- matrix(round(runif(n=2*nstart, min=0, max=100),0),
             nrow=nstart, ncol=2)  
  ntot <- nstart
  for(i in 1:ngen){
     
        tempx <- coord[i,1]
        tempy <- coord[i,2]
        newdir <- runif(n=nnew*ntot, min=-pi, max=pi)
        newdist <- rexp(n=nnew*ntot, rate=exppar)
        newx <- tempx + round(newdist * sin(newdir),0)
        newy <- tempy + round(newdist * cos(newdir),0)
        newcoord <- cbind(newx,newy)
        coord <- rbind(coord,newcoord)
      
   ntot <- length(coord[,1])
   
coord
}

#test lab
set.seed(103)
gen <- 3    #generations
n <- 4    #first infected
infect <- 2 #reproduction rate of disease
lam <- 0.1
loc <- matrix(round(runif(2*n, min=0, max=100),0), nrow=n, ncol=2)

ngrow <- n
for(i in 1:gen){
	for(j in 1:ngrow){
		ycoord <- loc[j, 2]
		xcoord <- loc[j, 1]
		spreadx <- rexp(n, lam)
		spready <- rexp(n, lam)
		newinfectx <- xcoord + round(spreadx, 0)
		newinfecty <- ycoord + round(spready, 0)
		newcoord <- cbind(newinfectx, newinfecty)
		loc <- rbind(loc, newcoord)
		}
		ngrow <- length(loc[,1])
	}

plot(loc, xlim=c(0, 100), ylim=c(0, 100), col="darkgreen")
points(loc[1:n,], col="red")


#test lab2
gen <- 5    #generations
n <- 5    #first infected
infect <- 12 #reproduction rate of disease
lam <- 0.15
loc <- matrix(round(runif(2*n, min=0, max=100),0), nrow=n, ncol=2)

ngrow <- n
for(i in 1:gen){
		
		ycoord <- loc[i*1:n, 2]
		xcoord <- loc[i*1:n, 1]
		direction <- runif(infect*ngrow, -pi, pi)
	      spreaddistance <- rexp(infect*ngrow, lam)
		newinfectx <- xcoord + spreaddistance*sin(direction)
		newinfecty <- ycoord + spreaddistance*cos(direction)
		newcoord <- cbind(newinfectx, newinfecty)
		loc <- rbind(loc, newcoord)
		
	ngrow <- length(loc[,1])
	}

plot(loc, xlim=c(0, 100), ylim=c(0, 100), col="darkgreen", pch = ".")
points(loc[1:n,1], loc[1:n,2], col="red")


#test lab 3
gen <- 5    #generations
n <- 10    #first infected
infect <- 10 #reproduction rate of disease
lam <- 0.1
loc <- matrix(round(runif(2*n, min=0, max=100),0), nrow=n, ncol=2)

ngrow <- n
for(i in 1:gen){
		ycoord <- loc[i*1:n, 2]
		xcoord <- loc[i*1:n, 1]
	      spreadx <- rexp(infect*ngrow, lam)
		spready <- rexp(infect*ngrow, lam)
		newinfectx <- xcoord + spreadx
		newinfecty <- ycoord + spready
		newcoord <- cbind(newinfectx, newinfecty)
		loc <- rbind(loc, newcoord)
		
	ngrow <- length(loc[,1])
	}

plot(loc, xlim=c(0, 100), ylim=c(0, 100), col="darkgreen")
points(loc[1:n,], col="red")



########################
#Time Based#
########################

lam <- 2
mu <- 3
dlt <- 0
m <- 5000
x <- numeric(m)
t <- numeric(m)
x[1] <- 5
for(i in 2:m){
		if(x[i-1]==0) {tau <- dlt
				   x[i] <- 1}
		if(x[i-1]>=1) {tau <- dlt + x[i-1]*(lam + mu)
				   pr <- c(x[i-1]*mu, dlt + x[i-1]*lam)/tau
				   x[i] <- x[i-1] + sample(c(-1, 1), 1, prob=pr)}
		t[i-1] <- rexp(1, tau)
	}
plot(c(0, cumsum(t)[1:100]), c(x[1], x[1:100]), type="S")
mx <- max(x)
t.avg <- numeric(mx+1)
n <- 1:(mx+1)
for(j in n){
		t.avg[j] <- sum(t[x==(j-1)])/sum(t)
	}
round(t.avg[1:21], 3)
L.sim <- sum((0:mx)*t.avg)
L.sim
mx
sum(t)



##TIME TO INFECT 100 PEOPLE

sample(c(-1, 0), 1, pr=c(0.7, 0.3)) #change of death
set.seed(12)
m <- 8
infectcount.eb <- c()
t.eb <- numeric(m)
infectcount.eb[1] <- 5
infect.eb <- 3
deathcount.eb <- numeric(m)

for(i in 2:m){
		incu <- sample(2:21, 1)
		deathtime <- sample(6:16, 1)
		deaths <- sum(sample(c(1, 0), infectcount.eb[i-1], pr=c(0.7, 0.3), repl=T))
		deathcount.eb[i] <- deaths
		infectcount.eb[i] <- infectcount.eb[i-1]*infect.eb - deaths
		t.eb[i] <- incu + deathtime
	}
infectcount.eb
deathcount.eb


#MEASLES
infectcount.me <- c()
t.me <- numeric(m)
infectcount.me[1] <- 5
infect.me <- 12
deathcount.me <- numeric(m)

for(i in 2:m){
		incu.me <- sample(10:12, 1) #incubation
		deathtime.me <- sample(4:7, 1) #someone dies 70% chance 
						     #this happens
		deaths.me <- sum(sample(c(1, 0), infectcount.me[i-1], pr=c(0.1, 0.9), repl=T))
		deathcount.me[i] <- deaths.me
		infectcount.me[i] <- infectcount.me[i-1]*infect.me - deaths.me
		t.me[i] <- incu.me + deathtime.me
	}
infectcount.me
deathcount.me
plot(cumsum(t.me), infectcount.me, type="S")


##SPATIAL ANALYSIS
set.seed(110)
gen <- 5    #generations
n <- 5    #first infected
infect <- 12 #reproduction rate of disease
lam <- 0.15
loc <- matrix(round(runif(2*n, min=0, max=100),0), nrow=n, ncol=2)
par(mfrow=c(2, 2))

ngrow <- n
for(i in 1:gen){
		
		ycoord <- loc[i*1:n, 2]
		xcoord <- loc[i*1:n, 1]
		direction <- runif(infect*ngrow, -pi, pi)
	      spreaddistance <- rexp(infect*ngrow, lam)
		newinfectx <- xcoord + spreaddistance*cos(direction)
		newinfecty <- ycoord + spreaddistance*sin(direction)
		newcoord <- cbind(newinfectx, newinfecty)
		loc <- rbind(loc, newcoord)
	if(i > 1){plot(loc, xlim=c(0, 100), ylim=c(0, 100), xlab="East/West Spread", 
			   ylab="North/South Spread", col="forestgreen", pch=".")
		    points(loc[1:n,1], loc[1:n,2], col="darkred", pch=16)}
		
	ngrow <- length(loc[,1])
	}

plot(loc, xlim=c(0, 100), ylim=c(0, 100), col="darkgreen", pch = "'")
points(loc[1:n,1], loc[1:n,2], col="red")


#EBOLA
lam <- 3/2
mu <- 5/7
m <- 5000
x <- numeric(m)
t <- numeric(m)
x[1] <- 5
for(i in 2:m){
		tau <- x[i-1]*(lam + mu)
		pr <- c(x[i-1]*mu, dlt + x[i-1]*lam)/tau
		x[i] <- x[i-1] + sample(c(-1, 1), 1, prob=pr)
		t[i-1] <- rexp(1, tau)
	}
plot(c(0, cumsum(t)[1:100]), c(x[1], x[1:100]), xlab="Elapsed time",
	ylab="Infection Size", type="S")
mx <- max(x)
t.avg <- numeric(mx+1)
n <- 1:(mx+1)
for(j in n){
		t.avg[j] <- sum(t[x==(j-1)])/sum(t)
	}
L.sim <- sum((0:mx)*t.avg)
L.sim
mx
sum(t)

#MEASLES TIME BASED
lam <- 12/4
mu <- 10
dlt <- 10
m <- 5000
x <- numeric(m)
t <- numeric(m)
x[1] <- 5
for(i in 2:m){
		if(x[i-1]==0) {tau <- dlt
				   x[i] <- 1}
		if(x[i-1]>=1) {tau <- dlt + x[i-1]*(lam + mu)
				   pr <- c(x[i-1]*mu, dlt + x[i-1]*lam)/tau
				   x[i] <- x[i-1] + sample(c(-1, 1), 1, prob=pr)}
		t[i-1] <- rexp(1, tau)
	}
plot(c(0, cumsum(t)[1:100]), c(x[1], x[1:100]), xlab="Elapsed time",
	ylab="Infection Size", type="S")
mx <- max(x)
t.avg <- numeric(mx+1)
n <- 1:(mx+1)
for(j in n){
		t.avg[j] <- sum(t[x==(j-1)])/sum(t)
	}
L.sim <- sum((0:mx)*t.avg)
L.sim
mx
sum(t)



	
		

