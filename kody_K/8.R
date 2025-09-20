# fixed point iteration (metoda proste iterace)
# konvergence/divergence
# y = f(x) => x = g(x)

g <- function(x) {
	return(sqrt(x+10)/x)
}

xstart <- 5
n <- 100
x <- numeric(n)
x[1] <- xstart
plot(g,xlim=c(0,xstart),ylim=c(0,xstart), col="blue")
abline(0,1, col="red") # a+b*x
for(i in 2:n){
	x[i] <- g(x[i-1])
	segments(x0=x[i-1],x1=x[i-1],y0=x[i-1],y1=x[i],col="green")
	segments(x0=x[i-1],x1=x[i],y0=x[i],y1=x[i],col="green")
	print(x[i])
}
# plot(x, type="b")

##################################################################

rm(list=ls())
g <- function(x) {
	return(x+(2/x)/2)
}

xstart <- 5
n <- 40
x <- numeric(n)
x[1] <- xstart
plot(g,xlim=c(0,xstart),ylim=c(0,xstart), col="blue")
abline(0,1, col="red") # a+b*x
for(i in 2:n){
	x[i] <- g(x[i-1])
	segments(x0=x[i-1],x1=x[i-1],y0=x[i-1],y1=x[i],col="green")
	segments(x0=x[i-1],x1=x[i],y0=x[i],y1=x[i],col="green")
	print(x[i])
}

##################################################################
# reseni diferencialnich rovnic pomoci teto metody

rm(list=ls())

step <- function(f,h,x0,y0){
	hpul <- 0.5*h
	x1 <- x0 + h
	y1 <- y0+h*f(x0,y0)
	for(i in 1:10){
		y1 <- y0+hpul*(f(x1,y1)+f(x0,y0))
	}
	return(y1)
}
f <- function(x,y) {
	return((1+y*y)/(x*y*(1+x*x)))
}

h <- 0.1
x <- seq(1,2,h)
n <- length(x)
y <- numeric(n)
y[1] <- 1
for(i in 2:n){
	y[i] <- step(f,h,x[i-1],y[i-1])
}
plot(x,y,col="blue")
lines(x,sqrt((3*x*x-1)/(1+x*x)), col="red")

###############################################################
# SIR model

rm(list=ls())

step <- function(f,h,x0,y0){
	hpul <- 0.5*h
	x1 <- x0 + h
	y1 <- y0+h*f(x0,y0)
	for(i in 1:10){
		y1 <- y0+hpul*(f(x1,y1)+f(x0,y0))
	}
	return(y1)
}
f <- function(x,y) {
	beta <- 1
	nu <-1
	dS <- beta*y[1]*y[2]
	dI <- nu*y[2]
	return(c(-dS,dS-dI,dI))
}

h <- 0.1
x <- seq(0,10,h)
n <- length(x)
y <- matrix(0,nrow=n,ncol=3)
y[1,] <- c(999,1,0)
for(i in 2:n){
	y[i,] <- step(f,h,x[i-1],y[i-1,])
}
points(x,y[,1], col="blue")
points(x,y[,2], col="red")
points(x,y[,3], col="green")


