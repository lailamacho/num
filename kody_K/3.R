AddNewtonCoef <- function(x,y,a) {
	n <- length(x)
	scitani <- 0
	nasobeni <- 1
	for(i in 1:(n-1)) {
		scitani <- scitani + a[i]*nasobeni
		nasobeni <- nasobeni*(x[n]-x[i])
	}
	return((y[n]-scitani)/nasobeni)
}

NewtonPoly <- function(z, a, x) {
	n <- length(a)
	res <- a[n]
	if(n < 1) {
		for(i in (n-1):1) res <- res*(z-x[i]) + a[i]
	}
	return(res)
}

n <- 10
x <- 1:n
y <- sin(x)-cos(x)
plot(x,y,col="red", pch=19)

a <- y[1]
# for(i in 2:n) a[i] <- AddNewtonCoef(x[1:i],y[1],a)
# 
z <- seq(x[1], x[n], 0.1)
print(z)
# for(i in 1:n) lines(z, NewtonPoly(z, a[1:n], x[1:n]), col=i+1)

lagrange1 <- function(xa, x, j){
	n <- length(x)
	res <- 1
	for(i in 1:n) {
		if(i != j) res <- res*(xa-x[i])/(x[j]-x[i])
	}
	return(res)
}
lagrange2 <- function(xa, x, y){
	n <- length(x)
	res <- 0
	for(i in 1:n) res <- res+y[i]*lagrange1(xa,x,i)
	return(res)
}

print(lagrange1(20, x, 1))
lines(z,lagrange2(z,x,y))
print(lagrange2(z,x,y))
Horner <- function(x, a){
  n <- length(a)
  res <-a[n]
  for (i in (n-1):1) {
    res <- res * x + a[i]
  }
  return(res)
}
print(Horner(0.1,lagrange2(z,x,y)))
LSA <- function(x, y, n){
  X <- matrix(1, nrow=n, ncol=length(x))
  for(i in 2:n) X[i, ] <- X[i-1, ]*x
  return(c(solve(X%*%t(X), X%*%y)))
}
a <- runif(100,-1,1)
b <- exp(a)
plot(a,b)
m=5
coef <- LSA(a,b,m)
print(LSA(a,b,m))
plot(function(x) Horner(x, coef), add=TRUE, col="red", lw=2,xlim = c(-1,1))
