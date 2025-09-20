GeomMethod <- function(f,a,b,h,n){
	x <- runif(n,a,b)
	y <- runif(n,0,h)
	return(h*(b-a)*sum(f(x) > y)/n)
}

na <- 1000
x <- replicate(na, GeomMethod(sin, 0, pi, 1, n=1000))
cat(mean(x), "pm", sd(x), "\n")

AveMethod <- function(f,a,b,n){
	return((b-a)*mean(f(runif(n,a,b))))
}

# Monte Carlo Trunec (skripta)

# symetrizace
AveMethod(function(x) sin(x)+sin(pi+0-x)/2, 0, pi, 1000)

# gaussova kvadrantura (integral na <-1;1>)
Gauss2 <- function(f) {
	x <- 1/sqrt(3)
	return(f(x)+f(-x))
}

# gaussova kvadrantura (integral na <a;b>)
Gauss2e <- function(f,a,b) {
	x <- 1/sqrt(3)
	slope <- (b-a)/2
	intercept <- (b+a)/2
	return(slope*sum(f(slope*c(-x,x) + intercept)))
}

Gauss3 <- function(f,a,b) {
	x <- sqrt(3/5)
	x <- c(-x,0,x)
	w <- c(5,8,5)/9
	slope <- (b-a)/2
	intercept <- (b+a)/2
	return(slope*sum(w*f(slope*x + intercept)))
}

Gauss4 <- function(f,a,b) {
	x1 <- sqrt((3/7) - (2/7)*sqrt(6/5))
	x2 <- sqrt((3/7) + (2/7)*sqrt(6/5))
	x <- c(x1, -x1, x2, -x2)
	w <- c(0.65, 0.65, 0.34, 0.34)
	slope <- (b-a)/2
	intercept <- (b+a)/2
	return(slope*sum(w*f(slope*x + intercept)))
}

Horner <- function(coef, x) {
	n <- length(coef)
	res <- coef[n]
	for(i in (n-1):1){res <- res*x+coef[i]}
	return(res)
}

# opravit
IntegratePoly <- function(coef, a,b) {
	n <- length(coef)
	coef <- coef/1:n
	x <- c(a,b)
	res <- coef[n]
	for(i in (n-1):1){res <- res*x+coef[i]}
	return(res[2]-res[1])
}
