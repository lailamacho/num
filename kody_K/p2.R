MidpointRule <- function(f,a,b,n=1e6) {
	h <- (b-a)/n
	return(h*sum(f(a+h*(1:n)-h/2)))
}

NewtonRoot <- function(f,fd,x0,tol=1e-6) {
	x <- x0
	repeat{
		dx <- f(x)/fd(x)
		if(abs(dx) < tol) return(x)
		x <- x - dx
	}
}

CentralDiff <- function(f,x,h=1e-6) {
	return((f(x+h)-f(x-h))/(2*h))
}

f <- function(p) m(function(alpha) m(function(x) exp(-alpha*x)*sin(x),0,2*pi),0,p)
