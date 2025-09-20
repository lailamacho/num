# Vraci spocitany polynom a jeho derivaci v tom bode
HornerD <- function(a, x) {
	n <- length(a)
	y <- a[n]
	if(n > 1){
		yd <- y
		if(n > 2){
			for(i in (n-1):2){
				y <- y*x+a[i]
				yd <- yd*x + y
			}
		}
		y <- y*x+a[1]
		return(c(y,yd))
	}
	return(c(y,0))
}
print(HornerD(c(8,3,1), 3))

Polx <- function(a,x){
	n <- length(a)
	y <- 0
	yd <- y
	for(i in 1:n){
		y <- y + a[i]*x^(n-1)
		yd <- yd + (i-1)*a[i]*x^(n-2)
	}
}

Newton <- function(f,fd,x0,tol=1e-6){
	x <- x0
	repeat {
		dx <- f(x)/fd(x)
		if(abs(dx) < tol) return(x)
		x <- x - dx
	}
}

x <- seq(0,1,1e-3)
plot(x,cos(x), type="l")
abline(a=0,b=1)
inter <- Newton(function(x) cos(x)-x,function(x) -sin(x)-1,0)
points(inter, cos(inter), col="red", cex=2)

NewtonHorner <- function(a,x0,tol=1e-6){
	x <- x0
	repeat {
		y_yd <- HornerD(a,x)
		dx <- y_yd[1]/y_yd[2]
		if(abs(dx) < tol) return(x)
		x <- x - dx
	}
}

# Chebyshevovy polynomy
ChebyCoef <- function(n){
	a0 <- numeric(n)
	a0[1] <- 1
	a1 <- numeric(n)
	a0[2] <- 1
	for(i in 3:n){
		a <- 2*c(0,a1[-n])-a0
		a0 <- a1
		a1 <- a
	}
	return(a)
}

a <- ChebyCoef(5)

x <- seq(-1,1,1e-3)
plot(x,sapply(x, function(x) HornerD(a,x)[1],x))
