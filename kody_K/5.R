EulerStep <- function(f,x,y,h) {
	return(y+h*f(x,y))
}

EulerStepIIa <- function(f,x,y,h) {
	hpul <- h*0.5
	return(y+h*f(x+hpul,EulerStep(f,x,y,hpul)))
}

EulerStepIIb <- function(f,x,y,h) {
	fn <- f(x,y) + f(x,EulerStep(f,x,y,h)) / 2
	return(y+h*fn)
}

RK4 <- function(f,x,y,h) {
	hpul <- h*0.5
	k1 <- f(x,y)
	xshift <- x - hpul
	k2 <- f(xshift, y + hpul*k1)
	k3 <- f(xshift, y + hpul*k2)
	k4 <- f(x + h, y + h*k3)
	return(y+h*(k1+2*(k2+k3)+k4)/6)
}

f <- function(x,y){return(-y)}

tMax <- log(2)
N <- 10
dt <- tMax/N
t <- (0:N)*dt
t
n0 <- 300

n <- t
n[1] <- n0
for(i in 1:N) n[i+1] <- EulerStep(f, t[i], n[i], dt)



plot(function(t) n0*exp(-t), xlim=c(0,tMax), col="red")
points(t, n, col="blue")

nIIa <- t
nIIa[1] <- n0
for(i in 1:N) nIIa[i+1] <- EulerStepIIa(f, t[i], nIIa[i], dt)
points(t, nIIa, col="green")

nIIb <- t
nIIb[1] <- n0
for(i in 1:N) nIIb[i+1] <- EulerStepIIb(f, t[i], nIIb[i], dt)
points(t, nIIb, col="cyan")

nRK4 <- t
nRK4[1] <- n0
for(i in 1:N) nRK4[i+1] <- RK4(f, t[i], nRK4[i], dt)
points(t, nRK4, col="violet")

# ------------------------------------------------------
fS <- function(x,y){return(c(y[2], -y[1]-y[2]))}

tMax <- 4*pi
N <- 100
dt <- tMax/N
t <- (0:N)*dt
n0 <- 300

x <- matrix(nrow=N+1, ncol=2)
x[1,1] <- 1
x[1,2] <- 0
for(i in 1:N) x[i+1,] <- EulerStep(fS, t[i], x[i,], dt)
plot(y)
points(x[1,], col="blue")
