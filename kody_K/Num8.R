# Hermit Cubic Spline
Hermite <- function(x0,x1,y0,y1,d0,d1){
  h <- x1-x0
  hrec <- 1/h
  delta <- (y1-y0)*hrec-d0
  Delta <- d1-d0
  a3 <- (Delta - 2*delta)
  return(c(y0,d0,(delta-a3)*hrec,a3*hrec*hrec))
}
Horner <- function(x,x0,coef){
  n <- length(coef)
  y <- coef[n]
  t <- x-x0
  for(i in (n-1):1) y <- y*t+coef[i]
  return(y)
}
fd <- function(f,x,h=0.0000001){
  return((f(x+h)-f(x-h))/(2*h))
  }

x0 <- -1
x1 <- 3
y0 <- 7
y1 <- -2
d0 <- -2
d1 <- 3

coef <- Hermite(x0,x1,y0,y1,d0,d1)
plot(function(x) Horner(x,x0,coef), xlim=c(x0-0.5,x1+0.5))
lines(c(x0,x1), c(y0,y1), col="red")

print(fd(function(x) Horner(x,x0,coef), x0)) #d0
print(fd(function(x) Horner(x,x0,coef), x1)) #d1

