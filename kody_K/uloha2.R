NewtonRoot <- function(f,fd,x0,tol=1e-6) {
  x <- x0
  repeat{
    dx <- f(x)/fd(x)
    if(abs(dx) < tol) return(x)
    x <- x - dx
  }
}
MidpointRule <- function(f,a,b,n=1e6) {
  h <- (b-a)/n
  return(h*sum(f(a+h*(1:n)-h/2)))
}

CentralDiff <- function(f,x,h=1e-6) {
  return((f(x+h)-f(x-h))/(2*h))
}

x <- seq(0.5,3,by=1e-2)
p <- function(v) {
  t <- 0.89
  return(8*t/(3*v-1)-3/(v*v))
}
y <- sapply(x, p)  # Use `x` instead of `v`
plot(x,y)
frwd <- function(y,g){
  NewtonRoot(function(x) p(x)-y, function(z) CentralDiff(function(x) p(x)-y, z),g)
}
f <- function(y) {
  a <- frwd(y,0.6)
  b <- frwd(y,2.9)
  MidpointRule(function(x) p(x)-y,a,b)
}
sol <- NewtonRoot(f,function(x) CentralDiff(f,x), 0.5)
print(sol)
plot(x,y,type="l")
abline(h=sol, col="red")  # Draw horizontal line at the solution `sol`
