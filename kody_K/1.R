#approximate the value of a definite integral
midpointrule <- function(f, a, b, n=1){
  h <- (b-a)/n
  return(h*sum(f(a+h*(1:n)-h/2)))
}
print(midpointrule(function(x) x^2*sin(x), 0, pi, 10000))

trapezoidalrule <- function(f, a, b, n) {
  h <- (b - a) / n  # Width of each subinterval
  x <- seq(a, b, by = h)  # Subinterval endpoints
  return(h * (0.5 * f(a) + sum(f(x[-c(1, length(x))])) + 0.5 * f(b)))
}
print(trapezoidalrule(function(x) x^2*sin(x), 0, pi, 10000))


f <- function(x){return(2^x)}

plot(function(x) 2^x*log(2), xlim=c(0.2*pi), col='blue', lwd = 2)
h <- 0.000000001
plot(function(x) (f(x+h)-f(x))/h, xlim=c(0,2*pi), col='red', add=TRUE)

SimpsonRule <- function(f, a, b, n=1){
  h <- (b-a)/n
  hpul <- h/2
  suma <- f(a) + f(b)
  xs <- h*(1:n) + a - hpul
  suma <- suma + 4*sum(f(xs))
  if(n > 1){
    xl <- (xs + hpul)[-n]
    suma <- suma + 2*sum(f(xl))
  }
  return(suma*h/6)
}
print(SimpsonRule(function(x) x^2*sin(x), 0, pi, 10000))

NewtonRoot <- function(f,x0,tol=1e-6) {
  x <- x0
  h <- tol
  repeat{
    dx <- 2*h*f(x)/(f(x+h)-f(x-h))
    if(abs(dx) < tol) return(x)
    x <- x - dx
  }
}

print(NewtonRoot(function(x) x+2, 0))

