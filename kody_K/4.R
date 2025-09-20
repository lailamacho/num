#polynomial Approximation
#spočte koeficienty aproximačního polynomu pro zadané body x, y

LSA <- function(x, y, n){
  A <- matrix(0, n, n)
  b <- numeric(n)
  for (i in 1:n){
    for(j in 1:n){
      A[i, j] <- sum(x^{i+j-2})
    }
    b[i] <- sum(y*x^{i-1})
  }
  return(solve(A,b))
}

#Horner
#spočte hodnoty polynomu s koeficienty a v bodě x

Horner <- function(x, a){
  n <- length(a)
  res <-a[n]
  for (i in (n-1):1) {
    res <- res * x + a[i]
  }
  return(res)
}
print(Horner(x,coef))

n <- 30
x <- runif(n, -1, 1)
f <- sin(x) - cos(x)
print(f)

print(x)
y <- f + rnorm(n, mean=0, sd=0.1)
print(y)
plot(x,y,pch=19)
m <- 4
coef <- LSA(x, y, m)
print(coef)

plot(function(x) Horner(x, coef), add=TRUE, col="red", lw=2, xlim = c(-1, 1)) 
points(x, f, pch=19, col="blue")

Hornerr <- function(x, a){
  n <- length(a)
  res <- a[n]
  for(i in (n-1):1){
    res <- res*x + a[i]
  }
  return(res) 
}

print(Hornerr(1,c(5, 2)))

      