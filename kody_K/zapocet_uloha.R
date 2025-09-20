
TrapezoidalRule <- function(f, a, b, n) {
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  sum((f(x[-1]) + f(x[-(n+1)])) * h / 2)
}

Ia <- log(2)
x <- 1:21
y <- numeric(length(x))

for (i in seq_along(x)) {
  n <- 2^x[i]
  In <- TrapezoidalRule(function(x) 1/x, 1, 2, n)
  y[i] <- Ia - In
}

print(x)
print(y)

plot(x, y)

LSA <- function(x, y, basis) {
  m <- length(x)
  n <- length(basis(1))
  A <- matrix(basis(x), m, n)
  
  
  coef <- solve(t(A) %*% A, t(A) %*% y)
  return(coef)
}

basis <- function(x){
  return(c(1/(4^x), 1/(8^x)))
}


coef <- LSA(x, y, basis)
print(coef)

y_final <- numeric(length(x))

for (i in 1:length(x)) {
  y_final[i] <- coef[1] * (1/4)^x[i] + coef[2] * (1/8)^x[i]
}

#basis_vals <- lapply(basis, function(f) f(x))  # list vektorů
#weighted_vals <- Map(function(fx, c) fx * c, basis_vals, coef)
#y_final <- Reduce("+", weighted_vals)

x_fit <- seq(min(x), max(x), length.out = 100)
y_final1 <- numeric(length(x_fit))
print(x_fit)
print(y_final1)


y_final1 <- matrix(basis(x_fit),length(x_fit), 2) %*% coef
print(y_final1)
plot(x,y)
lines(x_fit, y_final1, col="red")
