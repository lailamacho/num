#lagrange vykresleni v grafu, lagrange vraci hodnotu polynomu v zadanem bode xa zadny predpis polynomu
#ale i tak se to da vykreslit v grafu

# Tvoje původní funkce
Lagrange <- function(xa, x, y) {
  n <- length(x)
  suma <- 0
  for(i in 1:n){
    nasobic <- 1
    for(j in 1:n){
      if(j != i) nasobic <- nasobic * (xa - x[j]) / (x[i] - x[j])
    }
    suma <- suma + y[i] * nasobic
  }
  return(suma)
}

# Interpolační body
x <- c(1, 2, 3, 4)
y <- c(2, 3, 5, 4)

# Vykreslení původních bodů
plot(x, y, col = "red", pch = 19, xlim = range(x), ylim = range(y) + c(-1, 1), main = "Lagrangeova interpolace")

# Jemné dělení osy x
x_vals <- seq(min(x), max(x), length.out = 300)

# Výpočet odpovídajících y hodnot
y_vals <- sapply(x_vals, function(xa) Lagrange(xa, x, y))

# Přidání hladké křivky do grafu
lines(x_vals, y_vals, col = "blue", lwd = 2)

TrapezoidalRule <- function(f, a, b, n = 1000) {
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  sum((f(x[-1]) + f(x[-(n+1)])) * h / 2)
}

vysledek <- TrapezoidalRule(function(x) x^2, 0, 1, 100)
print(vysledek)


LSA <- function(x, y, n) {
  X <- outer(x, 0:(n-1), "^")  # Vandermondova matice
  a <- solve(t(X) %*% X, t(X) %*% y)  # Řešení (X^T X) a = X^T y
  return(a)
}

# Data
x <- c(1, 2, 3, 4)
y <- c(2, 3, 5, 4)

# Stupeň polynomu (n-1)
n <- 4  # Aproximace kvadratickou funkcí (y = a₀ + a₁x + a₂x²)

# Výpočet koeficientů
koef <- LSA(x, y, n)
print(koef)  # Vypíše koeficienty [a₀, a₁, a₂]

# 2. Hornerovo schéma pro vyhodnocení polynomu
Horner <- function(a, x) {
  n <- length(a)
  y <- a[n]
  for (i in (n-1):1) y <- y * x + a[i]
  return(y)
}

# 5. Vytvoření sítě bodů pro vykreslení
x_fit <- seq(min(x), max(x), length.out = 100)
y_fit <- sapply(x_fit, function(xi) Horner(koef, xi))

# 6. Základní graf v R (bez ggplot2)
plot(x, y, col = "red", pch = 19, main = "Aproximace metodou nejmenších čtverců",
     xlab = "x", ylab = "y", xlim = c(min(x), max(x)), ylim = c(min(y), max(y)))
lines(x_fit, y_fit, col = "blue", lwd = 2)
grid()
legend("topleft", legend = c("Data", "Aproximace"), col = c("red", "blue"), pch = c(19, NA), lty = c(NA, 1))

midpoint_integral <- function(f, a, b, n) {
  h <- (b - a) / n
  I <- 0
  
  for (i in 0:(n - 1)) {
    xi <- a + i * h + h / 2 
    Si <- h * f(xi)
    I <- I + Si
  }
  
  return(I)
}

y_final_horner <- sapply(x, function(xi) {
  t <- (1/4)^xi
  t * (coef[1] + t * (coef[2] / 2))  # Hornerovo schéma
})

BisecRoot <- function(f,a,b){
  fa <- f(a)
  fb <- f(b)
  if(fa*fb<0) {
    repeat {
      c <- (a+b)/2
      if(c==a | c==b) return(c)
      fc <- f(c)
      if(fa*fc<0) {
        b <- c
        fb <- fc
      } else {
        a <- c
        fa <- fc
      }
    }
  } else {
    stop("f(a)*f(b) < 0 not satisfied")
  }
}

result <- BisecRoot(function(x) x^2 - 2, 1, 2)
print(result)

EulerStep <- function(f,x,y,h) {
  return(y+h*f(x,y))
}
#je v jine uloze


# Vypočítáme hodnoty všech bázových funkcí na x a násobíme koeficienty
basis_vals <- lapply(basis, function(f) f(x))  # list vektorů
weighted_vals <- Map(function(fx, c) fx * c, basis_vals, coef)

# Sečteme po řádcích (element-wise součet)
y_final <- Reduce("+", weighted_vals)
