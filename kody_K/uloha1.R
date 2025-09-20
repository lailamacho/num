# Eulerova metoda pro řešení y' = y - x
euler_method <- function(y0, h = 0.001) {
  x <- seq(0, 1, by = h)
  y <- numeric(length(x))
  y[1] <- y0
  
  for (i in 2:length(x)) {
    y[i] <- y[i-1] + h * (y[i-1] - x[i-1])
  }
  
  return(data.frame(x = x, y = y))
}

# Lichoběžníkové pravidlo pro integraci
trapezoidal_integral <- function(x, y) {
  n <- length(x)
  sum((y[-1] + y[-n]) * diff(x)/2)
}

# Funkce pro výpočet rozdílu mezi integrálem a požadovanou hodnotou 2
integral_difference <- function(y0) {
  solution <- euler_method(y0)
  trapezoidal_integral(solution$x, solution$y) - 2
}

# Metoda půlení intervalu pro nalezení y0
bisection_method <- function(f, a, b, tol = 1e-6, max_iter = 100) {
  for (i in 1:max_iter) {
    c <- (a + b)/2
    fc <- f(c)
    
    if (abs(fc) < tol) {
      return(c)
    }
    
    if (f(a) * fc < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  
  warning("Nedosaženo požadované tolerance v max_iter krocích")
  return((a + b)/2)
}

# Najdeme počáteční podmínku y0
y0_optimal <- bisection_method(integral_difference, 1, 3)

# Výsledné řešení
final_solution <- euler_method(y0_optimal)

# Výpis výsledků
cat("Optimální počáteční podmínka y(0) =", y0_optimal, "\n")
cat("Vypočtený integrál od 0 do 1:", 
    trapezoidal_integral(final_solution$x, final_solution$y), "\n")

# Grafické znázornění
plot(final_solution$x, final_solution$y, type = "l", 
     main = "Řešení y' = y - x s ∫y(x)dx = 2",
     xlab = "x", ylab = "y(x)")
grid()