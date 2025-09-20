# Funkce pro výpočet integrálu (lichoběžníkové pravidlo)
compute_integral <- function(i, j, n_segments = 1000) {
  exponent <- i + j - 2
  t <- seq(0, 1, length.out = n_segments + 1)
  integrand <- t^exponent
  h <- 1 / n_segments
  integral <- h * (0.5 * integrand[1] + sum(integrand[2:n_segments]) + 0.5 * integrand[n_segments + 1])
  return(integral)
}

# Funkce pro vytvoření matice A
create_matrix_A <- function(N) {
  A <- matrix(0, nrow = N, ncol = N)
  for (i in 1:N) {
    for (j in 1:N) {
      A[i, j] <- compute_integral(i, j)
    }
  }
  return(A)
}

# Funkce pro vytvoření vektoru b
create_vector_b <- function(A, N) {
  b <- numeric(N)
  for (i in 1:N) {
    b[i] <- sum(A[i, ])
  }
  return(b)
}

# Gaussova eliminace (bez pivotování, pro jednoduchost)
solve_gauss <- function(A, b) {
  n <- length(b)
  Ab <- cbind(A, b)
  
  # Dopředná eliminace
  for (col in 1:(n-1)) {
    for (row in (col+1):n) {
      factor <- Ab[row, col] / Ab[col, col]
      Ab[row, col:(n+1)] <- Ab[row, col:(n+1)] - factor * Ab[col, col:(n+1)]
    }
  }
  
  # Zpětná substituce
  x <- numeric(n)
  x[n] <- Ab[n, n+1] / Ab[n, n]
  for (row in (n-1):1) {
    x[row] <- (Ab[row, n+1] - sum(Ab[row, (row+1):n] * x[(row+1):n])) / Ab[row, row]
  }
  
  return(x)
}

# Hlavní výpočet pro N = 2 až 20
results <- list()
for (N in 2:20) {
  A <- create_matrix_A(N)
  b <- create_vector_b(A, N)
  x <- solve_gauss(A, b)
  
  results[[N]] <- list(A = A, b = b, x = x)
  
  # Výpis výsledků pro každé N
  cat("\n--- Výsledky pro N =", N, "---\n")
  cat("Matice A:\n")
  print(A)
  cat("\nVektor b:\n")
  print(b)
  cat("\nŘešení x:\n")
  print(x)
  cat("\n------------------------\n")
}