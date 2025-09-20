n <- 10

# Vytvoření matice A
A <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    A[i, j] <- cos((i-1)*j) - j
  }
}

# Vytvoření vektoru y
y <- numeric(n)
for (i in 1:n) {
  y[i] <- sin(i)
}

# Upravená Gaussova eliminace s kontrolou dělení nulou
gauss_elimination <- function(A, y) {
  n <- length(y)
  
  # Přímý chod eliminace
  for (k in 1:(n-1)) {
    # Částečné pivotování
    pivot_row <- which.max(abs(A[k:n, k])) + k - 1
    if (A[pivot_row, k] == 0) {
      stop("Matice je singulární")
    }
    
    # Prohození řádků
    if (pivot_row != k) {
      A[c(k, pivot_row), ] <- A[c(pivot_row, k), ]
      y[c(k, pivot_row)] <- y[c(pivot_row, k)]
    }
    
    for (i in (k+1):n) {
      factor <- A[i, k] / A[k, k]
      A[i, k:n] <- A[i, k:n] - factor * A[k, k:n]
      y[i] <- y[i] - factor * y[k]
    }
  }
  
  # Zpětná substituce
  x <- numeric(n)
  x[n] <- y[n] / A[n, n]
  for (i in (n-1):1) {
    x[i] <- (y[i] - sum(A[i, (i+1):n] * x[(i+1):n])) / A[i, i]
  }
  
  return(x)
}

# Řešení soustavy s ošetřením chyb
x <- tryCatch({
  gauss_elimination(A, y)
}, error = function(e) {
  cat("Chyba při řešení soustavy:", e$message, "\n")
  cat("Používám pseudoinverzi pro aproximaci řešení\n")
  # Alternativní řešení pomocí SVD pseudoinverze (bez knihoven)
  svd_A <- svd(A)
  D <- diag(1/svd_A$d)
  D[abs(svd_A$d) < 1e-10] <- 0  # Oříznutí malých singulárních hodnot
  A_inv <- svd_A$v %*% D %*% t(svd_A$u)
  A_inv %*% y
})

# Ověření, zda řešení obsahuje konečné hodnoty
if (any(!is.finite(x))) {
  cat("Varování: Řešení obsahuje nekonečné nebo NaN hodnoty. Upravuji data.\n")
  x[!is.finite(x)] <- 0  # Nahrazení nekonečných hodnot nulou
}

# Interpolace pouze pokud máme konečné hodnoty
if (all(is.finite(x))) {
  # Newtonova interpolace
  newton_interpolation <- function(x, y) {
    n <- length(x)
    coef <- numeric(n)
    coef[1] <- y[1]
    
    dd <- matrix(0, n, n)
    dd[,1] <- y
    
    for (j in 2:n) {
      for (i in 1:(n-j+1)) {
        dd[i,j] <- (dd[i+1,j-1] - dd[i,j-1]) / (x[i+j-1] - x[i])
      }
      coef[j] <- dd[1,j]
    }
    
    polynomial <- function(z) {
      result <- coef[1]
      for (i in 2:n) {
        term <- coef[i]
        for (j in 1:(i-1)) {
          term <- term * (z - x[j])
        }
        result <- result + term
      }
      return(result)
    }
    
    return(polynomial)
  }
  
  p <- newton_interpolation(x, y)
  
  # (a) Průsečík s osou y
  y_intercept <- p(0)
  
  # (b) Integrál s kontrolou konečných mezí
  xMIN <- min(x[is.finite(x)])
  xMAX <- max(x[is.finite(x)])
  
  if (is.finite(xMIN) && is.finite(xMAX)) {
    # Numerická integrace
    integrate_polynomial <- function(f, a, b, n = 1000) {
      h <- (b - a) / n
      x <- seq(a, b, length.out = n + 1)
      y <- sapply(x, f)
      integral <- h * (0.5 * y[1] + sum(y[2:n]) + 0.5 * y[n+1])
      return(integral)
    }
    
    integral_value <- integrate_polynomial(p, xMIN, xMAX)
    
    # Vykreslení grafu
    x_plot <- seq(xMIN, xMAX, length.out = 100)
    y_plot <- sapply(x_plot, p)
    
    plot(x_plot, y_plot, type = "l", col = "blue", xlab = "x", ylab = "p(x)", 
         main = "Interpolační polynom p(x)")
    points(x, y, col = "red", pch = 19)
    abline(h = 0, lty = 2)
    
    # Výpis výsledků
    cat("Řešení x soustavy Ax = y:\n")
    print(x)
    
    cat("\nPrůsečík s osou y (p(0)):", y_intercept, "\n")
    
    cat("\nIntegrál p(x) od", xMIN, "do", xMAX, "je:", integral_value, "\n")
  } else {
    cat("Nelze vypočítat integrál - hranice nejsou konečné\n")
  }
} else {
  cat("Nelze provést interpolaci - řešení obsahuje nekonečné hodnoty\n")
  cat("Řešení x soustavy Ax = y:\n")
  print(x)
}