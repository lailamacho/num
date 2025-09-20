# Přesnější výpočet vnitřního integrálu ∫ e^(-αx) sin(x) dx od 0 do 2π
inner_integral <- function(alpha, n = 1000) {  # Zvýšený počet kroků
  if (alpha == 0) return(0)  # ∫sin(x)dx = 0 na [0, 2π]
  
  a <- 0
  b <- 2 * pi
  h <- (b - a) / n
  x <- seq(a, b, length.out = n + 1)
  y <- exp(-alpha * x) * sin(x)
  
  # Lichoběžníkové pravidlo (stabilnější než Simpson pro oscilující funkce)
  sum_y <- sum(y) - 0.5 * (y[1] + y[n + 1])
  integral <- h * sum_y
  
  return(integral)
}

# Výpočet vnějšího integrálu ∫ inner_integral(α) dα od 0 do p
outer_integral <- function(p, n = 10000) {
  if (p == 0) return(0)
  
  a <- 0
  b <- p
  h <- (b - a) / n
  alpha <- seq(a, b, length.out = n + 1)
  y <- sapply(alpha, inner_integral)
  
  # Lichoběžníkové pravidlo
  sum_y <- sum(y) - 0.5 * (y[1] + y[n + 1])
  integral <- h * sum_y
  
  return(integral)
}

# Hledání p pomocí metody sečen (rychlejší než půlení intervalu)
find_p <- function(target = 1.31848, tol = 1e-6, max_iter = 100) {
  # Počáteční odhady (p1 musí dát hodnotu < target, p2 > target)
  p1 <- 0.5
  p2 <- 2.0
  
  f1 <- outer_integral(p1) - target
  f2 <- outer_integral(p2) - target
  
  for (i in 1:max_iter) {
    if (abs(f2) < tol) return(p2)
    
    # Nový odhad pomocí metody sečen
    p_new <- p2 - f2 * (p2 - p1) / (f2 - f1)
    f_new <- outer_integral(p_new) - target
    
    # Aktualizace intervalů
    p1 <- p2
    f1 <- f2
    p2 <- p_new
    f2 <- f_new
    
    if (abs(f_new) < tol) break
  }
  
  return(p2)
}

# Výpočet p
p <- find_p()
print(paste("Hledané p =", round(p, 6)))
print(paste("Hodnota integrálu pro p =", p, "je:", outer_integral(p)))