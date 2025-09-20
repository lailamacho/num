# Nastavení parametrů
n <- 50  # řád matice
max_iter <- 10000  # maximální počet iterací
tolerance <- 1e-6  # kritérium konvergence

# Inicializace matice
A <- matrix(0, nrow = n, ncol = n)
A[1, ] <- 100  # první řádek = 100

# Iterativní řešení
for (iter in 1:max_iter) {
  max_diff <- 0
  
  # Procházíme vnitřní buňky (od 2 do n-1 v řádcích i sloupcích)
  for (i in 2:(n-1)) {
    for (j in 2:(n-1)) {
      old_val <- A[i, j]
      # Nová hodnota jako průměr sousedů
      new_val <- 0.25 * (A[i-1, j] + A[i+1, j] + A[i, j-1] + A[i, j+1])
      A[i, j] <- new_val
      # Sledujeme maximální změnu
      diff <- abs(old_val - new_val)
      if (diff > max_diff) max_diff <- diff
    }
  }
  
  # Kontrola konvergence
  if (max_diff < tolerance) {
    cat("Konvergence dosažena po", iter, "iteracích\n")
    break
  }
  
  if (iter == max_iter) {
    cat("Dosažen maximální počet iterací bez konvergence\n")
  }
}

# Výsledek - hodnoty vnitřních buněk
# Pro zobrazení můžeme vybrat část matice, např:
inner_values <- A[2:(n-1), 2:(n-1)]
print(inner_values[1:5, 1:5])  # ukázka prvních 5x5 vnitřních hodnot
print(A)
