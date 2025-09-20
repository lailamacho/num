solve_problem <- function() {
  # 1. Parameters
  h <- 0.001
  target_integral <- 2
  
  # 2. Function to compute integral difference
  compute_integral_diff <- function(y0) {
    x <- seq(0, 1, by = h)
    y <- numeric(length(x))
    y[1] <- y0
    
    for (i in 2:length(x)) {
      y[i] <- y[i-1] + h * (y[i-1] - x[i-1])
    }
    
    # Trapezoidal rule
    integral <- sum((y[-1] + y[-length(y)]) * h/2)
    return(integral - target_integral)
  }
  
  # 3. Bisection method
  a <- 1
  b <- 3
  tol <- 1e-6
  max_iter <- 100
  
  for (i in 1:max_iter) {
    c <- (a + b)/2
    fc <- compute_integral_diff(c)
    
    if (abs(fc) < tol) {
      # Final solution
      x <- seq(0, 1, by = h)
      y <- numeric(length(x))
      y[1] <- c
      
      for (i in 2:length(x)) {
        y[i] <- y[i-1] + h * (y[i-1] - x[i-1])
      }
      
      # Calculate final integral WITHOUT adding target_integral
      final_integral <- sum((y[-1] + y[-length(y)]) * h/2)
      
      return(list(
        y0 = c,
        integral = final_integral,
        solution = data.frame(x = x, y = y)
      ))
    }
    
    if (compute_integral_diff(a) * fc < 0) {
      b <- c
    } else {
      a <- c
    }
  }
  
  warning("Maximum iterations reached")
  return(NULL)
}

# Run and verify
result <- solve_problem()
cat("Optimal y0:", result$y0, "\n")
cat("Calculated integral:", result$integral, "\n")  # Should now be ≈2