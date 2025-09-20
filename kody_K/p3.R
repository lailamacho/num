n <- 10
A <- matrix(nrow=n,ncol=n)
for (i in 1:n) for (j in 1:n) A[i,j] <- cos((i-1)*j) - j
y <- sin(1:n)

GaussPivot <- function(A,b) {
	Ab <- cbind(A, b)
	n <- length(b)
	for(k in 1:(n-1)) {
		pivot <- which.max(abs(Ab[k:n,k])) + k - 1
		if(pivot != k){
			j <- k:(n+1)
			pom <- Ab[k,j]
			Ab[k,j] <- Ab[pivot,j]
			Ab[pivot,j] <- pom
		}
		for (i in (k+1):n) {
			j <- (k+1):(n+1)
			nasobek <- Ab[i,k] / Ab[k,k]
			Ab[i,j] <- Ab[i,j] - nasobek*Ab[k,j]
		}
	}
	x <- b
	x[n] <- Ab[n,n+1] / Ab[n,n]
	for(i in (n-1):1){
		j <- (i+1):n
		x[i] <- (Ab[i,n+1]-sum(Ab[i,j]*x[j]))/Ab[i,i]
	}
	return(x)
}

x <- GaussPivot(A,y)

l <- function(xa, x, j){
  res <- 1
  for(i in 1:length(x)){
    if(i != j) res <- res*(xa-x[i])/(x[j]-x[i])
  }
  return(res)
}
Lagrange <- function(xa, x, y){
  res <- 0
  for(i in 1:length(x)) res <- res+y[i]*l(xa, x, i)
  return(res)
}


z <- seq(x[1])
lines(Lagrange(x,x,y))
