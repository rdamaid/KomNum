# Metode Iterasi Jacobi
options(scipen = 999)
jacobi <- function (A, B, x0, epsilon) {
  n <- ncol(A)
  iter <- 0
  xi <- rep(0, n)
  e <- abs((xi - x0) / xi)
  while (all (e >= epsilon)) {
    iter <- iter + 1
    for (i in 1:n) {
      ax <- 0
      for (j in 1:n) {
        if (j!=i) {
          ax <- ax + A[i, j] * x0[j]
        }
      }
      xi[i] <- (B[i] - ax) / A[i, i]
    }
    e <- abs((xi - x0) / xi)
    x0 <- xi
  }
  list(solusi = xi, iterasi = iter)
}

x0 <- scan()
1
2
2

A <- scan()
4
4
-2
-1
-8
1
1
1
5

dim(A) <- c(3, 3)

B <- scan()
7
-21
15

epsilon <- 0.01
jacobi(A, B, x0, epsilon)
