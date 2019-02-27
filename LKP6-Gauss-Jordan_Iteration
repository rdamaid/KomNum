# Metode Eliminasi Gauss-Jordan
options(scipen = 999)
gauss_jordan <- function(A, B) {
  n <- ncol(A)
  AB <- cbind(A,B)
  for (p in 1:n) {
    if (p==1) {
      a <- abs(AB[, p])
    } else {
      a <- c(rep(0, p-1), abs(AB[p:n,p]))
    }
    pivot <- which (a == max(a))[1]
    temp <- AB[p, ]
    AB[p, ] <- AB[pivot, ]
    AB[pivot,] <- temp
    AB[p, ] <- AB[p, ] / AB[p, p]
    for (r in 1:n) {
      if(r!=p) {
        m <- AB[r, p]
        AB[r, ] <- AB[r, ] - m * AB[p, ]
      }
    }
  }
  AB
}

A <- scan()
2
4
-2
3
4
3
-1
-3
-1

dim(A) <- c(3, 3)

B <- scan()
5
3
1

gauss_jordan(A, B)
