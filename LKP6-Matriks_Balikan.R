# Metode Matriks Balikan
options(scipen = 999)
modifikasi_gauss <- function(A,B) {
  n <- ncol(A)
  AB <- cbind(A, B)
  for (p in 1:(n - 1)) {
    if (p == 1) {
      a <- abs(AB[, p])
    } else {
      a <- c(rep(0, p - 1), abs(AB[p:n, p]))
    }
    pivot <- which (a == max(a))
    temp <- AB[p, ]
    AB[p, ] <- AB[pivot, ]
    AB[pivot, ] <- temp
    for (r in (p + 1):n) {
      m <- AB[r, p] / AB[p, p]
      AB[r, ] <- AB[r, ] - m * AB[p, ]
    }
  }
  AB
}

backsub <- function (a) {
  n <- ncol(a)-1
  x <- rep(0,n) #replika 0 sebanyak n
  for (i in n:1) {
    if (i<n) {
      temp <- 0
      for (j in (i+1):n) {
        temp <- temp + a[i, j] * x[j]
      }
      x[i] <- (a[i, n + 1] - temp)/a[i, i]
    } else {
      x[i] <- a[i, n + 1] / a[i, n]
    }
  }
  return (x)
}

A <- scan()
1
3
1
-1
0
0
2
1
2

dim(A) <- c(3,3)

B <- scan()
5
10
5

modifikasi_gauss(A, B)
backsub(modifikasi_gauss(A, B))
