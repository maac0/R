
#5.1
t <- c(1, 4, 5, 2, 6, 9 ,1 ,2 ,4, 6, 0, 5, 3, 1, 4, 6, 7, 8, 4, 3, 1, 3)
one_hot_encode <- function(t){
  k <- max(t) 
  n <- length(t)
  y <- matrix(0, nrow = n, ncol = k, TRUE)
  y[cbind(1:n, t)] <- 1
  y
}

#5.2
x <- matrix(t, nrow = n, ncol = k, TRUE)
softmax <- function(m){
  k <- length(x[1, ])
  n <- length(x[, 1])
  x < - exp(x)
  x <- x / rowSums(x)
  x
  x <- abs(x - 1)
  x <- apply(x, 1, function(x){which.min(x)})
  one_hot_encode(x)
}
softmax(x)

#5.3
x <- matrix(t, 5, 6, TRUE)
x
y <- c(1,3,2,4,5,2)
y
przedzialw <- function(x){
  n <- length(x[1, ])
  d <- length(x[, 1])
  y <- matrix(c(apply(x, 1, min), apply(x, 1, max)) , nrow = 2, ncol = d, byrow=TRUE)
  y
}
przedzialw(x)

#5.4

odleglosc <- function(x, y){
  d <- length(x[1, ])
  n <- length(x[, 1])
  z<- matrix(y, nrow = n, ncol = d, byrow = TRUE)
  z
  z <- abs(x - y) ^ 2
  z <- rowSums(z)
  z <- sqrt(z)
  z
}
odleglosc(x, y)

#5.5
x <- matrix(c(0.1, 0.4, 0.15, 0.15, 0.1, 0.1), 2, 3, TRUE)
d <- length(x[1, ])
n <- length(x[, 1])
yn <- c(1, 2)
yd <- c(1, 2, 3)
x
niezaleznosc <- function(x){
  j <- rowSums(x)
  i <- colSums(x)
  z <- x - outer(j ,i, "*")
  z <- z != 0
  sum(z) == length(z)
}
niezaleznosc(x)
x
podststat <- function(yn, yd, x){
  x_j <- colSums(x)
  yd2 <- (yd) ^ 2
  yn2 <- (yn) ^ 2
  wartosc_oczekiwanaX <- sum(colSums(x) * yd)
  wartosc_oczekiwanaY <- sum(rowSums(x) * yn)
  wariancjaX <- sum((colSums(x) * yd2)) - wartosc_oczekiwanaX
  wariancjaY <- sum((rowSums(x) * yn2)) - wartosc_oczekiwanaY
  kowariancja <- sum(outer(yn, yd, "*") * x) - (wartosc_oczekiwanaX * wartosc_oczekiwanaY)
  wspolczynni_kkorelacji <-  kowariancja / sqrt(wariancjaX * wariancjaY)
  c("Ex"= wartosc_oczekiwanaX, "Ey"= wartosc_oczekiwanaY, "VarX" = wariancjaX, "VarY" = wariancjaY, "CovXY" = kowariancja, "rhoXY" = wspolczynni_kkorelacji)
}
podststat(yn, yd, x)
#5.6







