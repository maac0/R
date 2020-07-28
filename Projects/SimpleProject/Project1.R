
## ...---... Zadanie 01.01 ...---...

calka <- function(f, a, b, n = 100){
  stopifnot( a < b, n > 0, is.numeric(a), is.numeric(b), is.numeric(n), length(a) == 1, length(b) == 1, length(n) == 1, n == floor(n))
  h <- (b-a) / n
  yright <- f(a+cumsum(rep(h, n)))
  yleft <- f(a+cumsum(rep(h, n))-h)
  sum(h*((yleft+yright) / 2))
}

## ...---... Zadanie 01.02 ...---...

sklej <- function(x, sep = ""){
  stopifnot(is.list(x), lapply(x, is.character) == TRUE)
  do.call(paste, c(x, sep=sep))
}
## ...---... Zadanie 01.03 ...---...

repr_macierz <- function(x, eps = 1e-16){
  stopifnot(is.matrix(x), eps >= 0)
  y <- x[abs(x) >= eps]
  i <- which(abs(x) >= eps)
  j <- floor(i / (length(x[, 1]) +1)) +1
  i <- i %% (length(x[ , 1]))
  i[ i == 0] <- 5
  y <- matrix(c(i, j, y), ncol = 3, nrow = length(y))
  colnames(y) <- c("row", "col", "value") 
  y  
}

## ...---... Zadanie 01.04 ...---...

logiderle <- function(i, j, n){
  stopifnot(length(i) == length(j),  i == floor(i), j == floor(j), n == floor(n), 1 <= i, i <= j, i[2:length(i)] > j[1:(length(j)-1)], is.numeric(i), is.numeric(j), is.numeric(n), length(n) == 1)
  x <- 1:n
  y1 <- findInterval(x, i)
  y2 <- findInterval(x, j, left.open = TRUE)
  y1 != y2
} 
