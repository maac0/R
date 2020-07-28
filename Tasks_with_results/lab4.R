x <- round(rnorm(10, 3, 2), 0)
x
rle(x)$values
#4.1
moda <- function(x){
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}
?rle
x
moda(x)
#4.2
funkcja1 <- function(x){
  2 * x + 1
}
calkaMonteCarlo <- function(funkcja, a, b, n = 1000){
  fmin <- min(funkcja(a), funkcja(b))
  fmax <- max(funkcja(a), funkcja(b))
  x1 <- runif(n, a, b)
  x2 <- runif(n, fmin, fmax)
  suma <- length(x1[ funkcja(x1) >=  x2])
  (suma / n) * (b-a)*(fmax-fmin) +  (b-a)*fmin
}
calkaMonteCarlo(funkcja1, 1, 3)

#4.3
sample2 <- function(x, k){
  x[round(runif(k, 1, length(x)), 0)]
}
x <- c(1, 2)
x <- sample2(x, 5)

#4.4
unique2 <- function(x){
  y <- sort(x)
  which(tabulate(y) > 0)
}
x
unique2(x)

#4.5
tabulate(x)
x <- c(7, 1, 2, 2 ,3, 4, 1, 6, 7, 9)
y <- c(-1, 2, 4, 1, 2, 5, 2, 3, 7, 8)
intersect2 <- function(x, y){
  x1 <- tabulate(sort(x))
  y1 <- tabulate(sort(y))
  ifelse(length(x1) > length(y1), y1[(length(y1)+1):length(x1)] <- 0, 
         ifelse(length(x1) == length(y1), "",x1[(length(x1)+1):length(y1)] <- 0))
  x1
  y1
  which(y1 > 0 & x1 > 0)
} 
intersect2(x, y)

#4.6
findInterval(c(1, 2, 3, 4, 5, 6), c(1.5, 4.5))
rle(sort(c(1, 1, 5, 1, 3, 2, 3, 4)))

funkcja2 <- function(x, y){
  x_unique <- sort(unique(x))
  findInterval(y, x_unique)/length(x)
  
}
funkcja2(x, y)

#4.7
x<- c("a", "b", "c")
merge_string_list <- function(x){
  paste(x, sep =" ", collapse = " ")
}
merge_string_list(x)

#4.8
n <- 2
m <- 3
nm <- c(1, 2, 3, 2, 1, 5)
nm_matrix
nm_matrix[1:n ,1:m]



posortowanePunkty <- function(n, m, nm){
  nm_matrix <- matrix(nm, n,  m, byrow = TRUE)
  sqrt(colSums(nm_matrix[1:n ,1:m] ^ 2))
}
posortowanePunkty(2, 2, c(1,2,3,4))

#4.9

f <- function(x){
  2*x + 3
}
y <- c(1, 2, 3 , 4)
a <- 6
b <- 7
?approxfun
approxinvert <- function(f, y, a ,b, k = 100){
  approxfun(y, y = NULL, xout, method = "linear", n = k, a, b)
}
approxinvert(f, y, a, b)

#4.10
x <- list(c(1, 2, 2), c(4, 2), c(1, 3))

wystarczy <- function(x, r, R, funcja){
  y <- simplify2array(x)
  y <- lapply(y, funcja)
  y[which(y > r & R > y)]
}
wystarczy(x, 1, 5, sum)

#4.11
a <- c(1, 2, 3)
p <- c(4, 2, 1)
distance <- function(p, a, b){
  abs(sum(apply(matrix(c(p, a), nrow = length(p)), 1, prod))+b)/sqrt(sum(apply(matrix(c(a, a), nrow = length(a)), 1, prod)))
}
distance(c(2, -2, 0.5), c(2, 1, 4), b = -4)

#4.12
n <- 50
x <- c(3, 4, 6, 1, 5, 2)
p <- c(0.1, 0.2, 0.3, 0.1, 0.14, 0.16)
gendyskr <- function(n, x, p){
  ?replicate
  stopifnot(n > 0, length(x) == length(p), sum(p) == 1, length(unique(x)) == length(x))
  if(sum(p) != 1) p <- p / sum(p)
  u <- runif(n, 0, 1)
  u
  x[findInterval(u, cumsum(p))+1]
}
gendyskr(n, x, p)








