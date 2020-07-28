install.packages(c("nycflights13", "sqldf"))

library("sqldf")
library("nycflights13")

#7.1
N <- nrow(airports)
#a
airports[sample(N, 100, replace=F), ]
#b
airports[sample(N, floor(0.05*N), replace=F), ]
#c
head(airports, 100)
#d
tail(airports, 100)

#7.2
wqa <- read.csv("http://www.gagolewski.com/resources/data/winequality-all.csv",
    comment = '#')
dim(wqa)
wqa
white_wines <- wqa[wqa$color == "white", ]
white_wines
N1 <- nrow(white_wines)
p <-  sample(N1, 0.8*N1)
wines_train <- white_wines[p, ]
wines_train
#tak
x <- duplicated(rbind(white_wines, wines_train), fromLast = TRUE)
x <- white_wines[ !x[1:nrow(white_wines)], ]
x
#lub tak
wines_test <-  white_wines[-p, ]

#7.3
install.packages("fueleconomy")
library(fueleconomy)
v <- as.data.frame(vehicles)
v
v$cty <- 1/v$cty*3.785*100/1.6
v$hwy <- (100*3.7854/1.609344)/v$hwy
class_factor <- factor(v$class)
f <- unclass( class_factor )

cty_mean <- aggregate(v['cty'], v['class'], mean)
cty_sd <- aggregate(v['cty'], v['class'], sd)
hwy_mean <- aggregate(v['hwy'], v['class'], mean)
hwy_sd <- aggregate(v['hwy'], v['class'], sd)
v$z_cty <- (v$cty - cty_mean$cty[f])/cty_sd$cty[f]
v$z_hwy <- (v$hwy - hwy_mean$hwy[ f ])/hwy_sd$hwy[ f ]
head(v)

#7.4

rozwin <- function(x, name){
  stopifnot(is.matrix(x), !is.null(dimnames(x)), is.character(name), length(name) == 3)
  res <- data.frame( 
    as.double(x), 
    rep(colnames(x), each = nrow(x)), 
    rep(rownames(x), times = ncol(x)))
  colnames(res) <-  name
  res
}
name <-  c("ile", "gdzie", "kiedy")
a <-rozwin(WorldPhones, c("ile", "gdzie", "kiedy"))
a[order(a[2], a[3]), ]
#7.5

zwin <- function(x){
  name <- colnames(x)
  x <- x[order(x[2], x[3]), ]
  rnames <- unique(x[[2]])
  cnames <-  unique(x[[3]])
  res <- matrix(x[[1]], nrow = length(rnames), dimnames=list(cnames, rnames))
  res
  }
zwin(a)










