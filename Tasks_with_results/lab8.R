options(repr.plot.width = 7)
options(repr.plot.height = 5)
options(jupyter.rich_display = FALSE)
options(width = 120)

#8.1
mypie <- function(x){
  stopifnot(is.numeric(x) | is.factor(x))
  stopifnot(length(x) > 0)
  
  plot.new()
  plot.window(c(-1.5, 1.5), c(-1.5, 1.5))
  x_1 <- c(0, 2*pi*cumsum(x) / sum(x))
  colors <- rainbow(length(x))
  
  for(i in 1:length(x)){
    t <- seq(x_1[i], x_1[i+1], length.out = 200)
    x_plot <- sin(t)
    y_plot <- cos(t)
    polygon(c(0, x_plot, 0), c(0, y_plot, 0), col = colors[i])
    text(sin(t[100]) * 1.2, cos(t[100]) *1.2, names(x)[i])
    
  }
  
  
}

#8.2
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 50)
myboxplot <- function(x){  
  stopifnot(is.numeric(x))
  plot.new()
  max1 <- max(abs(min(x)), max(x))
  plot.window(c(-10, 10), c(-2 * max1, 2 * max1))
  med <- median(x)
  quant1 <- quantile(x)[2]
  quant3 <- quantile(x)[4]
  iqr <- quant3 - quant1
  rect(-8,quant1 , 8,  quant3, col="black")
  rect(-8, med-med/100, 8, med+med/100, col="red")
  lines(c(0, 0), c(quant3, quant3+1.5*iqr), col="black")
  lines(c(0, 0), c(quant1, quant1-1.5*iqr), col="black")
  lines(c(-5, 5), c(quant1-1.5*iqr, quant1-1.5*iqr), col="black")
  lines(c(-5, 5), c(quant3+1.5*iqr, quant3+1.5*iqr), col="black")
  x_q <- x[x > quant3 + 1.5*iqr | x < quant1 - 1.5*iqr]
  points(rep(0, n=length(x_q)), x_q, pch = 20)
  par(bg ="grey")

  par(mar = c(2, 2, 2, 2))
  ?axis
  axis(1)
  axis(2, las = 1)
}
myboxplot(x)
boxplot(x)

#8.3
?plotmath
sin_plot <- function(){
  x <- seq(0, 2*pi, length.out=1000)
  plot.new()
  plot.window(c(0, 2*pi), c(-1, 1))
  points(x, sin(x), pch = 20)
  lines(x, sin(x), lty = 2, col = rgb(1, 0, 0), lwd = 2)
  ?lines
  lines(x, cos(x), lty = 4, col = rgb(0, 0, 1), lwd = 2)
  title(main = "Sinus i cosinus")
  box()
  axis(1, at = seq(0, 2*pi, 0.5*pi), label = expression(0, frac(pi, 2), pi, frac(3, 2)*pi, 2*pi), mgp=c(3,2,0))
  axis(2, at= c(-1, 0, 1), las = 1)
   
  rect(par("usr")[1], par("usr")[3], pi/1.5, -0.55)
  segments(c(0, 0), c(-0.9, -0.7), c(pi/4, pi/4), c(-0.9, -0.7), col = c("red", "blue"), lty = c(2, 4), lwd = c(2, 2))
  text(c(pi/4, pi/4), c(-0.9, -0.7), c("sin", "cos"), pos = 4)
}


sin_plot()
multiboxplot1 <- function(x){
  stopifnot(is.list(x))
  max1 <- max(as.numeric(lapply(x, max), max), as.numeric(lapply(abs(as.numeric(lapply(x, min))), max)))
  n <- length(x)
  nseq <- seq(0, 10*(n-1), length.out = n)
  
  med <- as.numeric(lapply(x, median))
  quant1 <- as.numeric(lapply(x, FUN = function(x){quantile(x)[[2]]}))
  quant3 <- as.numeric(lapply(x, FUN = function(x){quantile(x)[[4]]}))
  iqr <- quant3 - quant1

  plot.new()
  plot.window(c(0, 10*n), c(-2 * max1, 2 * max1))
  rect(nseq+1, quant1 , 9+nseq,  quant3, col="black")
  rect(nseq+1, med-med/100, 9+nseq, med+med/100, col="red")
  nseq <- c(nseq, 10*n)
  segments(nseq-5, quant3, nseq-5,  quant3+1.5*iqr, col="black")
  segments(nseq-5, quant1, nseq-5,  quant1-1.5*iqr, col="black")
  segments(nseq-7.5, quant1-1.5*iqr ,nseq-2.5, quant1-1.5*iqr, col="black")
  segments(nseq-7.5, quant3+1.5*iqr ,nseq-2.5, quant3+1.5*iqr, col="black")
  for(i in 1:length(x)){
    tmp <- x[[i]]
    y <- tmp[tmp > quant3[[i]] + 1.5*iqr[[i]] | tmp < quant1[[i]] - 1.5*iqr[[i]]]
    points(rep(i*10 - 5, length(y)) ,y, pch = 20)
}
  par(bg ="grey")
  axis(1)
  axis(2, lwd = 1)
  }

multiboxplot1(x)
x <- list(c(1, 2, 3, 4), c(3, 2, 1, 3, 8), c(1, 2, 3, 4, 2))
y <- x[[1]]
y[ y > 2]

t <- seq(0, 2*pi, length.out = 100)
x1 <- cos(t)
y1 <- sin(t)
plot.new()
plot.window(c(-1, 1), c(-1, 1), asp = TRUE)
polygon(x1, y1, col = "blue")
text(-1, 0, "(-1, 0)", pos = 4)
axis(3)
axis(4)
axis(2, las = 1)

mypie(dzieci)
plot.new()
plot.window(c(0, 10), c(0, 10))
x <- 
polygon(c(), c())
text(pi/2, 0.5, "XD")

polygon(c(1, 2, 2, 1), c(2, 2, 4, 4), col="red")
plot(dzieci)

dzieci <- c(krasnale = 40, zuchy = 69, wesolki = 32)
pie(dzieci)
