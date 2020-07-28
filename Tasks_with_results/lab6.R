install.packages(c("ggplot2"))

library("sqldf")
library("nycflights13")

#dla_sportu
class(iris)
mode(iris)
df <- data.frame(
  x = rnorm(5),
  y = sample(c(TRUE, FALSE), 5, replace = TRUE),
  z = letters[1:5],
  row.names = seq(1, 5)
)
df
head(df, 2)
tail(df, 3)
class(df[["x"]])
df[c(1, 3, 5), "x", drop=FALSE]
df[df$x > 0 & df$y, 1:2]

planes
weather
head(planes, 5)

#6.2

#a
sqldf("SELECT DISTINCT engine FROM planes")
unique(planes["engine"])
#b
sqldf("SELECT DISTINCT type, manufacturer FROM planes")
unique(planes[c("type", "manufacturer")])
#c
sqldf("SELECT COUNT(*), engine FROM planes GROUP BY engine")
x <- aggregate(planes$engine, planes["engine"], length)
colnames(x) <- c("engine", "count")
x
#d
sqldf("SELECT COUNT(*), engine, type FROM planes GROUP BY engine, type")
aggregate(1:nrow(planes[1]), planes[c("engine", "type")], length)
#e
sqldf("SELECT MIN(year), AVG(year), MAX(year), engine, manufacturer FROM planes
      GROUP BY engine, manufacturer")
x2 <- aggregate(planes["year"], planes[c("engine", "manufacturer")], FUN = function(y){
  c(mean = mean(y), min = min(y), max = max(y))})
x2
#6.3
#a
sqldf("SELECT * FROM planes WHERE speed IS NOT NULL")
planes[ !is.na(planes$speed), ]
#b
sqldf("SELECT tailnum FROM planes WHERE seats BETWEEN 150 and 190 AND year >= 2012")
na.omit(planes[planes$seats >= 150 & planes$seats <= 190 &  planes$year >= 2012,"tailnum", drop=FALSE])
#c
sqldf("SELECT  * FROM planes WHERE manufacturer IN ('BOEING', 'AIRBUS', 'EMBRAER') AND seats > 390")
planes[planes$seats > 390 & planes$manufacturer %in% c("BOEING", "AIRBUS", "EMBRAER"), ]
#d
sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY year ASC, seats DESC")
x <- na.omit(unique(planes[planes$year >= 2012, c("year", "seats")]))
x
x <- x[order(x$seats, decreasing = TRUE), ]
x <- x[order(x$year), ]
x
#e
sqldf("SELECT DISTINCT year, seats FROM planes WHERE year >= 2012 ORDER BY seats DESC, year ASC")
x1 <- na.omit(unique(planes[planes$year >= 2012, c("year", "seats")]))
x1
x1 <- x[order(x$year), ]
x1 <- x[order(x$seats, decreasing = TRUE), ]
x1

# 6.4
#a
sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer")
x <- aggregate(planes$manufacturer[planes$seats > 200], planes[planes$seats > 200, c("manufacturer")], length)
colnames(x) <- c("manufacturer" ,"count") 
x
#b
sqldf("SELECT manufacturer, COUNT(*) FROM planes GROUP BY manufacturer HAVING COUNT(*) > 10")
x <- aggregate(planes$manufacturer, planes[, c("manufacturer")], length)
x[ x$x > 10, ]
#c
sqldf("SELECT manufacturer, COUNT(*) FROM planes WHERE seats > 200 GROUP BY manufacturer HAVING COUNT(*) > 10")
x <- aggregate(planes$manufacturer[ planes$seats > 200], planes[planes$seats > 200, c("manufacturer")], length)
x[ x$x > 10, ]
#d
sqldf("SELECT manufacturer, COUNT(*) AS howmany FROM planes GROUP BY manufacturer ORDER BY howmany DESC LIMIT 5")
x <- aggregate(planes$manufacturer,  planes[c("manufacturer")], length)
head(x[order(x$x, decreasing = TRUE), ],5)
#6.5
#a
sqldf("SELECT * FROM flights LEFT JOIN planes ON flights.tailnum = planes.tailnum")
head(merge(flights, planes, by="tailnum", all.x = TRUE, all.y = FALSE),2)
#b
cartail <- unique(flights[c("carrier", "tailnum")])
head(merge(merge(cartail, planes, by.x="tailnum", by.y="tailnum"), airlines, by.x= "carrier", by.y="carrier"), 2)
head(sqldf("SELECT planes.*, airlines.* FROM (SELECT DISTINCT carrier, tailnum FROM flights) AS cartail INNER JOIN planes ON cartail.tailnum=planes.tailnum INNER JOIN airlines ON cartail.carrier=airlines.carrier"), 2)
#c
flights2 <- flights[flights$origin == 'EWR', ]
weather2 <- weather[ , c("year", "month", "day", "temp", "humid", "pressure")]
weather2 <- aggregate(weather2[c("temp", "humid", "pressure")], by=weather[c("day", "month", "year")], FUN = mean, na.rm = TRUE)
x <- head(merge(x = flights2, y = weather2, by = c("year", "month", "day"), all.x = TRUE, all.y = FALSE), 3)
colnames(x)[colnames(x) %in% c('temp', 'humid', 'pressure')] <- c('atemp', 'ahumid', 'apressure')
x

#6.6
a <- head(airports, 10)
b <- tail(head(airports, 15), 10)
#a
unique(rbind(a, b))
#b
rbind(a, b)
#c
x <- rbind(a, b)
x[duplicated(x), ]
#d
x <- duplicated(rbind(a, b), fromLast = TRUE)
x <- a[ !x[1:nrow(a)], ]
x
#e
binded <- rbind(a,b) 
x <- duplicated(binded)
x <- b[ !x[(nrow(a)+1):length(x)], ]
x



