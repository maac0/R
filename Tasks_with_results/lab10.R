path <- file.path("~", "Desktop", "chess.py")
dirname(path)
normalizePath(".")
getwd()
normalizePath("..")
file.info("~/Desktop/chess.py")
file.size("~/Desktop/chess.py")
file.mtime("")
file.create("xdddd.txt")
cat("Dopisanie\n", file = "xdddd.txt", append = TRUE)
readLines("xdddd.txt")
list.dirs("Desktop", recursive = FALSE)
list.files("~",  recursive = TRUE)
x <- list(a = 1:25000, b=letters, c=LETTERS)
file.create("lista_r.rds")
plik <- file.path(normalizePath("."), "lista_r.rds")
saveRDS(x, file = plik)
as.numeric(object.size(x))/1e6
file.size(plik)/1e6
y <- readRDS(plik)
y
iris
path <- file.path(".", "dane")
read.table(file.path(path, "iris.txt"))
df <- data.frame(A = rnorm(10), B = letters[1:10])
nazwa <- tempfile()
write.table(df, nazwa)
write.csv(df, nazwa)
write.csv2(df, nazwa)
path <-
open(nazwa, open="r")
readLines(nazwa, n=4)

#1
library(RCurl)
library(XML)
url <- "http://cran.rstudio.com/src/base/R-3/"
webpage <- getURL(url)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
name <- stringi::stri_extract_first_regex(webpage, "(?<=\">)(.*)(?=</a)")
name <- name[!is.na(name)]
name <- name[-1]
name
webpage
date <- stringi::stri_extract_first_regex(webpage, "(?<=</a>)(.*)")
date <- stringi::stri_trim(date)
date <- stringi::stri_replace_all_regex(date, "\\s*\\w*$", "")
date <- date[!is.na(date)]
date <- date[-1]
date

size <- stringi::stri_extract_first_regex(webpage, "(?<=\\s)([0-9]+)(?=M)")
size <- size[!is.na(size)]
size
df <- data.frame(url = rep(url, length(name)), nazwa = name, data = date, rozmiar = size)
df

#2
rozmiar <- function(path){
  stopifnot(dir.exists(path))
  x <- list.files(path, full.names = TRUE, recursive = TRUE)
  sum(file.size(x))/10^9
}
rozmiar("Desktop")

#4

oldest <- function(path, k = 10){
  stopifnot(dir.exists(path))
  x <- list.files(path, full.names = TRUE, recursive = TRUE)
  df <- file.info(x)
  o <- order(df$mtime)
  re <- x[o[1:min(k, length(x))]]
  re[order(df$size[o[1:(min(k, length(x)))]])]
}
oldest("Desktop/programs")

#5
seriale <- function(dir){
  dir <- normalizePath(dir)
  stopifnot(dir.exists(dir))
  files <- list.files(dir, full.names = TRUE)
  seasons <- stringi::stri_extract_all_regex(files, "(?<=[sS](eason)?(s)?)([0-9]+)(?=.*)")
  episodes <- stringi::stri_extract_all_regex(files, "(?<=[eE](pisode)?(s)?)([0-9]+)(?=.*)")
  ext <- stringi::stri_extract_all_regex(files, "(?<=\\.)(.*)$")
  correct_files <- !is.na(seasons) && !is.na(episodes) && !is.na(ext)
  names <- sprintf("S%02dE%02d.%s", as.integer(seasons[correct_files]), as.integer(episodes[correct_files]), ext[correct_files])
  file.rename(files[correct_files], file.path(dir, names))
  return(invisible((NULL)))
  }
seriale("Desktop/bolet")

#9

require(XML)
script.dir <- getwd()
xmlfile <- file.path(script.dir, "data.xml")
lista <- XML::xmlToList(XML::xmlParse( xmlfile))
res <- lista[["DataSet"]][["Series"]]
df <- data.frame(do.call("rbind", res[names(res) == "Obs"]), stringsAsFactors = FALSE)
df
#example1
doc <- XML::xmlParse(file.path(script.dir, "example_1.xml"))
doc
lista <- XML::xmlToList(doc)
lista
data.frame(do.call("rbind", lista), stringsAsFactors = TRUE)
XML::xmlToDataFrame(doc)

#example2
doc_2 <- XML::xmlParse(file.path(script.dir, "example_2.xml"))
doc_2
lista_2 <- XML::xmlToList(doc_2)
str(lista_2)
do.call("rbind", lista_2)
x <- XML::xmlChildren(XML::xmlRoot(doc_2))
t(sapply(x, XML::xmlAttrs))









