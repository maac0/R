library(stringi)
library()
install.library(nasaweather)
napisy <- c("Kaczka", "kot", "pies", NA, "kura", "")
tekst_kodowany <- c("\xb1b\xe6 chrz\xb1szcz brzmi w trzcinie \xe6ma grzeg\xbf\xf3\xb3ka ")
tekst_kodowany
znaki <- character(127)
for(i in 1:127){
  znaki[i] <-  sprintf("%3i : %s", i, rawToChar(as.raw(i)))
}
znaki
stri_enc_list()
stri_length(napisy)
text <- readLines("kodowanie_latin.txt")
stri_enc_detect(tekst_kodowany)
length(napisy)
stri_conv(tekst_kodowany, 'latin2', "UTF-8") #
stri_cmp("hortensja", "chaber") 
stri_locale_info() 
data <- Sys.Date()
data
strftime(data, '%d %B, %Y')
Sys.getlocale()
sort(c('hortensja', 'chaber'))
stri_sort(c('hortensja', 'chaber'), locale = "sk_SK") #
paste("a", "b", "c")
stri_paste(letters[1:3], "!") 
paste(letters[1:3], "!") 
rep(c("a", "b", "c"), 2)
stri_dup(c("a", "b", "c"), 2)
stri_dup(c("a", "b", "c"), c(2, 4, 8)) 
print("napis\nnapis")
cat("napis\nnapis")
format(pi, digits = 6, width = 4)
format(1000, scientific = TRUE)
cat(10**(-1:2), sep="\n")

sprintf("Stala pi = %.4f", pi)
sprintf("Stala pi = %.5f", pi)
sprintf("Stala pi = %.5e", pi)
sprintf("Stala pi = %+05i", 1)
sprintf("Stala pi = %5s", "xd")

N = 10
for(i in 1:N){
  Sys.sleep(0.1)
  cat(sprintf("\r%5.f%%", 10*i/N))
}
stri_trim("    asdasfsgdfgsdfgdsfgsrhstrhtsrsdtrhdtrhdtrhtsrgstrgdffffffffffffffffffffffffffffffffffffffffffffffffgtsrhdtrhtsrhstrhstr")
stri_trans_toupper("ABCD efg 12345 lmnoP !.)")
stri_trans_totitle("najważniejsza publikacja na swiecie")
stri_trans_char("Ćma śniła o świetle, które jasno świeci.",
                "ćśłó", "cslo")
stri_trans_general("Ćma śniła o świetle, które jasno świeci.", "Latin-ASCII")
stri_trans_general('napis', "latin-cyrillic")
napis <- "kot pies krowa"
stri_sub(napis, 5) 
stri_sub(napis, 5, 8) <- "kurasdas"
napis
stri_count_fixed("ababaabbab cdef ghij bbab", "ab", overlap = FALSE)
stri_locate_first_fixed("ababaabbab cdef ghij bbab", "ab")

#1
kombinuj <- function(a, b){
  let <- letters[1:b]
  num <- rep(1:b, each=b)
  do.call(paste, list(let, num, sep="", collaps=""))
}
kombinuj(3, 2)
#2
xd <- nycflights13::weather[c("year", "month", "day")]
for(i in 1:nrow(xd)){
  a <- 
  a <- c(a, sprintf("%s-%s-%s", xd[i, "year"], xd[i, "month"], xd[i, "day"]))
}
as.character(a)

#4
daty <- c("2012-02-01", "2015-12-21", "1995-02-21", "2004-12-01")
frame <- data.frame(
  rok = as.numeric(stri_sub(daty, 1 ,4)),
  miesiac = as.numeric(stri_sub(daty, 6 ,7)),
  dzien = as.numeric(stri_sub(daty, 9 ,10))
)
y <- as.numeric(stri_sub(Sys.Date(), 1 ,4))
m <- as.numeric(stri_sub(Sys.Date(), 6 ,7))
d <- as.numeric(stri_sub(Sys.Date(), 9 ,10))
y <- as.numeric(y - frame[, "rok"])
m <- as.numeric(m - frame[, "miesiac"])
d <- as.numeric(d - frame[, "dzien"])
cbind(frame, pelnoletnosc = ifelse(y > 18, TRUE, ifelse(y == 18 & m > 0 , TRUE, ifelse((y == 18 &m  == 0 & d >= 0), TRUE, FALSE))))

#5
napis <- "casfadaer      .[[[[[]]]]] 12312412412454521  aefaeefesf dasidnasiodasd  idasjdiasd dasdifn 121214sd  asfasfdsfdsfr"
stri_extract_all_regex(napis, "\\[.")
stri_count_regex(napis, "....")
stri_extract_all_regex("\\begin{equation} x^2 = y - 5 \\end{equation}", "\\\\")
stri_extract_all_regex(napis, "[13]{2}")
stri_replace_all_regex(napis, " +"," ")

#7
good <- c( 
  '0',
  '123',
  '-0',
  '+10',
  '4.',
  '.2',
  '12.123',
  '1e04',
  '-1e04',
  '+1e-04',
  '-12.423e+10',
  '4.e3',
  '.2e-1'
)
bad <- c( 
  'abc',
  '.',
  '1.2.2',
  '-',
  '+',
  'e',
  'e-4e',
  '10-10',
  '2.3e'
)
stri_extract_first_regex(good, '^[+-]?(\\.\\d+|\\d+\\.?\\d*)(e[+-]?\\d+)?$')
stri_extract_first_regex(bad,  '^[+-]?(\\.\\d+|\\d+\\.?\\d*)(e[+-]?\\d+)?$')

#11
tekst <- c("Accumsan zbigniew@yahoo.com in lectus foo@bar.com.pl nunc sollicitudinmauris
elementum morbi praesent. In natoque odio. Gravida eleifend arcu. Sagittis sed
laboriosamkasia88@onet.pl turpis non porta est est condimentum.Viverra
marcel@onet.pl magna praesent @cursus tempor czesiek@onet.pldonec.
marcel456@yahoo.com Justo Zenon.Burak@yahoo.com quam hac iaculis maecenas nisl
quasi in penatibus.",
           "Tellus laoreet vel. Orci nulla id volutpat sed praesent
foo@onet.com purus sed id.Duis amet nulla nonummy mus tellus. Id pellentesque
suspendisse. marcel6@yahoo.com Consequat nibh ja@my-mail.pl turpis. Ultrices
feugiat stefan23.burak@gmail.com luctus lectus nibh arcu metus mi
praesent.",
           "Ultricies urna vivamus. Scelerisque purus integer. Aen@ean nam
habitant est tincidunt blandit diam nec phasellussed euismod arcu a@@yahoo.com
nam pellentesque lorem etiam id magna. Donec neque habitasse. Mauris nunc
massa.Lacus gravida condimentum nam mollis vitae. @@ Feugiat metus sed.
Pharetra scelerisque suscipit.Est stefan45burak@yahoo.com venenatis volutpat
tomek123@onet.pl laoreet rutrum porttitor.Pede et et nec felis venenatis platea
suspendisse sed nunc nam amet julia12.k@yahoo.com.")

findemails <- function(napis){
  stri_extract_all_regex(napis, '\\w+(\\.\\w+)*@[\\w-]+(\\.\\w+)+')
}
findemails(tekst)

#12
good <- c(
  'http://www.wikipedia.com/',
  'http://www.wikipedia.com',
  'http://WIKIPEDIA.COM',
  'http://www.wikipedia.com/wiki/URL',
  'http://www.wikipedia.com/wiki',
  'ftp://213.135.44.35',
  'http://www.wikipedia.com/wiki/',
  'ftp://213.135.44.35////',
  'http://wikipedia/wiki/url',
  'http://www.wikipedia.com/wiki/',
  'http://www.wikipedia.com/wiki/',
  'http://www.wiki-pedia.com/wiki-pedia/'
)
bad <- c( 
  'ala ma kota',
  'ftp:/wikipedia.com',
  'http://www.wikipedia..com'
)
decomposeurls <- function(napis){
  stri_match_first_regex(napis,'^([\\w-]+)://((?:[\\w-]+)(?:\\.[\\w-]+)*)/?([\\w-/]*)$')
  
}
decomposeurls(good)
decomposeurls(bad)

