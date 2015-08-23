setwd("~/Workspace/R/Data Science Capstone/")

# loading connections file
sourceTxtBlog <- file("./final/en_US/en_US.blogs.txt")
sourceTxtNews <- file("./final/en_US/en_US.news.txt")
sourceTxtTwitter <- file("./final/en_US/en_US.twitter.txt")

# reading lines and load text in memory
linesBlog <- readLines (con = sourceTxtBlog)
linesNews <- readLines (con = sourceTxtNews)
linesTwitter <- readLines (con = sourceTxtTwitter)

close(sourceTxtBlog)
close(sourceTxtNews)
close(sourceTxtTwitter)

#Count number of words of Blogs file
#numWordsBlog <- sum (sapply(gregexpr("\\W+", linesBlog), length) + 1)
#Count number of lines of Blogs file
numLinesBlog <- length (linesBlog)
#Calculate average number of words per line of Blogs file
#avgWordsLinesBlog <- round (numWordsBlog / numLinesBlog)

#Count number of words of News file
#numWordsNews <- sum (sapply(gregexpr("\\W+", linesNews), length) + 1)
#Count number of lines of News file
numLinesNews <- length (linesNews)
#Calculate average number of words per line of News file
#avgWordsLinesNews <- round (numWordsNews / numLinesNews)

#Count number of words of Twitter file
#numWordsTwitter <- sum (sapply(gregexpr("\\W+", linesTwitter), length) + 1)
#Count number of lines of Twitter file
numLinesTwitter <- length (linesTwitter)
#Calculate average number of words per line of Twitter file
#avgWordsLinesTwitter <- round (numWordsTwitter / numLinesTwitter)

#Combine counts in a vector in order to be plotted
#counts <-matrix(c(numWordsBlog, numWordsNews, numWordsTwitter, numLinesBlog, numLinesNews, numLinesTwitter, avgWordsLinesBlog, avgWordsLinesNews, avgWordsLinesTwitter), nrow=3, ncol=3)
#rownames(counts)<- c("blog","news","twitter")
#colnames(counts)<- c("Number of words","Number of lines", "Average number of words per line")

#Plots number of words
#barplot(counts[,1], main=colnames(counts)[1], xlab="data", col=c("blue","green", "red"), log="y")
#Plots number of lines
#barplot(counts[,2], main=colnames(counts)[2], xlab="data", col=c("blue","green", "red"), log="y")
#Plots Average number of words per line
#barplot(counts[,3], main=colnames(counts)[3], xlab="data", col=c("blue","green", "red"), log="y")

#sampling text
sampleBlog <- sample(linesBlog, numLinesBlog * 0.1)
sampleNews <- sample(linesNews, numLinesNews * 0.1)
sampleTwitter <- sample(linesTwitter, numLinesTwitter * 0.1)

vectorEnglishText <- c(sampleBlog, sampleNews, sampleTwitter)

rm(linesBlog)
rm(linesNews)
rm(linesTwitter)

#Cleaning no ASCII characters 

# convert string to vector of words
splittedSample<- unlist(strsplit(vectorEnglishText, split=", "))

# find indices of words with non-ASCII characters
nonAscIDX<- grep("splittedSample", iconv(splittedSample, "latin1", "ASCII", sub="splittedSample"))

# subset original vector of words to exclude words with non-ACCII characters
asciiVector<- splittedSample[ - nonAscIDX]

# convert vector back to string
preCleanedTextVector<- paste(asciiVector, collapse = ", ")

library("tm")
english <- Corpus(VectorSource(preCleanedTextVector))

rm(nonAscIDX)
rm(sampleBlog)
rm(sampleNews)
rm(sampleTwitter)

# to lower all letters
english <- tm_map(english, tolower)
english <- tm_map(english, PlainTextDocument)
# removing URLs
urlPat<-function(x) gsub("(ftp|http)s?:(\\/)+[\\d\\w.]*\\b", " ", x)
english<-tm_map(english, urlPat)
# removing Emails
emailRgx <- "[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"
emlPat<-function(x) gsub(emailRgx, " ", x)
english<- tm_map(english, emlPat)
# removing Twitter tags
tt<-function(x) gsub("(RT )|via", " ", x)
english <- tm_map(english, tt)
# removing numbers
english <- tm_map(english, removeNumbers)
# removing punctuation characters
english <- tm_map(english, removePunctuation)
# stripping whites and spaces
english <- tm_map(english, stripWhitespace)
english <- tm_map(english, PlainTextDocument)

english <- tm_map(english, removeWords, stopwords("english"))
swearWords <- c("arse",
                "ass",
                "asshole",
                "bastard",
                "bitch",
                "bloody",
                "bollocks",
                "cunt",
                "damn",
                "fuck",
                "goddamn",
                "godsdamn",
                "hell",
                "holy shit",
                "masturbating",
                "motherfucker",
                "shit",
                "shitass")
english <- tm_map(english, removeWords, swearWords)
rm(vectorEnglishText)
rm(asciiVector)
rm(emailRgx)
rm(swearWords)
rm(preCleanedTextVector)

#1-gram analysis
tdm1Gram <- TermDocumentMatrix(english)
#inspect(tdm1Gram[1:10,1])
#Min frequency term
#min((tdm1Gram[,1])[,1])
#Max frequency term
#max((tdm1Gram[,1])[,1])
#Shows terms with a frequency of at least 10
#findFreqTerms(tdm1Gram, lowfreq =  2000)
#dim(tdm1Gram)

#size of corpus
V <- dim(tdm1Gram)[1]

m1 <- as.matrix(tdm1Gram)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(freq=v1)
rm(m1)
rm(v1)
rm(tdm1Gram)

# Weka library is required in order to use tokenizer function
options(java.parameters = "-Xmx4g")
options(java.parameters = "-XX:+UseConcMarkSweepGC")
options(java.parameters = "-XX:-UseGCOverheadLimit")
library(RWeka)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

#2-gram analysis
tdm2Gram <- TermDocumentMatrix(english, control = list(tokenize = BigramTokenizer))
#inspect(tdm2Gram[1:10,1])
#min((tdm2Gram[,1])[,1])
#max((tdm2Gram[,1])[,1])
#dim(tdm2Gram)
#findFreqTerms(tdm2Gram, 200, 800)

#plot(tdm2Gram, terms = findFreqTerms(tdm2Gram, lowfreq = 500), corThreshold = 0.5)

m2 <- as.matrix(tdm2Gram)
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(freq=v2)
words <- sapply(rownames(d2), function(x) (strsplit(as.character(x), " ", fixed=TRUE)))
d2$w1 <- sapply(words,function(x) x[1])
rm(m2)
rm(v2)
rm(tdm2Gram)

#3-gram analysis
tdm3Gram <- TermDocumentMatrix(english, control = list(tokenize = TrigramTokenizer))
#inspect(tdm3Gram[1:10,1])
#min((tdm3Gram[,1])[,1])
#max((tdm3Gram[,1])[,1])
#dim(tdm3Gram)

#plot(tdm3Gram, terms = findFreqTerms(tdm3Gram, lowfreq = 2), corThreshold = 0.5)

m3 <- as.matrix(tdm3Gram)
v3 <- sort(rowSums(m3),decreasing=TRUE)
d3 <- data.frame(freq=v3)
words <- sapply(rownames(d3), function(x) (strsplit(as.character(x), " ", fixed=TRUE)))
d3$w1w2 <- sapply(words,function(x) paste(x[1],x[2]))
rm(m3)
rm(v3)
rm(tdm3Gram)

#smoothing Good-Turing Nc vector of counts of frequencies
N1 <- aggregate(rownames(d1), by=d1["freq"], FUN=length); colnames(N1) <- c("r", "Nr")
N2 <- aggregate(rownames(d2), by=d2["freq"], FUN=length); colnames(N2) <- c("r", "Nr")
N3 <- aggregate(rownames(d3), by=d3["freq"], FUN=length); colnames(N3) <- c("r", "Nr")

#function for getting linear regression on Good-Turing methods
getLinearGT <- function(N, col){
  log_r <- log(N$r)
  log_Zr <- log(N[,col])
  return(lm(log_Zr ~ log_r, data = N1)) 
}

#smoothing Nr function
smoothNr <- function(N) {
  rows <- dim(N)[1]
  Zr <- rep(0, rows)
  for(r in 1:rows){
    if(N[r,"r"] == 1) 
      q <- 0
    else
      q <- N[r - 1,"r"]
    
    if(r == rows)
      t <- 2 * N[r,"r"] - q
    else
      t <- N[r + 1,"r"]
    
    Zr[r] <- 2 * N[r,"Nr"] / (t - q)
  }
  return(Zr)
}

N1$Zr <- smoothNr(N1)
N2$Zr <- smoothNr(N2)
N3$Zr <- smoothNr(N3)

#Linear regressions for good-turing models
fitGT1 <- getLinearGT(N1, "Nr");#summary(fitGT1)
fitGT2 <- getLinearGT(N2, "Nr");#summary(fitGT2)
fitGT3 <- getLinearGT(N3, "Nr");#summary(fitGT3)

fitSGT1 <- getLinearGT(N1, "Zr");#summary(fitSGT1)
fitSGT2 <- getLinearGT(N2, "Zr");#summary(fitSGT2)
fitSGT3 <- getLinearGT(N3, "Zr");#summary(fitSGT3)

#Good-Turing counts function
countGT <- function(x,N,fit) {
  y<-x+1
  numerator <- N[N$r == y, "Nr"]
  if(identical(numerator, integer(0))){
    numerator <- exp(predict(fit, newdata = data.frame(log_r = log(y))))
  }
  return(y*(numerator/N[N$r == x, "Nr"]))
}

d1$freqGT <- sapply(d1$freq, countGT, N = N1, fit = fitGT1)
d2$freqGT <- sapply(d2$freq, countGT, N = N2, fit = fitGT2)
d3$freqGT <- sapply(d3$freq, countGT, N = N3, fit = fitGT3)

Nt <- sum(N1$r*N1$Nr)
pGT0 <- N1[1,"Nr"] / Nt

#Simple Good-Turing counts function
countSGT <- function(x,fit) {
  y<-x+1
  return(y*(exp(predict(fit, newdata = data.frame(log_r = log(y)))))/(Nt*exp((predict(fit, newdata = data.frame(log_r = log(x)))))))
}

d1$freqSGT <- sapply(d1$freq, countSGT, fit = fitSGT1)
d2$freqSGT <- sapply(d2$freq, countSGT, fit = fitSGT2)
d3$freqSGT <- sapply(d3$freq, countSGT, fit = fitSGT3)

write.csv(d1, "d1.csv"); write.csv(d2, "d2.csv"); write.csv(d3, "d3.csv")