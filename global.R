#global functions

d1 <- read.csv("d1.csv")
rownames(d1) <- d1$X; d1$X <- NULL
d2 <- read.csv("d2.csv")
rownames(d2) <- d2$X; d2$X <- NULL
d3 <- read.csv("d3.csv")
rownames(d3) <- d3$X; d3$X <- NULL

#size of corpus
V <- dim(d1)[1]

pUnigram <- function(w) {
  return(d1[w,"freq"] / V)
}

#getting counts of words, freq column
getCounts <- function(W, freqCol){
  if(length(W) == 3){
    C <- c(d3[paste(W[1],W[2],W[3]),freqCol], d2[paste(W[1],W[2]),freqCol])
  }else if(length(W) == 2){
    C <- c(d2[paste(W[1],W[2]),freqCol],d1[W[1],freqCol])
  }else if(length(W) == 1){
    C <- c(d1[W[1],freqCol])
  }
  return(C)
}

#Maximum-likelihood model
pMaximumLikelihood <- function(W){
  C <- getCounts(W, "freq")
  #getting counts of words, freq column
  if(length(C) == 2){
    p <- C[1]/C[2]
  }else if(length(C) == 1){
    p <- pUnigram(W[1])
  }
  return(p)
}

#add k smoothing - m =k*V
#Laplace smoothing - add-1 smoothing, when k = 1
pAddK <- function(C, k){
  C <- getCounts(W, "freq")
  m <- k*V
  if(length(C) == 2){
    p <- (C[1] + m*(1/V))/(C[2] + m)
  }else if(length(C) == 1){
    p <- pUnigram(W[1])
  }
  return(p)
}

#Unigram prior smoothing
pUnigramPrior <- function(W, k){
  C <- getCounts(W, "freq")
  m <- k*V
  if(length(C) == 2){
    p <- (C[1] + m*pUnigram(W[1]))/(C[2] + m)
  }else if(length(C) == 1){
    p <- pUnigram(W[1])
  }
  return(p)
}

#smoothing Good-Turing
pSimpleGoodTuring <- function(W){
  CSGT <- getCounts(W, "freqSGT")
  CGT <- getCounts(W, "freqGT")
  if(is.na(CGT[1])){
    p <- pGT0
  } else {
    if(length(CGT) == 2){
      if(CGT[1] > 1)
        p <- CSGT[1]/CSGT[2]
      else
        p <- CGT[1]/CGT[2]
    }else if(length(CGT) == 1){      
      p <- CGT[1] / V
    }
  }
  return(p)
}

#Probability function
P <- function(words){  
  #max ngram depth: 
  if(length(words) > 3){
    p <- -1
  }
  else{
    #Maximum-likelihood model
    pML <- pMaximumLikelihood(words)
    #Unigram prior
    #pUP <- pUnigramPrior(words, k = 10)
    #smoothing Good-Turing
    pSGT <- pSimpleGoodTuring(words)
    if(!is.na(pML)){
      if(!is.na(pSGT)){
        p <- if(pML > pSGT) {pML} else {pSGT}
      } else {
        p <- 0.0
      }
    } else {
      if(!is.na(pSGT)){
        p <- pSGT
      } else {
        p <- 0.0
      }
    }
      
  }  
  
  return(p)
}

#function for predicting the next word
predictNextWord <- function(sentence) {
  nextWords <- ""
  words <- unlist(strsplit(sentence, " ", fixed=TRUE))
  total_words <- length(words)
  if(total_words > 0){
    #Assuring the last three words to calculate n-gram probabilities
    if(total_words > 2){
      words <- words[(total_words-1):total_words]
      total_words <- length(words)
    }
    #bi-gram to be completed
    if(total_words == 1){
      idx <- d2$w1 == words[1] & d2$freq > 2
      wordsOptions <- data.frame(word = rownames(d2[idx,]))
    } 
    #tri-gram to be completed
    else{
      idx <- d3$w1w2 == paste(words[1],words[2]) & d3$freq > 1
      wordsOptions <- data.frame(word = rownames(d3[idx,]))
    } 
    sentencesSplitted <- lapply(wordsOptions$word, function(x) unlist(strsplit(as.character(x), " ", fixed=TRUE)))
    wordsOptions$prob <- sapply(sentencesSplitted, P)
    wordsOptions$suggestedWord <- sapply(sentencesSplitted, function(x) x[length(x)])
    nextWords <- wordsOptions[order(wordsOptions$prob, decreasing = T),]
    nextWords <- unlist(nextWords[1:3,3])
  }
  return(nextWords)
}

# a function from r-bloggers to clean invalid input in 'utf8towcs'
cleanText <- function(some_txt) {
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", " ", some_txt)
  some_txt = gsub("[[:punct:]]", " ", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("\\s+", " ", some_txt)
    # define "tolower error handling" function
  try.tolower = function(x) {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}