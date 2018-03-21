library("tm") # for text mining and corpus
library("data.table")
library("RCurl")
library("SnowballC")
library("stringi")
library("wordcloud")
library("ggplot2") # for plotting
library("RWeka") #for tokenization
library("Matrix")

RemoveEmails <- function(x) {gsub("\\S+@\\S+", "", x)} 
RemoveUrls <- function(x) {gsub("http[[:alnum:]]*","",x)}
RemoveHashtags <- function(x) {gsub("#[[:alnum:]]*","",x)}
RemoveHandles <- function(x) {gsub("@[[:alnum:]]*","",x)}


setwd("C:\\Users\\AKINO\\Desktop\\capstone\\final\\en_US") #set the directory to the current one

clean_file <- function(file){
  text <- readLines(file,encoding= "UTF-8", warn = F)
  nonascIndex <- grep("text.tmp", iconv(text, "latin1", "ASCII", sub="text.tmp"))
  text <- text[-nonascIndex]
}
blog <- clean_file("en_US.blogs.txt")
news <- clean_file("en_US.news.txt")
twitter <-clean_file("en_US.twitter.txt")

set.seed(2345)

blogSample <- blog[sample(1:length(blog), 10000)]
newsSample <- news[sample(1:length(news), 10000)]
twitterSample <- twitter[sample(1:length(twitter), 10000)]
badwords <- readLines("swearWords.txt",encoding= "UTF-8", warn = F)
fusedText<- c(blogSample,newsSample,twitterSample)


CleanCorpus <- function(chrVector) {
  corpus<- Corpus(VectorSource(list(chrVector))) # create corpus from all the files
  corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
  corpus <- tm_map(corpus,RemoveEmails)
  corpus <- tm_map(corpus,RemoveUrls)
  corpus <- tm_map(corpus,RemoveHashtags)
  corpus <- tm_map(corpus,RemoveHandles)
  corpus <- tm_map(corpus, removeWords, c("rt","pm","p m")) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))
  corpus<- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  corpus <- tm_map(corpus, removeWords, badwords)
  corpus<- tm_map(corpus, PlainTextDocument)
  corpus
}


CleanCorpus<- CleanCorpus(fusedText)



CreateNgram <- function(minmaxgram,delimiterin=NULL) {
  if (!is.null(delimiterin)){
    ngram <- NGramTokenizer(CleanCorpus, Weka_control(min = minmaxgram, max = minmaxgram,delimiters =delimiterin))
  }else{
    ngram <- NGramTokenizer(CleanCorpus, Weka_control(min = minmaxgram, max = minmaxgram))
  }
  #We create the dataFrame with the words and the frequencies
  ngram <- data.frame(V1 = as.vector(names(table(unlist(ngram)))), V2 = as.numeric(table(unlist(ngram))))
  names(ngram) <- c("word","freq")
  ngram <- ngram[with(ngram, order(-ngram$freq)),]
  row.names(ngram) <- NULL
  ngram$cumsum <- cumsum(ngram$freq)
  ngram$pct <- (ngram$freq/sum(ngram$freq))*100
  ngram$cumpct <- cumsum(ngram$pct)
  ngram
}

#Creating the NGRAMS takes time, we save them to an RDATA the first time so we can load them later on

unigram <- CreateNgram(1)
bigram <- CreateNgram(2, " \\r\\n\\t.,;:\"()?!")
trigram <- CreateNgram(3," \\r\\n\\t.,;:\"()?!")
quadragram <- CreateNgram(4," \\r\\n\\t.,;:\"()?!")
quintagram <- CreateNgram(5," \\r\\n\\t.,;:\"()?!")

save(unigram, file="unigram.Rda")
save(bigram, file="bigram.Rda")
save(trigram, file="trigram.Rda")
save(quadragram, file="quadragram.Rda")
save(quintagram, file="quintagram.Rda")

saveRDS(unigram, file="unigram.Rdata")
saveRDS(bigram, file="bigram.Rdata")
saveRDS(trigram, file="trigram.Rdata")
saveRDS(quadragram, file="quadragram.Rdata")
saveRDS(quintagram, file="quintagram.Rdata")

#load(unigram, file="./unigram.Rda")
#load(bigram, file="./bigram.Rda")
#load(trigram, file="./trigram.Rda")
#load(quadragram, file="./quadragram.Rda")
#load(quintagram, file="./quintagram.Rda")

predictNextWord <- function(inputString, numOfResults) {
  CleanedText <- cleanText(inputString)
  TextLength <- length(unlist(strsplit(CleanedText, " ")))
  if(TextLength == 0) {
    return(predictGram1(CleanedText, numOfResults))
  }
  
  if(TextLength == 1) { 
    return(predictGram2(CleanedText, numOfResults))
  }
  
  if(TextLength == 2) { 
    return(predictGram3(CleanedText, numOfResults))
  }
  
  if(TextLength == 3) { 
    return(predictGram4(CleanedText, numOfResults))
  }
  
  if(TextLength >= 4) { 
    return(predictGram5(CleanedText, numOfResults))
  }
}


predictGram1 <- function(inString, y) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=0, max=0))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- unigram[grepl(paste0("^"), unigram$word),]
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram2 <- function(inString, y) { 
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- bigram[grepl(paste0("^",lastngram[1]," "), bigram$word),]
  if(nrow(prediction) == 0) { return(predictGram1(inString, y))  }
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram3 <- function(inString, y) { 
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- trigram[grepl(paste0("^",lastngram[1]," ",lastngram[2]," "), trigram$word),]
  if(nrow(prediction) == 0) { return(predictGram2(inString, y)) }
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram4 <- function(inString, y) { 
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quadragram[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," "), quadragram$word),]
  if(nrow(prediction) == 0) { return(predictGram3(inString, y)) }
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

predictGram5 <- function(inString, y) {
  kevTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
  ngrams <- kevTokenizer(inString)
  lastngram <- unlist(strsplit(ngrams[length(ngrams)], " "))
  prediction <- quintagram[grepl(paste0("^",lastngram[1]," ",lastngram[2]," ",lastngram[3]," ",lastngram[4]," "), quintagram$Terms),]
  if(nrow(prediction) == 0) { return(predictGram4(inString, y)) }
  mostFrequentWords <- toString(prediction$word[1:y])
  return(mostFrequentWords)
}

cleanText <- function(inString) {
  inString <- iconv(inString, from = "UTF-8", to = "latin1")
  inCorpus <- Corpus(VectorSource(inString))
  inCorpus <- tm_map(inCorpus,RemoveEmails)
  inCorpus <- tm_map(inCorpus,RemoveUrls)
  inCorpus <- tm_map(inCorpus,RemoveHashtags)
  inCorpus <- tm_map(inCorpus,RemoveHandles)
  inCorpus <- tm_map(inCorpus, removePunctuation)
  inCorpus <- tm_map(inCorpus, removeNumbers)
  inCorpus <- tm_map(inCorpus, stripWhitespace)
  inCorpus <- tm_map(inCorpus, content_transformer(tolower))
  inCorpus <- tm_map(inCorpus, removeWords, badwords)
  inCorpus <- tm_map(inCorpus, PlainTextDocument)
  return(inCorpus$content[[1]][[1]])
}

