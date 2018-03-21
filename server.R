#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
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

unigram <- readRDS("unigram.rdata")
bigram <- readRDS("bigram.rdata")
trigram <- readRDS("trigram.rdata")
quadragram <- readRDS("quadragram.rdata")
quintagram <- readRDS("quintagram.rdata")

badwords <- readLines("swearWords.txt",encoding= "UTF-8", warn = F)

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




shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    result <- predictNextWord(input$entryText,input$numResults)
    output$sentence2 <- renderText({message})
    result
  });
  output$sentence1 <- renderText({
    input$inputText});
}
)
