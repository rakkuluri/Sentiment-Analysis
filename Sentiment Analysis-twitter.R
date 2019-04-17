
getwd()
setwd("C:/Users/Ravinder/Desktop/Sentiment Analysis")

install.packages(c("devtools", "rjson", "bit64", "httr"))

#RESTART R session!

library(devtools)
install.packages("curl")
require("curl")
install_github("twitteR", username="XXXXXX")
library(twitteR)
library(plyr)
library(stringr)
library(tm)

#=============================================#
#           Authentication Process            #
#=============================================#


consumer_key      <- "XXXXXXXX"
consumer_secret   <- "XXXXXXXX"
access_token      <- "XXXXXXXX"
access_secret     <- "XXXXXXXX"

setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)

#=============================================#
#           Searching Text                    #
#=============================================#

search.string <- "bendgate"
no.of.tweets <- 1000

input_tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
input_tweets[1:10] 

#============================================================#
#     Extracts a specific set of words as a text string      #
#============================================================#

tweet=sapply(input_tweets,function(x) x$getText()) 

#====================================#
#     Score Sentiment Functions      #
#====================================#

score.sentiment = function(sentences, pos.words,neg.words){ 
  scores = laply(sentences, function(sentence, pos.words, neg.words){ 
    sentence = gsub("[[:punct:]]", "", sentence)  
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub('\\d+', '', sentence)
    #sentence = gsub('[^A-Za-z]', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, "\\s+")
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)    
  },
  pos.words, 
  neg.words)
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}
score.sentiment(tweet[2], pos, neg)
#setwd("..../Datasets")

#============================================================#
#         Positive and Negative Words Analysis               #
#============================================================#

pos=readLines("positive-words.txt") # find file positive_words.txt
neg=readLines("negative-words.txt") # find file negative_words.txt

#============================================================#
#         Score Computation from the Tweets                  #
#============================================================#

scores <- NULL
#pb <- txtProgressBar(max=no.of.tweets, style=3)
#k <- 0
for(i in 1:no.of.tweets){
  #  k <- k+1
  # setTxtProgressBar(pb, k)
  P.scores = score.sentiment(tweet[i], pos, neg)
  scores <- rbind(scores,P.scores)
}
#close(pb)

#============================================================#
#   Categorize each tweet as positive, negative, or neutral  #
#============================================================#

scores$very.pos = as.numeric(scores$score > 0)
scores$very.neg = as.numeric(scores$score < 0)
scores$very.neu = as.numeric(scores$score == 0)

#============================================================#
#       Number of positive, neutral, and negative tweets     #
#============================================================#

numpos = sum(scores$very.pos)
numneg = sum(scores$very.neg)
numneu = sum(scores$very.neu)

#============================================================#
#             Final Results with Pie Plot                    #
#============================================================#

s <- c(numpos,numneg,numneu)
lbls <-c("POSITIVE","NEGATIVE","NEUTRAL")
pct <- round(s/sum(s)*100)
lbls <- paste(lbls, pct)
lbls <-paste(lbls,"%",sep="")

pie(s,labels = lbls, col = rainbow(length(lbls)),main="OPINION")
