library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(brewer)
library(wordcloud)
library(RColorBrewer)
library(topicmodels)
library(SnowballC)
library(stringr)
library(reshape2)

#structure of config.r file
#consumer_key <- see below
#consumer_secret <- see below
#access_token <- see below
#access_secret <- see below
#banknamestring <- "name of the bank"
#searchterm <- "@name of the bank"



source("config.r")

#Get twitter API settings
#log into twitter
#go to https://apps.twitter.com/
#create a new app
#go to Keys and access tokens

#NOTE PEM file no longer necessary on windows 
#taken from http://davetang.org/muse/2013/04/06/using-the-r_twitter-package/




setup_twitter_oauth(consumer_key,
                    consumer_secret,
                    access_token,
                    access_secret)


bankname <- searchTwitter(searchterm, n=1500)

length(bankname)

banknametext <- sapply(bankname, function(x) x$getText())

banknametextcorpus <- Corpus(VectorSource(banknametext))

#cleaning up tweets code

banknametextcorpus <- tm_map(banknametextcorpus, removePunctuation)
banknametextcorpus <- tm_map(banknametextcorpus, function(x)removeWords(x,stopwords()) )
banknametextcorpus <- tm_map(banknametextcorpus, content_transformer(tolower))


palDark2 <- brewer.pal(8,"Dark2")
wordcloud(banknametextcorpus, min.freq=20, colors = palDark2)


#document term matrix and frequency counts
banknametextcorpusdtm <- DocumentTermMatrix(banknametextcorpus)

#need to find proper method for dealing with this
banknamematrix <- as.matrix(banknametextcorpusdtm)

#find the terms which appear at least 20 times
#banknamefreqlist <- findFreqTerms( banknamedtm ,20 )
banknamefreq <- sort(colSums(as.matrix(banknamematrix)), decreasing=TRUE)   
banknamefreqframe <- data.frame(word=names(banknamefreq), freq=banknamefreq)
banknamefreqframe <- banknamefreqframe [1: 20,]

#plot the first 20 terms (histogram)
p <- ggplot(banknamefreqframe , aes(word, freq))    
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=90, hjust=1))  
p





#topic modelling
#https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
#http://www.r-bloggers.com/a-delicious-analysis-aka-topic-modelling-using-recipes/

#create a dictionary object where everything has appeared at least 20 times
banknametweetsdict = findFreqTerms(banknametextcorpusdtm,20)
#removing bankname and banknames from dict
banknametweetsdict <- banknametweetsdict [banknametweetsdict != bankname]
banknametweetsdict <- banknametweetsdict [banknametweetsdict !=  paste (banknamestring,"s",sep = "")]
#Extracting only the dict objects
banknametextcorpusdtmfiltered = DocumentTermMatrix(banknametextcorpus, list(dictionary = banknametweetsdict))

#zero rows appear and need to be removed
#http://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
rowTotals <- apply(banknametextcorpusdtmfiltered , 1, sum)
banknametextcorpusdtmfiltered   <- banknametextcorpusdtmfiltered[rowTotals> 0, ] 

banknamelda = LDA(banknametextcorpusdtmfiltered, 3)
t = terms(banknamelda,5)
t


#get bankname timeline
#banknametimeline <- userTimeline('bankname',n=20)
#not useful for this application

positive <- readLines("positive-words.txt")
negative <- readLines("negative-words.txt")

SentimentScorer <- function (  tweets, positive, negative ){
  
  notweets <- length(tweets)
  pcount <- vector("numeric")
  ncount <- vector("numeric")
  
  
  for (i in 1: notweets){
    temp <- strsplit(tweets[i], " ")
    #for whatever reason temp can be accessed with [[1]]
    #seen this in python before and is annoying
    temp <- temp[[1]]
    npos <- temp %in% positive
    nneg <- temp %in% negative
    
    pcount <- cbind (pcount,  length(which(npos)))
    ncount <- cbind (ncount,  length (which (nneg))  )
    
  }
  pcount <- t(pcount)
  ncount <- t(ncount)
  
  #used for melt later on
  tweetno<- c(1: notweets)
  #tweetno<- t(tweetno)
  return (data.frame(pcount, ncount, tweetno))
}



sentiments <- SentimentScorer( banknametext, positive, negative ) 
#kind of boring as is because for a lot of values it is neither positive or negative

sentimentslong <- melt(sentiments, id.vars = "tweetno")

sentimentslong[1:100,]

p <- ggplot(data = sentimentslong, mapping = aes(x = tweetno, y=value, colour = variable))
p <- p+ geom_line()
p

