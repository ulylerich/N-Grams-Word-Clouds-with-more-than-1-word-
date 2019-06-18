#Load library
library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)

#Read Data
com <- read.csv("comments.csv", header = T)

#Create a corpus
corpus <- Corpus(VectorSource(com$Responses))

#clean Text for analysis
## Convert the text to lower case
corpus <- tm_map(corpus, content_transformer(tolower))
# Remove numbers
corpus <- tm_map(corpus, removeNumbers)
# Remove english common stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Remove punctuations
corpus <- tm_map(corpus, removePunctuation)
# Eliminate extra white spaces
corpus <- tm_map(corpus, stripWhitespace)

#check corpus
as.character(corpus[[1]])

#create Bigram

minfreq_bigram<-2 #minimum frequency

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(corpus, Weka_control(min=2,max=2, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]

#world cloud
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),
          min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=50)

#create Trigrams
minfreq_bigram<-5 #minimun frequency

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(corpus, Weka_control(min=3,max=3, delimiters = token_delim))
two_word <- data.frame(table(bitoken))
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]

#world cloud
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(1.2,0.35),
          min.freq = minfreq_bigram,colors = brewer.pal(8,"Dark2"),max.words=50)
