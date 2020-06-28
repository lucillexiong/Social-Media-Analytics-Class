#base for countries analysis

#import libraries
library(tidyverse)
library(tm)
library(SentimentAnalysis)
library(syuzhet)
library(ggplot2)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(biclust)
library(cluster)
library(igraph)
library(fpc)
#install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")
library(Rcampdf)
library(sentimentr)
library(summarytools)
library(plotly)
library(quanteda)
library(dplyr)
library(e1071)
library(caret)
library(randomForest)
library(doSNOW)
library(readxl)
library(plyr)
library(knitr)
library(qdapDictionaries)
library(tidytext)
library(rvest)
library(stringr)
library(colorRamps)
require(tidyr)
require(gridExtra)
library(ngramrr)
library(qdap)
library(readtext)
library(devtools)
library(rJava)
library(lattice)
library(udpipe)
library(igraph)
library(ggraph)
library(lattice)



reddit_data <- read.csv(file.choose())
reddit_data$score <- as.numeric(reddit_data$score)
reddit_data$num_comments <- as.numeric(reddit_data$num_comments)
reddit_data$Date <- as.Date(reddit_data$created_utc, format = "%Y-%m-%d")
reddit_data$time <- format(as.POSIXct(reddit_data$created_utc,format="%Y-%m-%d %H:%M:%S"),"%H")
reddit_data$day <- format(as.POSIXct(reddit_data$created_utc,format="%Y-%m-%d %H:%M:%S"),"%d")
reddit_data$TextLength <- nchar(reddit_data$title)

reddit_data$title <- removeWords(reddit_data$Title, "21 positive")

title_data <- VCorpus(VectorSource(reddit_data$title))
title_data <- tm_map(title_data, stripWhitespace)
title_data <- tm_map(title_data, content_transformer(tolower))
title_data <- tm_map(title_data, removeNumbers)
title_data <- tm_map(title_data, removePunctuation)
title_data <- tm_map(title_data, removeWords, stopwords("english"))

titles <- data.frame(Title = title_data[[1]]$content)

for (i in 2:5000){
  titles <- rbind(titles, data.frame(Title = title_data[[i]]$content))
}
titles$Title <- removeWords(titles$Title, "best case: diamond princess")
titles$Title <- removeWords(titles$Title, "grand princess")
titles$Title <- removeWords(titles$Title,"wonderful vacation")
titles$Title <- removeWords(titles$Title, "diamond princess")
titles$Title <- removeWords(titles$Title, "egypt health admenstration  new corona cases discovered")
titles$Title <- removeWords(titles$Title, "popular with tourists")

#gets sentiment score for title of each reddit post
sentiment <- sentiment_by(get_sentences(titles$Title), by=NULL)

print(qplot(sentiment$ave_sentiment, geom="histogram",binwidth=0.1,main="Review Sentiment Histogram"))

#adds the information from sentiment scoring back into reddit data
reddit_data$word_count <- sentiment$word_count
reddit_data$ave_sentiment <- sentiment$ave_sentiment

#finds authors with the top average sentiment scores
reddit_data %>% group_by(ï..author) %>%
  summarize(sent = mean(ave_sentiment)) %>%
  arrange(desc(sent)) %>%
  head(n = 10)

#finds authors with the lowest average sentiment scores
reddit_data %>% group_by(ï..author) %>%
  summarize(sent = mean(ave_sentiment)) %>%
  arrange(sent) %>%
  head(n = 10)

#stems title dat, makes a dtm and finds top 50 most frequent words
title_data_stemmed <- tm_map(title_data, stemDocument)
dtm_no_control <- DocumentTermMatrix(title_data_stemmed)
tophundred <- head(sort(colSums(as.matrix(dtm_no_control)), decreasing=TRUE), 300)

#Regression Analysis ----
reddit_data$day <- as.numeric(reddit_data$day)
Reg_score <- lm(reddit_data$score ~ reddit_data$word_count + reddit_data$day)
summary(Reg_score)

Reg_comments <- lm(reddit_data$num_comments~reddit_data$word_count)
summary(Reg_comments)

#list of my words
my_words<- c("itali", "china", "korea", "south", "australia", "india", "chines", "usa","union","eu", "iran", "uk", "america", "italian", "europ", "bali", "irish", "bhutan", "australian", "turkey", "cambodia", "india", "thai", "ireland", "canada", "columbia", "american", "tasmania", "carribean", "singapore", "england", "state", "country", "travel", "county",  "lockdown", "chinese", "york", "city", "washington", "apulia", "countries", "nonessenti", "nonessential", "ban", "restrict", "domestic", "citizen", "airport", "hotel", "boat", "nile", "turkish", "visa", "cyprus", "arrive", "oversea", "capital", "province", "hubei", "wuhan", "mainland", "brunkswick", "waterloo", "alberta", "vancouver", "bhutan", "canadian", "westjet", "vacation", "expedia", "book", "checkin", "rental", "ticket", "vacat", "drove", "rome", "florida", "oregan", "tourist", "tour", "airbnb",  "world", "vacasa", "milan", "citizen", "airport", "lombardi", "sicili", "marriot", "motel", "seoul", "london", "global", "world", "map", "worldwid", "europ", "airlin", "continent", "quebec", "nation", "countri", "outsid", "fli", "texa", "tourism", "tour", "country", "air","countri","home", "australian","indian", "cruis")

#dtm with filtered data
dtm <- DocumentTermMatrix(title_data_stemmed, control=list(dictionary = my_words))

#New data frame with countries
df1 <- data.frame(as.matrix(dtm), stringAsFactors=FALSE)

#frequency of the dtm
freq <- colSums(as.matrix(dtm))   
freq 

#adds the words we want to look at into reddit_data
reddit_data$italy <- df1$itali
reddit_data$italian <- df1$italian
reddit_data$china <- df1$china
reddit_data$chinese <- df1$chines
reddit_data$korea <- df1$korea
reddit_data$australia <- df1$australia
reddit_data$australian <- df1$australian
reddit_data$canada <- df1$canada
reddit_data$canadian <- df1$canadian
reddit_data$america <-df1$america
reddit_data$usa <- df1$usa
reddit_data$american <- df1$american
reddit_data$travel <- df1$travel
reddit_data$tourism <- df1$tourism
reddit_data$air <- df1$air
reddit_data$airbnb <- df1$airbnb
reddit_data$airlin <- df1$airlin
reddit_data$airport <- df1$airport
reddit_data$boat <- df1$boat
reddit_data$fli <- df1$fli
reddit_data$home <- df1$home
reddit_data$hotel <- df1$hotel
reddit_data$motel <- df1$motel
reddit_data$tour <- df1$tour
reddit_data$tourist <- df1$tourist
reddit_data$vacation <- df1$vacation
reddit_data$westjet <- df1$westjet
reddit_data$cruis <- df1$cruis

#reddit_data now contains the orginal information plus sentiment scores, words in title, and binary variables of words that appear

italy <- filter(reddit_data, italy>0 | italian>0)
china <- filter(reddit_data, china>0 | chinese>0)
korea <- filter(reddit_data, korea > 0)
canada <- filter(reddit_data, canada>0 | canadian >0)
australia <- filter(reddit_data, australia >0 | australian >0)
usa <- filter(reddit_data, america>0 | usa>0 | american > 0)
travel <- filter(reddit_data, travel > 0)
tourism <- filter(reddit_data, tourism>0 | tour>0 | tourist>0)
accomodation<-filter(reddit_data, motel>0 | hotel>0 | airbnb>0)
flying <- filter(reddit_data, westjet>0 | fli>0 | airport>0 | air>0 | airlin>0)
cruise <- filter(reddit_data, cruis>0 | boat>0)
home<-filter(reddit_data, home>0)

dim(tourism)

prop.table(table(italy$day))
prop.table(table(china$day))
prop.table(table(korea$day))
prop.table(table(canada$day))
prop.table(table(australia$day))
prop.table(table(usa$day))
prop.table(table(travel$day))
prop.table(table(tourism$day))
prop.table(table(accomodation$day))
prop.table(table(flying$day))
prop.table(table(cruise$day))
prop.table(table(home$day))




names <- c("Italy", "China", "Korea", "Canada", "Australia", "USA", "Travel", "Tourism", "Accomodation", "Flying", "Cruise", "Home")
i=1

#Creates histograms for the sentiment of each top word
for (word in list(italy,china,korea,canada,australia,usa,travel, tourism, accomodation,flying,cruise,home)){
  histogram <- hist(word$ave_sentiment, main = paste("Histogram of Sentiment Scores of", names[i]), xlab = "Sentiment Scores")
  i = i+1
}


#regression to see if we can predict sentiment scores using the reddit post score and number of comments
#regression_list = list()
key_words <- list(italy,china,korea,canada,australia,usa,travel, tourism, accomodation,flying,cruise,home)
for (i in 1:12){
  Reg1 <- lm(ave_sentiment~num_comments+score, data=key_words[[i]])
  print(summary(Reg1))
}


for (i in 1:12){
  Reg2 <- lm(ave_sentiment~num_comments, data=key_words[[i]])
  print(summary(Reg2))
}


for (i in 1:12){
  Reg3 <- lm(ave_sentiment~score, data=key_words[[i]])
  print(summary(Reg3))
}

for (i in 1:12){
  print(names[i])
  Reg4 <- lm(score~ave_sentiment+word_count, data=key_words[[i]])
  print(summary(Reg4))
}

for (i in 1:12){
  print(names[i])
  Reg5 <- lm(score~ave_sentiment, data=key_words[[i]])
  print(summary(Reg5))
}

i=1
for (word in list(italy,china,korea,canada,australia,usa,travel, tourism, accomodation,flying,cruise,home)){
  dailyaverage <- data.frame(word$Date, word$ave_sentiment)
  mean(dailyaverage$word.ave_sentiment)
  dailysentaverage <- aggregate(dailyaverage$word.ave_sentiment, by = list(dailyaverage$word.Date) , FUN = mean)
  plot(dailysentaverage$Group.1, dailysentaverage$x, type="l", col="darkseagreen4", main = paste("Average Sentiment Scores Over Time for",names[i]), xlab = "Date", ylab="Average Sentiment")
  i=i+1
}

for (word in list(italy,china,korea,canada,australia,usa,travel, tourism, accomodation,flying,cruise,home)){
  print(mean(word$ave_sentiment))
}

i=1
for (word in list(italy,china,korea,canada,australia,usa,travel, tourism, accomodation,flying,cruise,home)){
  print(mean(word$ave_sentiment))
  print(qplot(word$Date, word$ave_sentiment, main = paste("Sentiment of",names[i])))
  i=i+1
}


totalAvgSentiment <-c(-0.01297392,-0.118739,-0.04568562,0.04069791,-0.05210596,-0.06782139,-0.0380617,-0.0905812,-0.1104738,-0.04021328,0.0005966416,0.02720339)
meanSentiment <- data.frame(names=c("Italy", "China", "Korea", "Canada", "Australia", "USA", "Travel", "Tourism", "Accomodation", "Flying", "Cruise", "Home"),totalAvgSentiment=c(-0.01297392,-0.118739,-0.04568562,0.04069791,-0.05210596,-0.06782139,-0.0380617,-0.0905812,-0.1104738,-0.04021328,-0.006103611,0.02720339))
meanSentiment$names <- as.factor(meanSentiment$names)
ggplot(meanSentiment) + geom_bar(aes(x = names, y = totalAvgSentiment), stat = "identity") + labs(title="Average Sentiment per Keyword",x="Keyword",y="Average Sentiment")

#--------TDIF
#tokenize
reddit_tokens <- tokens(reddit_data$title, what = "word",
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE, split_hyphens = TRUE)

#make everything lower case
reddit_tokens <- tokens_tolower(reddit_tokens)
reddit_tokens <- tokens_select(reddit_tokens, stopwords(), selection = "remove")

#create our first bag of words model
reddit_tokens.dfm <- dfm(reddit_tokens, tolower = FALSE, remove = stopwords())

#transform to a matrix and inspect
reddit_tokens.matrix <- as.matrix(reddit_tokens.dfm)

#cleanup column names
names(reddit_tokens.matrix) <- make.names(names(reddit_tokens.matrix))

#function to calculate relative term frequency
term.frequency <- function(row){
  row/sum(row)
}

#function for calculating inverse document frequency
inverse.doc.freq <- function(col){
  corpus.size <- length(col)
  doc.count<-length(which(col>0))
  
  log10(corpus.size/doc.count)
}

#function for calculating TF-IDF
tf.idf <- function(tf, idf){
  tf*idf
}

#first step, normalize all documents via TF
reddit_tokens.matrix <- apply(reddit_tokens.matrix, 1, term.frequency)

#second step, calculate the IDF vector that we will use - both
#for training data and for the test data!
reddit_tokens.idf <- apply(reddit_tokens.matrix, 2, inverse.doc.freq)
str(reddit_tokens.idf)

#Lastly, calculate TF-IDF 
reddit_tokens.tfidf <- apply(reddit_tokens.matrix, 2, tf.idf, idf = reddit_tokens.idf)

# Transpose the matrix
reddit_tokens.tfidf <- t(reddit_tokens.tfidf)
dim(reddit_tokens.tfidf)
View(reddit_tokens.tfidf[1:25, 1:25])

#===============FREQUENCY ANALYSIS=======================

#------------TOTAL DATA----------------
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
m <- as.matrix(dtm)
dim(m)

dtms <- removeSparseTerms(dtm, 0.1)

head(table(freq), 20)

tail(table(freq), 20)

freq <- colSums(as.matrix(dtms))
freq

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 20) 

findFreqTerms(dtm, lowfreq = 50)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

#Plot Word Frequencies
p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p   

p <- ggplot(subset(wf, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

p <- ggplot(subset(wf, freq>150), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))
p

#Word Cloud
dtms <- removeSparseTerms(dtm, 0.15)
freq <- colSums(as.matrix(dtm))
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100,rot.per=0.2, colors=dark2)

#separate the data into before, during, and after
before <- reddit_data[1:1721,]
during <- reddit_data[1722:2082,]
after <- reddit_data[2083:5000,]

for (data in list(before, during, after)){
  corp <- VCorpus(VectorSource(data$title))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  
  corp[[1]]$content
  
  corp_stemmed <- tm_map(corp, stemDocument)
  
  split_dtm <- DocumentTermMatrix(corp_stemmed)
  split_tdm <- TermDocumentMatrix(corp_stemmed)
  
  inspect(split_dtm)
  inspect(split_tdm)
  
  freq <- colSums(as.matrix(split_dtm))
  length(freq)
  ord <- order(freq)
  matr <- as.matrix(split_dtm)
  dim(matr)
  
  dtms <- removeSparseTerms(split_dtm, 0.1)
  
  head(table(freq), 20)
  
  tail(table(freq), 20)
  
  freq <- colSums(as.matrix(dtms))
  freq
  
  freq <- sort(colSums(as.matrix(split_dtm)), decreasing=TRUE)   
  head(freq, 20) 
  
  findFreqTerms(split_dtm, lowfreq = 50)
  
  wf <- data.frame(word=names(freq), freq=freq)
  head(wf)
  
  #Plot Word Frequencies Before
  p <- ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  p   
  
  #Word Cloud before
  dtms <- removeSparseTerms(split_dtm, 0.15)
  freq <- colSums(as.matrix(split_dtm))
  dark2 <- brewer.pal(6, "Dark2")
  wordcloud(names(freq), freq, max.words=100,rot.per=0.2, colors=dark2)
  
  print(findAssocs(dtm, "coronavirus", corlimit=0.03))
  print(findAssocs(dtm, "covid", corlimit=0.07))
  print(findAssocs(dtm, "case", corlimit=0.1))
  print(findAssocs(dtm, "travel", corlimit=0.15))
  print(findAssocs(dtm, "tourism", corlimit=0.1))
  print(findAssocs(dtm, "tourist", corlimit=0.1))
  print(findAssocs(dtm, "itali", corlimit=0.1))
  print(findAssocs(dtm, "china", corlimit=0.1))
  print(findAssocs(dtm, "korea", corlimit=0.1))
  print(findAssocs(dtm, "australia", corlimit=0.1))
  print(findAssocs(dtm, "america", corlimit=0.1))
  print(findAssocs(dtm, "canada", corlimit=0.1))
}

#---------------Unigram analysis------------------

for (data in list(reddit_data,before, during, after)){
  italy_split <-filter(data, italy + italian>0)
  china_split <- filter(data, china + chinese>0)
  korea_split <-filter(data, korea>0)
  australia_split <- filter(data, australia + australian>0)
  canada_split <- filter(data, canada + canadian>0)
  usa_split <- filter(data, america + american +usa>0)
  travel_split <- filter(data, travel>0)
  for(country in list(italy_split,china_split,korea_split,australia_split,canada_split,usa_split,travel_split)){
    corpus_data <- VCorpus(VectorSource(country$title))
    corpus_data <- tm_map(corpus_data, stripWhitespace)
    corpus_data <- tm_map(corpus_data, content_transformer(tolower))
    corpus_data <- tm_map(corpus_data, removeNumbers)
    corpus_data <- tm_map(corpus_data, removePunctuation)
    corpus_data <- tm_map(corpus_data, removeWords, stopwords("english"))
    
    dtm <- DocumentTermMatrix(corpus_data)
    
    corp_stemmed <- tm_map(corpus_data, stemDocument)
    
    freq <- colSums(as.matrix(dtm))
    length(freq)
    ord <- order(freq)
    matr <- as.matrix(dtm)
    dim(matr)
    
    dtms <- removeSparseTerms(dtm, 0.1)
    
    head(table(freq), 20)
    
    tail(table(freq), 20)
    
    freq <- colSums(as.matrix(dtms))
    freq
    
    freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
    head(freq, 20) 
    
    findFreqTerms(dtm, lowfreq = 50)
    
    wf <- data.frame(word=names(freq), freq=freq)
    wf
    
    #Plot Word Frequencies Before
    p <- ggplot(subset(wf, freq>15), aes(x = reorder(word, -freq), y = freq)) +
      geom_bar(stat = "identity") + 
      theme(axis.text.x=element_text(angle=45, hjust=1))
    p   
    
    #Word Cloud before
    dtms <- removeSparseTerms(dtm, 0.15)
    freq <- colSums(as.matrix(dtm))
    dark2 <- brewer.pal(6, "Dark2")
    wordcloud(names(freq), freq, max.words=100,rot.per=0.2, colors=dark2)
    
    print(findAssocs(dtm, "travel", corlimit=0.05))
    print(findAssocs(dtm, "tourism", corlimit=0.001))
    print(findAssocs(dtm, "tourist", corlimit=0.05))
    print(findAssocs(dtm, "itali", corlimit=0.01))
    print(findAssocs(dtm, "china", corlimit=0.01))
    print(findAssocs(dtm, "korea", corlimit=0.01))
    print(findAssocs(dtm, "australia", corlimit=0.01))
    print(findAssocs(dtm, "america", corlimit=0.001))
    print(findAssocs(dtm, "canada", corlimit=0.01))
  }
}

#----------------BIGRAM AND TRIGRAM GENERAL ANALYSIS------------
reddit_data$title_char <- as.character(reddit_data$title)

mycorp <- corpus(reddit_data$title_char)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## bg1: the words that appear most often together include, new _cases, corona_virus, coronavirus cases, etc new york, south coria, 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures2

# the 3 words that appear most often together, test postivei coronavirus, new case coronavirus, declares state of emergency 
  
  
#Time Analysis  
before <- reddit[1:1721,]
during <- reddit[1722:2082,]
after <- reddit[2083:5000,]

# BEFORE THE ANNOUNCEMENT: BG2
before$title <- as.character(before$title)

mycorp <- corpus(before$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## the words that appear most often together dont really change from total set, new _cases, corona_virus, coronavirus cases, first case, etc new york, south Korean
#cruise ship is on here, washington state, , 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2

# the 3 words that appear most often together, new cases coronavirus, first case, first confirmed case, tested postivei coronavirus, new case coronavirus, declares state of emergency 
#princess cruise ship, grand princess cruise ship 
# similar analysis nd findings to the POS one tbh 

# During THE ANNOUNCEMENT: bg3
during$title <- as.character(during$title)

mycorp <- corpus(during$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## these words on the day of are differed, due coronavirus, tom hanks, positive coronavirus, rita wilson, they are liteally all about tom hanks and rita wilson and online classes

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2
# yeah the 3 words are really similar stuff to bigrams



# After THE ANNOUNCEMENT: BG4
after$title <- as.character(after$title)

mycorp <- corpus(after$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

# the words that appear most often together are now mostly about new cases, pandemic, state emergency, stay home, new york, prime minister 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2
# after the announcement, people tend to talk about new cases and new positive tests and live updates. its like they just want to know what is the new number every day
# it also contains words such as concerns, crisis, thats how theyre descriving it now

#----------------------POS ANALYSIS-----------------------------
txt <- paste(reddit_data$title, collapse = "")

ud_model <- udpipe_download_model(language = "english")

ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = reddit_data$title, doc_id = reddit_data$ï..author)
x <- as.data.frame(x)

#Basic Frequency Statistics
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue",
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence",
         xlab = "Freq")
# Plot 1:the reddit_data data contains mainly nouns and pronouns 

#Nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "lightblue",
         main = "Most occurring nouns", xlab = "Freq")
# Plot2: the most frequently occuring nouncs are actually the most frequenlt occuring terms overall = coronavirus, cases, covid, people, vusy, case etc 



# ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple",
         main = "Most occurring adjectives", xlab = "Freq")

#Plot 3: Most occuring adjectives are overwhemlingly, "new, positive, more, total, first, chinese, "

#Verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold",
         main = "Most occurring Verbs", xlab = "Freq")
#plot4 : most occuring verbs are, confirmed, have, says, tested, get, help, infected etc. (confirmed is almost double of the 2nd highest)


# FInding Key Words: there are 3 methods to finding keyewords
# Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id",
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 25), 20), col = "cadetblue",
         main = "Keywords identified by RAKE",
         xlab = "Rake")
# plot 5: highest and main keywords in this data set are: " test positive, first case, new case, coronairus case, coronqvirus outbreak,"


# Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 60), 20), col = "cadetblue",
         main = "Keywords identified by PMI Collocation",
         xlab = "PMI (Pointwise Mutual Information)")
#plot6: i would say this method doesnt work as well, but the keywords stay the same basically 

# Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")
#plot7: this method fave me similar key words to the first method, some interesting words such as cruise ship
#i see that new york and south korea appear much higher on this list, as well as washington state, state of emergency, public health and toilet paper

# Another way to do Co-occurences

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc, 20)
# Ive done cooccurences on a different scrit but this is a good way to compare methods. So my top terms are : case+new(291), Case+covid(217), honestly nothing special here 

#Word Networkd
# Nouns and adjectives only 
wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

# Plot8: Im starting to see cruise ship appear a lot more, interesting term related to tourism

# nouns and adjectives that follow each other 
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)
# new + case, coronavisu(170) + case(94)

wordnetwork <- head(cooc, 200)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")
#plot 9: this is a great plot, so it shows me nouns and adjectives that most frequently occur together in the same title 
# what im seeing is that cruise ship is always seperate from the cluster surrounding the term"coronavisu". chinese on the other hand is linked directly
# ban, travel and essential are all linked together , why do i keep seeing place+shelter?
# Pplot 10: lets say i increase the frequency count to 200 from 100, i start to see words like flight + international ( but its not in the cluster, so modes of transportation are not the concern for travel), however countries are directly reltated to the cluster


# Correlations 

x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 20)
# I know arnica is doing this as well but I may as well check and learn more- things that have the highest correlation are "HRK + USD (currency?), paper+toilet, day+parade, el+son, conference + press, cruise+ship 


## Analysis Over Time

# Before the announcement 
before <- reddit_data[1:1721,]
during <- reddit_data[1722:2082,]
after <- reddit_data[2083:5000,]


ud_model <- udpipe_download_model(language = "english")

ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = before$title, doc_id = before$author)
x <- as.data.frame(x)

#Basic Frequency Statistics
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", xlab = "Freq")

# nothing special here

#Nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 30), col = "lightblue", 
         main = "Most occurring nouns", xlab = "Freq")
# Plot11: the most frequently occuring nouncs dont change= coronavirus, cases, case, covid people, state, outbreak, deaths  etc 


# ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", main = "Most occurring adjectives", xlab = "Freq")

#Plot 12: Most occuring adjectives are still the same: "new, positive, coronavirus, total, first, more, total, chinese, possible, american, "

#Verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")
#Plot13 : most occuring verbs are, confirmed, have, tested, reported, infected etc. (confirmed is almost double of the 2nd highest)
# i can see a slight difference from before the announcement, people see much more skeptical, curious, everything is new to them 


# FInding Key Words: there are 3 methods to finding keyewords
# Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 25), 20), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")
# plot 14: highest and main keywords in this data set are: " new case (higher on this list), first case, , coronairus case,  positive,"
# so definetly the before data set is more about new and firsts 




# Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 60), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")

# Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")
#Plot15: i really think this is the best method, i see of course: new cases, corona virus, first case, coronavirus outbreak. here south korea and new york are right next to each other 
#people are talking about cruise ship and washington state, even the grand princess is on here, south africa, white house

# Another way to do Co-occurences

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc, 20)
#  So my top terms are : case+new (140, this means that the data is pretty evenly distributed when talking about new cases), Case+covid (126), case first (62), honestly nothing special here 

#Word Network

# Nouns and adjectives only 
wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

# Plot16: i see a lot of more terms relating to presumption, fear, updats, novel 
# also something interesting, cruise ship is linked to the covid cluster here, so is country

# nouns and adjectives that follow each other 
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)
# new + case(95), coronavisu(53) + case, first +case (48), coronavirus + outbreak(25)

wordnetwork <- head(cooc, 200)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")
#plot 17: this is the best plot ever, here i see tourism, travle connected to american, transport and public 
# in terms of countries, italian is related to doctors , chinese stays deeply inside the cluster
# interesting terms, princess diamond cruise, , history travel ban, flight attendent, spring break, hotel collapse 
# its cool so before the announcement you can see how people were using terms like suspect, presumptious possible, 

### Correlations 
x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 20)
# on this correlation list, paper +toilet, cruise+ sjop. case+ new, conference + press, collapse + hotel, fligjt + mass, mass + travel!


#On the Day of the Announcement
x <- udpipe_annotate(ud_model, x = during$title, doc_id = during$author)
x <- as.data.frame(x)

#Basic Frequency Statistics
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")
# nothing of interest

#Nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "lightblue", main = "Most occurring nouns", xlab = "Freq")
# Plot18: the most frequently occuring nouncs on the day of are= coronavirus, cases, covid, people, vusy, case etc 
# here i see schools higher on the list, pandemic is low but its there, travel is also here 



## ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

#Plot 19: Most occuring adjectives are now, " positive, coronavirus, new, total, italian, first, more, online, local, medical , "
# suprisingly japanese and chinese are much lower on the list

#Verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")
#plot20 : most occuring verbs are, confirmed, , says, has, get, have, declares, tested, infected, suspends, cancelled etc. (confirmed is almost double of the 2nd highest)
# words like suspend and canclled, closed make a lot of sense fot the day of the annoucnemetn 

### FInding Key Words: 
## Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 10), 20), col = "cadetblue", 
         main = "Keywords identified by PMI Collocation", 
         xlab = "PMI (Pointwise Mutual Information)")
#plot21: there is a lot less data from today, we see keywords like "rita wilson, tom hanks, my school, new york, new mexico 

# Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")
#plot21: same words as the one aboce, 

# Another way to do Co-occurences

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc, 20)
# my top terms are : positive +test (13), coronavisu test (11), case +tootal (11), honestly nothing special here 

#Word Networkd

# Nouns and adjectives only 
wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

# PPlot22: okay so on the day of the announcement, travel is directly inside the cluster, surrounded by " air and hour" 
# words related to country like country, region, state, are always tied to cases and covid 

# nouns and adjectives that follow each other 
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)
# test+positive (12), coronavirus +case (8)

wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")
#plot 23: now whn people talk about covid, they talk about lockdown, declare, response 
# words related to travel: chinese + doctore, french + group, border + closrue , cold germany rule, 

# Correlations 

x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 20)

# wordis that had the highest correlation incluse, close+schol, case+total , positive + test, 


#After the Announcement

x <- udpipe_annotate(ud_model, x = after$title, doc_id = after$author)
x <- as.data.frame(x)


#Nouns
stats <- subset(x, upos %in% c("NOUN")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "lightblue", 
         main = "Most occurring nouns", xlab = "Freq")
# Plot24: gosh look at the distribution on this thing, all people talk about are coronavirus, covid, people, virus, state, case, death, pandemic  


# ADJECTIVES
stats <- subset(x, upos %in% c("ADJ")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "purple", 
         main = "Most occurring adjectives", xlab = "Freq")

#Plot 25: Most occuring adjectives are, "new, positive, more, coronavirus, total, first, public, free, last, medical , "

#Verbs
stats <- subset(x, upos %in% c("VERB")) 
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "gold", 
         main = "Most occurring Verbs", xlab = "Freq")
#plot26: nothing special 

# FInding Key Words: there are 3 methods to finding keyewords
# Using RAKE
stats <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 25), 30), col = "cadetblue", 
         main = "Keywords identified by RAKE", 
         xlab = "Rake")
# plot 27: honestly people are pretty much talking about the same thing 



## Using a sequence of POS tags (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 40), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")
#plot28: here people are talking about sourth korea, state of emergency, prime minister, public schools, united states 

# Another way to do Co-occurences

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc, 20)
#  my top terms are : case+new (143), Case+total(82), honestly nothing special here 

#Word Networkd

# Nouns and adjectives only 
wordnetwork <- head(cooc, 100)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")

# Plot29: this is the first time im seeing hashtaggin and for countries specifically, people want to hear aboutt their own country news 

# nouns and adjectives that follow each other 
cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)
# new + case, coronavisu(170) + case(94)

wordnetwork <- head(cooc, 200)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")
#plot 30: hotel, company and fear here are connected -> people are becoming fearful of travelling, its directly in the cluster, spain appears also 

#COrrelation
x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y, 20)
# same words as the total set appear here,

#-----------------------BIGRAMS AND TRIGRAMS BY COUNTRY----------------
mycorp <- corpus(reddit_data$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## bg1: the words that appear most often together include, new _cases, corona_virus, coronavirus cases, etc new york, south coria, 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures2

# the 3 words that appear most often together, test postivei coronavirus, new case coronavirus, declares state of emergency 
  
  
#Time Analysis  
before <- reddit_data[1:1721,]
during <- reddit_data[1722:2082,]
after <- reddit_data[2083:5000,]

# BEFORE THE ANNOUNCEMENT: BG2
before$title <- as.character(before$title)

mycorp <- corpus(before$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## the words that appear most often together dont really change from total set, new _cases, corona_virus, coronavirus cases, first case, etc new york, south Korean
#cruise ship is on here, washington state, , 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2
# the 3 words that appear most often together, new cases coronavirus, first case, first confirmed case, tested postivei coronavirus, new case coronavirus, declares state of emergency 
#princess cruise ship, grand princess cruise ship 
# similar analysis nd findings to the POS one tbh 
  
# During THE ANNOUNCEMENT: bg3
during$title <- as.character(during$title)

mycorp <- corpus(during$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## these words on the day of are differed, due coronavirus, tom hanks, positive coronavirus, rita wilson, they are liteally all about tom hanks and rita wilson and online classes

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2
# yeah the 3 words are really similar stuff to bigrams
  
  
  
  
  
# After THE ANNOUNCEMENT: BG4
after$title <- as.character(after$title)

mycorp <- corpus(after$title)
mycorp[10]
mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
mytokens <- tokens_remove(mytokens, stopwords("en"))
mytokens[10]
# Lowercase the tokens
newtokens2 <- tokens_tolower(mytokens)

# Bigram: See what data looks like 
newtokens2[10]

toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
table(head(toks_ngram[[1]], 30))
tail(toks_ngram[[1]], 30)

##Check the Highest appearing words
mydfm <- dfm(toks_ngram)
topfeatures <- topfeatures(mydfm,20 )
topfeatures

## the words that appear most often together are now mostly about new cases, pandemic, state emergency, stay home, new york, prime minister 

#Trigram
toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
table(head(toks_ngram2[[1]], 30))
tail(toks_ngram2[[1]], 30)

##Check the Highest appearing words
mydfm2<- dfm(toks_ngram2)
topfeatures2 <- topfeatures(mydfm2,20 )
topfeatures
topfeatures2
# i would say that after thea announcement, people tend to talk about new cases and new positive tests and live updates. its like they just want to know what is the new number every day
# it also contains words such as conerns, crisis, thats how theyre descriving it now 


names <- c("italy", "china","canada","korea","australia","usa","travel")
i=1

topfeaturelist <- list()
for(word in list(italy,china,canada, korea, australia, usa, travel)){
  mycorp <- corpus(word$title)
  mycorp[10]
  mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
  mytokens <- tokens_remove(mytokens, stopwords("en"))
  mytokens[10]
  # Lowercase the tokens
  newtokens2 <- tokens_tolower(mytokens)
  
  # Bigram: See what data looks like 
  newtokens2[10]
  
  toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
  table(head(toks_ngram[[1]], 30))
  tail(toks_ngram[[1]], 30)
  
  ##Check the Highest appearing words
  mydfm <- dfm(toks_ngram)
  topfeatures <- topfeatures(mydfm,20 )
  topfeaturelist[[names[i]]] <- topfeatures
  i=i+1
}  


names <- c("italy", "china","canada","korea","australia","usa","travel")
i=1
newtopfeaturelist <- list()
for(word in list(italy,china,canada, korea, australia, usa, travel)){
  mycorp <- corpus(word$title)
  mycorp[10]
  mytokens <- tokens(mycorp, remove_punct = TRUE, remove_symbols = TRUE)
  mytokens <- tokens_remove(mytokens, stopwords("en"))
  mytokens[10]
  # Lowercase the tokens
  newtokens2 <- tokens_tolower(mytokens)
  
  
  toks_ngram <- tokens_ngrams(newtokens2, n = 2:4)
  table(head(toks_ngram[[1]], 30))
  tail(toks_ngram[[1]], 30)
  
  ##Check the Highest appearing words
  mydfm <- dfm(toks_ngram)
  topfeatures <- topfeatures(mydfm,20)
  
  #Trigram
  toks_ngram2 <- tokens_ngrams(newtokens2, n = 3:10)
  table(head(toks_ngram2[[1]], 30))
  tail(toks_ngram2[[1]], 30)
  
  ##Check the Highest appearing words
  mydfm2<- dfm(toks_ngram2)
  topfeatures2 <- topfeatures(mydfm2,20 )
  newtopfeaturelist[[names[i]]] <- topfeatures2
  i=i+1
}  

newtopfeaturelist
topfeaturelist

save.image( file = "newtopfeatureslist.rdata" )
save.image(file ="topfeatureslist.rdata")

#---------------------POS Analysis by Time------------------
bef <- reddit_data[1:1721,]
dur <- reddit_data[1722:2082,]
aft <- reddit_data[2083:5000,]
for (split_data in list(bef,dur,aft)){
  italy_split <- filter(split_data, italy>0 | italian>0)
  china_split <- filter(split_data, china>0 | chinese>0)
  korea_split <- filter(split_data, korea > 0)
  canada_split <- filter(split_data, canada>0 | canadian >0)
  australia_split <- filter(split_data, australia >0 | australian >0)
  usa_split <- filter(split_data, america>0 | usa>0 | american > 0)
  travel_split <- filter(split_data, travel > 0)
  tourism_split <- filter(split_data, tourism>0 | tour>0 | tourist>0)
  accomodation_split<-filter(split_data, motel>0 | hotel>0 | airbnb>0)
  flying_split <- filter(split_data, westjet>0 | fli>0 | airport>0 | air>0 | airlin>0)
  cruise_split <- filter(split_data, cruis>0 | boat>0)
  home_split<-filter(split_data, home>0)
  
  for(key_word in list(italy_split,china_split,korea_split,australia_split,canada_split,usa_split,travel_split,tourism_split,accomodation_split,flying_split,cruise_split,home_split)){
    x_split <- udpipe_annotate(ud_model, x = key_word$title, doc_id = key_word$ï..author)
    x_split <- as.data.frame(x_split)
    
    
    #Nouns   
    stats <- subset(x_split, upos %in% c("NOUN")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    barchart(key ~ freq, data = head(stats, 20), col = "lightblue", 
             main = "Most occurring nouns", xlab = "Freq")
    
    
    ## ADJECTIVES
    stats <- subset(x_split, upos %in% c("ADJ")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    barchart(key ~ freq, data = head(stats, 20), col = "purple", 
             main = "Most occurring adjectives", xlab = "Freq")
    
    
    #Verbs
    stats <- subset(x_split, upos %in% c("VERB")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    barchart(key ~ freq, data = head(stats, 20), col = "gold", 
             main = "Most occurring Verbs", xlab = "Freq")
    
    
    ### FInding Key Words: there are 3 methods to finding keyewords
    ## Using RAKE
    stats <- keywords_rake(x = x_split, term = "lemma", group = "doc_id", 
                           relevant = x_split$upos %in% c("NOUN", "ADJ"))
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    barchart(key ~ rake, data = head(subset(stats, freq > 5), 20), col = "cadetblue", 
             main = "Keywords identified by RAKE", 
             xlab = "Rake")
    
    
    ## Using a sequence of POS tags (noun phrases / verb phrases)
    x_split$phrase_tag <- as_phrasemachine(x_split$upos, type = "upos")
    stats <- keywords_phrases(x = x_split$phrase_tag, term = tolower(x_split$token), 
                              pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                              is_regex = TRUE, detailed = FALSE)
    stats <- subset(stats, ngram > 1 & freq > 3)
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    barchart(key ~ freq, data = head(stats, 5), col = "cadetblue", 
             main = "Keywords - simple noun phrases", xlab = "Frequency")
    
    cooc <- cooccurrence(x = subset(x_split, upos %in% c("NOUN", "ADJ")), 
                         term = "lemma", 
                         group = c("doc_id", "paragraph_id", "sentence_id"))
    head(cooc, 20)
    
    #Word Networkd
    # Nouns and adjectives only 
    wordnetwork <- head(cooc, 100)
    wordnetwork <- graph_from_data_frame(wordnetwork)
    ggraph(wordnetwork, layout = "fr") +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Arial Narrow") +
      theme(legend.position = "none") +
      labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")
    
    
    # nouns and adjectives that follow each other 
    cooc <- cooccurrence(x_split$lemma, relevant = x_split$upos %in% c("NOUN", "ADJ"), skipgram = 1)
    head(cooc)
    # new + case, coronavisu(170) + case(94)
      
    wordnetwork <- head(cooc, 50)
    wordnetwork <- graph_from_data_frame(wordnetwork)
    ggraph(wordnetwork, layout = "fr") +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      theme_graph(base_family = "Arial Narrow") +
      labs(title = "Words following one another", subtitle = "Nouns & Adjective")
    
    ### Correlations 
    
    x_split$id <- unique_identifier(x_split, fields = c("sentence_id", "doc_id"))
    dtm <- subset(x_split, upos %in% c("NOUN", "ADJ"))
    dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
    dtm <- document_term_matrix(dtm)
    dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
    termcorrelations <- dtm_cor(dtm)
    if (length(termcorrelations)!=0){
      y_split <- as_cooccurrence(termcorrelations)
      y_split <- subset(y_split, term1 < term2 & abs(cooc) > 0.05)
      y_split <- y_split[order(abs(y_split$cooc), decreasing = TRUE), ]
      print(head(y_split,20))
    }
  }
}