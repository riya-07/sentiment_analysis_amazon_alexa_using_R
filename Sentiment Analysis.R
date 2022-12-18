# Amazon Alexa Reviews

library(tm) 
library(wordcloud)
library(syuzhet)
library(RColorBrewer)

# importing data to R
reviews <- read.csv("amazon_alexa.csv")

# Checking the structure of file
str(reviews)

is.na(reviews)

summary(reviews)

# This function uses the base package function iconv to translate labels into a specific encoding
corpus <- iconv(reviews$verified_reviews)
corpus <- Corpus(VectorSource(corpus))

# To see the corpus
inspect(corpus[1:5])

# cleaning corpus
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

inspect(corpus[1:5])

reviews_final <- corpus

# Create term document
tdm <- TermDocumentMatrix(reviews_final)
tdm <- as.matrix(tdm)
tdm[1:10, 1:5]

# Bar plot of words
w <- rowSums(tdm)
w <- subset(w, w >= 25)
barplot(w, las = 2, col = "blue")

# Word Cloud
w <- sort(rowSums(tdm), decreasing = T)
set.seed(2000)
wordcloud(words = names(w), freq = w, max.words = 100, random.order = T, min.freq = 5, colors = brewer.pal(8,"Dark2"), scale = c(3, 0.5))

# Obtaining sentiment scores
sentiment_data <- iconv(reviews$verified_reviews)
s <- get_nrc_sentiment(sentiment_data) # gives us scores based on few sentiments
s[1:10,]

# calculate review wise score
s$score <- s$positive - s$negative
s[1:10,]

# checking product sentiment
# check overall sentiment of the product
review_score <- colSums(s[,])
print(review_score)

# Bar Plot for product sentiment
barplot(colSums(s), las=2, col=rainbow(10),ylab='Count',main='Sentiment')

# Sentiment Analysis
pos.words <- scan(file='positive-words.txt', what='character')
neg.words <- scan(file = 'negative-words.txt', what='character')

