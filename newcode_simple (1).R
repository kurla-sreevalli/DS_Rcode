txt = readLines(file.choose())
txt <- read.csv(file.choose(), header = TRUE)

txt <- txt[,3]
View(txt)
str(txt)
length(txt)

# Corpus
library(tm)
x <- iconv(txt, "UTF-8")
x <- Corpus(VectorSource(txt))
inspect(x[1])
?iconv

#Data Cleansing
x1 <- tm_map(x, tolower)
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)
inspect(x1[1])

x1 <- tm_map(x1, removeNumbers)
inspect(x1[1])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])

# Remove URL's from corpus
removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)

x1 <- tm_map(x1, content_transformer(removeURL))
inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)
inspect(x1[1])

#Term document matrix 
# converting unstructured data to structured format using TDM

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

tdm <- as.matrix(tdm)
tdm[90:100, 1:20]
# View(tdm)

# Bar plot
w <- rowSums(tdm)
w

w_sub <- subset(w, w >= 20)
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats in all most all documents
x1 <- tm_map(x1, removeWords, 'phone')
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm



# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))


library(wordcloud2)
w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
windows()
wordcloud2(w1, size=0.3, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.3, shape = 'triangle')


#windows()
#letterCloud(w1, word = "w")


#### emotion mining

install.packages("syuzhet")
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
head(s)

x[4]
get_nrc_sentiment('excellent')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')
