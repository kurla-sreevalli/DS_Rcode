salarytt <- read.csv(file.choose(),stringsAsFactors = F)
class(salarytt)
str(salarytt)
salarytt$sex <- factor(salarytt$sex)#converting text into factor form
str(salarytt)
table(salarytt$sex)#says no.of spam and ham are present in a data

library(tm)

# Prepare corpuse for the text data 
salarytt_corpous <- Corpus(VectorSource(salarytt$sex))


# Cleaning data (removing unwanted symbols)
corpus_clean<-tm_map(salarytt_corpous,tolower)
corpus_clean<-tm_map(corpus_clean, removeNumbers)
corpus_clean<-tm_map(corpus_clean,removeWords, stopwords())
corpus_clean<-tm_map(corpus_clean,removePunctuation)
corpus_clean<-tm_map(corpus_clean,stripWhitespace)
class(corpus_clean)


# Do not run the plainTextDocument
# corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
as.character(corpus_clean)
# create a document-term sparse matrix
#corpus_clean<-Corpus(VectorSource(corpus_clean))
salarytt_dtm <- DocumentTermMatrix(corpus_clean) 
class(salarytt_dtm)

# if code at 28 shows any error run the code at line 27 first and proceed
as.character(salarytt_dtm)
# creating training and test datasets
salarytt_raw_train <- salarytt[1:10542, ]
salarytt_raw_test  <- salarytt[10543:15060, ]

salarytt_dtm_train <- salarytt_dtm[1:10542, ]
salarytt_dtm_test  <- salarytt_dtm[10543:15060, ]

salarytt_corpus_train <- corpus_clean[1:10542]
salarytt_corpus_test  <- corpus_clean[10543:15060]

# check that the proportion of spam is similar
prop.table(table(salarytt_raw_train$sex))
prop.table(table(salarytt_raw_test$sex))

# indicator features for frequent words
salarytt_dict<-findFreqTerms(salarytt_dtm_train, 5)

salarytt_train <- DocumentTermMatrix(salarytt_corpus_train, list(dictionary = salarytt_dict))
salarytt_test  <- DocumentTermMatrix(salarytt_corpus_test, list(dictionary = salarytt_dict))
salarytt_dict
inspect(salarytt_corpus_train[1:100])
list(salarytt_dict[1:100])

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
salarytt_train <- apply(salarytt_train, MARGIN = 2, convert_counts)
salarytt_test  <- apply(salarytt_test, MARGIN = 2, convert_counts)
View(salarytt_train)
View(salarytt_test)


##  Training a model on the data ----
library(e1071)
salarytt_classifier <- naiveBayes(salarytt_train, salarytt_raw_train$sex)
salarytt_classifier

##  Evaluating model performance ----
salarytt_test_pred <- predict(salarytt_classifier, salarytt_test)

library(gmodels)
CrossTable(salarytt_test_pred, salarytt_raw_test$sex,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# 100% accuracy by crosstable
salarytt_classifier2 <- naiveBayes(salarytt_train, salarytt_raw_train$sex, laplace = 1)
salarytt_test_pred2 <- predict(salarytt_classifier2, salarytt_test)
CrossTable(salarytt_test_pred2, salarytt_raw_test$sex,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
# Accuracy 
mean(salarytt_test_pred2==salarytt_raw_test$sex)
# 100 % 