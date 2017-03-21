tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
corpus <- tm_map(corpus, tolower)
corpus[[1]]
corpus[[1]]$content
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content
stopwords("english")[1:10]
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content
frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)
sparse <- removeSparseTerms(frequencies, 0.995)
sparse
tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse)= make.names(colnames(tweetsSparse))
tweetsSparse$Negative <- tweets$Negative
library(caTools)
set.seed(123)
split <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainsparse <- subset(tweetsSparse, split == TRUE)
testsparse <- subset(tweetsSparse, split == FALSE)
library(rpart)
library(rpart.plot)
tweetCART <- rpart(Negative ~ ., data = trainsparse, method = "class")
prp(tweetCART)
predictCART <- predict(tweetCART, newdata = testsparse, type = "class")
table(testsparse$Negative, predictCART)
(294+18)/(294+6+37+18)
table(testsparse$Negative)
300/(300+55)
library(randomForest)
set.seed(123)
tweetRF <- randomForest(Negative ~ ., data = trainsparse)
predictRF <- predict(tweetRF, newdata = testsparse)
table(testsparse$Negative, predictRF)
(293+21)/(293+7+34+21)
