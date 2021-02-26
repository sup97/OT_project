#apply random forest to full data
full_data$year <- as.factor(full_data$year)

test_sample_e <- sample_n(test_sample[test_sample$true_value=="n",], 660)
test_sample_e <- rbind(test_sample_e, sample_n(test_sample[test_sample$true_value=="p",], 660))

test_sample_r <- anti_join(test_sample, test_sample_e)
test_sample_r$label <- "test sample"
test_sample_r$year <- 1000

test_sample_e$label <- "test sample selected"
test_sample_e$year <- 1000

full_data <- select(dataset, year, tweet)
full_data$label <- "full data"
full_data$true_value <- "p" #arbitrarly label true value.

full_data <- rbind(test_sample_e, test_sample_r, full_data)

tweets <- full_data$tweet[full_data$label=="test sample selected"|full_data$year=="2019"|
                            full_data$year=="2018"|full_data$year=="2017"]

docs <- Corpus(VectorSource(tweets))

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- docs[-619]
docs <- docs[-1208]
docs <- docs[-2063]
docs <- docs[-2593]

docs <- docs[-1165] #when using only even sample

docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

#build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 100)

barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words 2019",
        ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$year=="1000"|full_data$year=="2019"|
                     full_data$year=="2018"|full_data$year=="2017",]
match <- match[-c(619,1208,2063,2593),]

dtm$true_value <- match$true_value
dtm$year <- match$year

dtm$true_value <- as.factor(dtm$true_value)

n <- length(full_data[full_data$year=="1000",]$true_value)-4
#colnames(training) <- paste(colnames(training), "_c", sep = "")
#training the classifier

training_set <- dtm[dtm$year=="1000",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))

test_set <- dtm[dtm$year!="1000",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

test_set$true_value <- "p"
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

#Predicting the test set results
pred.rf <- predict(classifier, test_set)

#Making the confusion Matrix
# cm = table(test_set$true_value, pred.rf)
# evaluation(cm)
# evaluation_n(cm)

test_set$predicted <- pred.rf

graph2019_7 <- select(test_set, predicted, year)
write.csv(graph2019_7, file="~/Documents/GitHub/OT_analysis/results/graph2019_7.csv")


#now we do another three years - we do this due to large size of data. it kept crashing my R
tweets <- full_data$tweet[full_data$year=="1000"|full_data$year=="2016"|
                            full_data$year=="2015"|full_data$year=="2014"]

docs <- Corpus(VectorSource(tweets))

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- docs[-619]
docs <- docs[-1208]
docs <- docs[-2063]
docs <- docs[-2593]
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

#build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 100)

barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))

dtm

dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$year=="1000"|full_data$year=="2016"|
                     full_data$year=="2015"|full_data$year=="2014",]
match <- match[-c(619,1208,2063,2593),]

dtm <- as.data.frame(as.matrix(dtm))

dtm$true_value <- match$true_value
dtm$year <- match$year

dtm$true_value <- as.factor(dtm$true_value)

n <- length(full_data[full_data$year=="1000",]$true_value)-4

#training the classifier
training_set <- dtm[dtm$year=="1000",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))

test_set <- dtm[dtm$year!="1000",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

test_set$true_value <- "p"
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

#Predicting the test set results
pred.rf <- predict(classifier, test_set)
test_set$predicted <- pred.rf

graph2016_4 <- select(test_set, predicted, year)
write.csv(graph2016_4, file="~/Documents/GitHub/OT_analysis/results/graph2016_4.csv")

#Last try with 2010-2013
tweets <- full_data$tweet[full_data$year=="1000"|full_data$year=="2013"|
                            full_data$year=="2012"|full_data$year=="2011"|
                            full_data$year=="2010"]

docs <- Corpus(VectorSource(tweets))

toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- docs[-619]
docs <- docs[-1208]
docs <- docs[-2063]
docs <- docs[-2593]
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

#build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

library(wordcloud)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

findFreqTerms(dtm, lowfreq = 100)

barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))

dtm

dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$year=="1000"|full_data$year=="2013"|
                     full_data$year=="2012"|full_data$year=="2011"|
                     full_data$year=="2010",]
match <- match[-c(619,1208,2063,2593),]

dtm <- as.data.frame(as.matrix(dtm))

dtm$true_value <- match$true_value
dtm$year <- match$year

dtm$true_value <- as.factor(dtm$true_value)

n <- length(full_data[full_data$year=="1000",]$true_value)-4
#training the classifier
training_set <- dtm[dtm$year=="1000",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))

test_set <- dtm[dtm$year!="1000",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

test_set$true_value <- "p"
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

#Predicting the test set results
pred.rf <- predict(classifier, test_set)
test_set$predicted <- pred.rf

graph2013_0 <- select(test_set, predicted, year)
write.csv(graph2013_0, file="~/Documents/GitHub/OT_analysis/results/graph2013_0.csv")

graph <- rbind(graph2013_0, graph2016_4, graph2019_7)
table(graph)

graph <- graph %>%
  count(year, predicted) %>%
  group_by(year) %>%
  mutate(ratio = n/sum(n))

ggplot(graph, aes(year, y=n, fill=predicted)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=n), vjust=-0.5,
            position = position_dodge(0.9), size=3) +
  scale_fill_discrete(name="Predicted Value",
                      breaks=c("p", "n"),
                      labels=c("Positive", "Negative")) +
  xlab("Year") + ylab("Number of Tweets") + theme_classic()
```