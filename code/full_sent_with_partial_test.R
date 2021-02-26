#apply random forest to full data
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
full_data$year <- as.factor(full_data$year)

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
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)
# 
# library(wordcloud)
# wordcloud(words = d$word, freq = d$freq, min.freq = 100,
#           max.words=200, random.order=FALSE,
#           colors=brewer.pal(8, "Dark2"))
# 
# findFreqTerms(dtm, lowfreq = 100)
# 
# barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
#         col ="lightblue", main ="Most frequent words 2019",
#         ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))
dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$label=="test sample selected"|full_data$year=="2019"|
                     full_data$year=="2018"|full_data$year=="2017",]
match <- match[-1165,]
match$true_value <- ifelse(grepl("orphanage tourism", match$tweet), 
                           "n", match$true_value)
match$label <- ifelse(grepl("orphanage tourism", match$tweet), 
                      "test sample selected", match$label)

table(match$label) 

dtm <- as.data.frame(as.matrix(dtm))
dtm$true_value <- match$true_value
dtm$year <- match$year
dtm$label <- match$label

dtm$true_value <- as.factor(dtm$true_value)

#training the classifier

training_set <- dtm[dtm$label=="test sample selected",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))
n <- length(training_set)

test_set <- dtm[dtm$label!="test sample selected",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

#Predicting the test set results
pred.rf <- predict(classifier, test_set)

#Making the confusion Matrix
# cm = table(test_set$true_value, pred.rf)
# evaluation(cm)
# evaluation_n(cm)

test_set$predicted <- pred.rf
add <- training_set[training_set$year!="1000" & 
                      training_set$label=="test sample selected",]
add$predicted <- "n"
test_set <- rbind(test_set, add)
graph2019_7 <- select(test_set, predicted, year)
table(graph2019_7)

write.csv(graph2019_7, file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2019_7.csv")

#now we do another three years - we do this due to large size of data. it kept crashing my R
rm(add, classifier, tweets, docs, dtm, match, test_set, training_set)
tweets <- full_data$tweet[full_data$label=="test sample selected"|full_data$year=="2016"|
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
docs <- docs[-1165]

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
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)
# 
# library(wordcloud)
# wordcloud(words = d$word, freq = d$freq, min.freq = 100,
#           max.words=200, random.order=FALSE,
#           colors=brewer.pal(8, "Dark2"))
# 
# findFreqTerms(dtm, lowfreq = 100)
# 
# barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
#         col ="lightblue", main ="Most frequent words",
#         ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))

dtm

dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$label=="test sample selected"|full_data$year=="2016"|
                     full_data$year=="2015"|full_data$year=="2014",]
match <- match[-1165,]
match$true_value <- ifelse(grepl("orphanage tourism", match$tweet), 
                           "n", match$true_value)
match$label <- ifelse(grepl("orphanage tourism", match$tweet), 
                      "test sample selected", match$label)

dtm <- as.data.frame(as.matrix(dtm))

dtm$true_value <- match$true_value
dtm$year <- match$year
dtm$label <- match$label

dtm$true_value <- as.factor(dtm$true_value)

#training the classifier
training_set <- dtm[dtm$label=="test sample selected",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))
n <- length(training_set)

test_set <- dtm[dtm$label!="test sample selected",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

#Predicting the test set results
pred.rf <- predict(classifier, test_set)
test_set$predicted <- pred.rf
add <- training_set[training_set$year!="1000" & 
                      training_set$label=="test sample selected",]
add$predicted <- "n"
test_set <- rbind(test_set, add)

graph2016_4 <- select(test_set, predicted, year)
table(graph2016_4)
write.csv(graph2016_4, file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2016_4.csv")

#Last try with 2010-2013
rm(add, classifier, tweets, docs, dtm, match, test_set, training_set)
tweets <- full_data$tweet[full_data$label=="test sample selected"|full_data$year=="2013"|
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
docs <- docs[-1165]

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
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)
# 
# library(wordcloud)
# wordcloud(words = d$word, freq = d$freq, min.freq = 100,
#           max.words=200, random.order=FALSE,
#           colors=brewer.pal(8, "Dark2"))
# 
# findFreqTerms(dtm, lowfreq = 100)
# 
# barplot(d[1:20,]$freq, names.arg = d[1:20,]$word,
#         col ="lightblue", main ="Most frequent words",
#         ylab = "Word frequencies", horiz=TRUE)

# Creating the bag of word model
dtm <- DocumentTermMatrix(docs, control = list(weighting = weightTfIdf))

dtm

dtm <- removeSparseTerms(dtm, 0.999)

dtm
dim(dtm)

match <- full_data[full_data$label=="test sample selected"|full_data$year=="2013"|
                     full_data$year=="2012"|full_data$year=="2011"|
                     full_data$year=="2010",]
match <- match[-1165,]
match$true_value <- ifelse(grepl("orphanage tourism", match$tweet), 
                           "n", match$true_value)
match$label <- ifelse(grepl("orphanage tourism", match$tweet), 
                      "test sample selected", match$label)

dtm <- as.data.frame(as.matrix(dtm))

dtm$true_value <- match$true_value
dtm$year <- match$year
dtm$label <- match$label

dtm$true_value <- as.factor(dtm$true_value)

#training the classifier
training_set <- dtm[dtm$label=="test sample selected",]
training_set$true_value <- factor(training_set$true_value, levels = c("p", "n"))
n <- length(training_set)

test_set <- dtm[dtm$label!="test sample selected",]
test_set$true_value <- factor(test_set$true_value, levels = c("p", "n"))

classifier <- randomForest(x = training_set[-n],
                           y = training_set$true_value)

#Predicting the test set results
pred.rf <- predict(classifier, test_set)
test_set$predicted <- pred.rf
add <- training_set[training_set$year!="1000" & 
                      training_set$label=="test sample selected",]
add$predicted <- "n"
test_set <- rbind(test_set, add)

graph2013_0 <- select(test_set, predicted, year)
table(graph2013_0)
write.csv(graph2013_0, file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2013_0.csv")

graph2013_0 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2013_0.csv")
graph2016_4 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2016_4.csv")
graph2019_7 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2019_7.csv")

graph <- rbind(graph2013_0, graph2016_4, graph2019_7)
table(graph)

graph <- graph %>%
  group_by(year) %>%
  count(predicted) %>%
  mutate(ratio = n/sum(n))

graph_n <- graph[graph$predicted=="n",]

graph$year <- as.factor(graph$year)

p1 <- ggplot(graph, aes(year, y=n, fill=predicted)) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label=n), position = position_stack(vjust = 0.5), size=3) +
  scale_fill_discrete(name="Predicted Value",
                      breaks=c("p", "n"),
                      labels=c("Positive", "Negative")) +
  xlab("Year") + ylab("Number of Tweets") + theme_classic() +
  theme(legend.position = "bottom")

p2 <- ggplot(graph_n, aes(year, y=ratio*100, group=1)) +
  geom_line(size=1, color="black") +
  geom_label(aes(label=paste0(round(ratio*100,2), "%"))) +
  xlab("Year") + ylab("Percentage of negative tweets (%)") + theme_classic() +
  theme(legend.position = "bottom")

multiplot(p1, p2, cols=1)


