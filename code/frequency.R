cleaned_dataset <- dataset[dataset$year==2010 & dataset$username!="abroaderview",]

replace_reg <- " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

keywords <- c("orphanage","children's", "children", "child", "kid", 
              "kids", "orphan", "boy's", "boy", "girl's", "girl", 
              "volunteer", "travel", "travels", "tourism", "twitter", 
              "www", "orphanages", "home", "volunteers", "ly", "volunteering", 
              "org", "au","utm_source", "p3zf4y", "iq", "christina", "baker", 
              "kline", "orphan train", "http", "https", "mission", 
              "missionary", "missionaries","can", "child", "via", "abroad")

tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
       remove_numbers=TRUE, remove_symbols=TRUE,
       remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

b <- ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2010")


#overall
cleaned_dataset <- dataset[dataset$username!="abroaderview",]
tweet <- tokens(dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

a <- ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram all years")

cleaned_dataset <- dataset[dataset$year==2019 & dataset$username!="abroaderview",]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

d <- ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("Frequency") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2019")

cleaned_dataset <- dataset[dataset$year==2014 & dataset$username!="abroaderview",]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

c <- ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2015")

multiplot(a, b, c, d, cols=2)

cleaned_dataset <- dataset[dataset$year==2016 & dataset$username!="abroaderview",]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2016")

cleaned_dataset <- dataset[dataset$year==2017 & dataset$username!="abroaderview",]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2017")

cleaned_dataset <- dataset[dataset$year==2018 & dataset$username!="abroaderview",]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2018")

sent_data <- rbind(graph2019_7, graph2016_4, graph2013_0)
dataset$sentiment <- sent_data$predicted

cleaned_dataset <- dataset[dataset$sentiment==n,]
tweet <- tokens(cleaned_dataset$tweet, remove_puct=TRUE,
                remove_numbers=TRUE, remove_symbols=TRUE,
                remove_url=TRUE) %>%
  tokens_remove("\\p{P}", valuetype = "regex", padding = TRUE) %>%
  tokens_remove(stopwords("english"), padding  = TRUE) %>%
  tokens_remove(keywords, padding  = TRUE) %>%
  tokens_ngrams(n = 2) %>%
  dfm()

freq <- tbl_df(textstat_frequency(tweet)) %>%
  filter(nchar(feature)>2)

freq_t <- tbl_df(freq) %>%
  group_by(feature) %>%
  tally(frequency) %>%
  top_n(20)

ggplot(freq_t, aes(x=reorder(feature,n), n)) +
  geom_col() +
  xlab("Word") +
  ylab("") +
  coord_flip() + theme_classic() +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16),
        axis.text=element_text(size=14)) +
  ggtitle("Top 20 bi-gram 2018")
