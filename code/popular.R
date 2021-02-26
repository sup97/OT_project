library(dplyr)
library(stringr)
  
dataset$attention <- dataset$replies_count+
dataset$retweets_count+dataset$likes_count

popular <- dataset %>%
  select(username, date, attention, tweet, year, retweets_count) 

popular <- popular[popular$year>2018,]
popular <- popular[order(popular$retweets_count, decreasing=TRUE),]

View(head(popular, 50))

popular <- popular[order(popular$attention, decreasing=TRUE),]
