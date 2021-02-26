#retweet pattern

library(rtweet)
library(ggplot2)
library(dplyr)

dee <- get_retweets("1201521831171280896", n = 100) %>%
  mutate(label = "Dee")
misha <- get_retweets("1177267009366552577", n = 100) %>%
  mutate(label = "Misha")
kate <- get_retweets("1162941235595636737", n = 100) %>%
  mutate(label = "Kate")
lumos <- get_retweets("1187358708159975424", n = 100) %>%
  mutate(label = "Lumos")
sharp <- get_retweets("1168976430585262080", n = 100) %>%
  mutate(label = "Sharp")

graph <- rbind(lumos, misha, dee, kate, sharp)
graph <- select(graph, created_at, screen_name, text, 
                name, location, description, label)

graph_m <- graph %>%
  group_by(label) %>%
  count(created_at, text) %>%
  mutate(cn = cumsum(n))

jpeg('~/Documents/GitHub/OT_analysis/results/retweet.jpg', width = 1000, height = 800, res=200)
ggplot(graph_m, aes(x=created_at, y=cn, color=label, group=label)) +
  geom_point(stat="identity") +
  xlab("Date") + ylab("Number of Retweets") + 
  theme_classic() +
  theme(legend.title = element_blank())
dev.off()

#network of the retweeters
network <- read.csv("~/Documents/GitHub/OT_analysis/data/full_network.csv")
Rnetwork <- network[network$Source=="lumos" 
                    | network$Source=="stcampbell27"
                    | network$Source=="thesharpedge1"
                    | network$Source=="katevandoore"
                    | network$Source=="mishacollins" 
                    | network$Target=="lumos" 
                    | network$Target=="stcampbell27"
                    | network$Target=="thesharpedge1"
                    | network$Target=="katevandoore"
                    | network$Target=="mishacollins",]
IDlist <- c(as.character(Rnetwork$Source), 
            as.character(Rnetwork$Target)) %>%
  unique


retweeters <- function(user) {
  rt <- user$screen_name
  pf<- lookup_users(rt)
  select(pf, screen_name, name, 
                     location, description, followers_count, 
                     friends_count, statuses_count, 
                     favourites_count, verified)
}

misha_re <- retweeters(misha)
misha_re$label <- "Misha"
lumos_re <- retweeters(lumos)
lumos_re$label <- "Lumos"
kate_re <- retweeters(kate)
kate_re$label <- "Kate"
dee_re <- retweeters(dee)
dee_re$label <- "Dee"
sharp_re <- retweeters(sharp)
sharp_re$label <- "Sharp"

Rgraph <- rbind(misha_re, dee_re, 
                sharp_re, lumos_re, kate_re)

Rgraph <- Rgraph %>%
  group_by(label) %>%
  mutate(fn = sum(followers_count)/1000)

Rgraph <- Rgraph %>%
  group_by(label) %>%
  mutate(frn = sum(friends_count)/1000)

Rgraph <- Rgraph %>%
  group_by(label) %>%
  mutate(sn = sum(statuses_count)/1000)

Rgraph$retweet <- 0
Rgraph$retweet[Rgraph$label=="Lumos"] <- 53
Rgraph$retweet[Rgraph$label=="Misha"] <- 408
Rgraph$retweet[Rgraph$label=="Dee"] <- 319
Rgraph$retweet[Rgraph$label=="Kate"] <- 21
Rgraph$retweet[Rgraph$label=="Sharp"] <- 882

g <- select(Rgraph, label, fn, frn, sn, retweet) %>%
  distinct()

options(scipen=999)

a <- ggplot(g, aes(x=label, y=fn, fill=label)) +
  geom_bar(stat="identity") +
  xlab("User") + ylab("Number of Followers (1,000)") + 
  theme_classic() +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("red", "red", "blue", "red", "blue"))

b <- ggplot(g, aes(x=label, y=frn, fill=label)) +
  geom_bar(stat="identity") +
  xlab("User") + ylab("Number of Friends (1,000)") + 
  theme_classic() +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("red", "red", "blue", "red", "blue"))


c <- ggplot(g, aes(x=label, y=sn, fill=label)) +
  geom_bar(stat="identity") +
  xlab("User") + ylab("Number of Statuses (1,000)") + 
  theme_classic() +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("red", "red", "blue", "red", "blue"))

d <- ggplot(g, aes(x=label, y=retweet, fill=label)) +
  geom_bar(stat="identity") +
  xlab("User") + ylab("Number of Retweets") + 
  theme_classic() +
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("red", "red", "blue", "red", "blue"))

multiplot(a, b, c, d, cols=2)

# graph$screen_name <- tolower(graph$screen_name)
# m <- graph[graph$label=="Lumos",]
# colnames(m)[2] <- "Source"
# m <- c(m$Source)
#   
# lumos <- network[m %in% network$Source 
#                 | m %in% Rnetwork$Target,]
write.csv(Rnetwork, 
          "~/Documents/GitHub/OT_analysis/data/retweet_network.csv",
          row.names = FALSE)

#correlation test
retweet <- select(dataset, username, 
                  retweets_count, replies_count, 
                  likes_count, year)

RtId <- unique(retweet[retweet$year==2019,]$username)
RtProfile <- lookup_users(RtId)
RtProfile <-  select(RtProfile, screen_name, name, 
         followers_count, 
         friends_count, statuses_count, 
         favourites_count)

colnames(RtProfile)[1] <- "username"
RtProfile$username <- tolower(RtProfile$username)
retweet <- full_join(retweet, RtProfile, by="username")
retweet <- retweet[!is.na(retweet$followers_count),] %>%
  filter(year==2019) %>%
  distinct()

retweet <- retweet %>%
  group_by(username) %>%
  mutate(retweets_count = sum(retweets_count)) 

fit <- lm(retweets_count~followers_count+
      friends_count+statuses_count, data=retweet)
summary(fit)

cor.test(retweet$followers_count, retweet$retweets_count, method=c("pearson", "kendall", "spearman"))
cor.test(retweet$friends_count, retweet$retweets_count, method=c("pearson", "kendall", "spearman"))
cor.test(retweet$statuses_count, retweet$retweets_count, method=c("pearson", "kendall", "spearman"))

graph <- retweet %>%
  filter(followers_count<1000000) %>%
  filter(retweets_count<750)

ggscatter(graph, x = "statuses_count", y = "retweets_count", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Friends", ylab = "Number of Retweets")

install.packages("ggpubr")
library("ggpubr")

cor.test(g$fn, g$retweet, method=c("pearson", "kendall", "spearman"))
cor.test(g$fvn, g$retweet, method=c("pearson", "kendall", "spearman"))
cor.test(g$sn, g$retweet, method=c("pearson", "kendall", "spearman"))

cor.test(g$frn, g$retweet, method=c("pearson", "kendall", "spearman"))

ggscatter(g, x = "frn", y = "retweet", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of Friends", ylab = "Number of Retweets")
## map locations of the retweeters
library(ggmap)
library(tidyverse)

userID <- c(graph$screen_name)

register_google(key="AIzaSyD3w87pFMI4mDaBHg9znJjgbOW2bOSr2C0")
ggmap_show_api_key()
has_google_key()

userProfile <- lookup_users(userID)
glimpse(userProfile)

userProfile <- userProfile[userProfile$location!="",]

coded <- userProfile$location %>%
  ggmap::geocode()

userProfile <- cbind(userProfile, coded)
location <- select(userProfile, screen_name, lon, lat)

location <- merge(location, graph, by="screen_name")

library(rworldmap)

world <- getMap(resolution = "low")
plot(world)
points(location$lon, location$lat, col = "red", cex = .6)