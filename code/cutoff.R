dataset_ot <- filter(dataset, grepl("orphanage tourism", tweet))
uniqueid <- unique(dataset_ot$username)
length(uniqueid)/length(unique(dataset$username))

table(dataset_ot$year)

graph <- dataset_ot %>%
  select(year, username) %>%
  distinct()

graph <- graph[order(graph$username, graph$year),]

graph <- graph %>%
  group_by(username) %>%
  arrange(order(year)) %>% 
  slice(1) %>%
  ungroup()

graph[870,]

View(filter(dataset_ot, grepl("globltrvlreport", username)))

graph <- graph %>%
  group_by(year) %>%
  count()

ggplot(graph, aes(as.factor(year), y=n)) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_line(aes(y=cumsum(n), group=1, 
                linetype="Cumulative Number")) +
  geom_hline(yintercept = 870, 
             linetype="dashed", 
             color="red")+
  geom_text(aes(label=n), vjust=-0.5,
            position = position_dodge(0.9), size=4) +
  annotate(geom="label", x="2019", y=2462, label="2462",
           color="red") +
  xlab("Year") + ylab("Number of Users") + 
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=15))

##Add people who have negative sentiments
graph2013_0 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2013_0.csv")
graph2016_4 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2016_4.csv")
graph2019_7 <- read.csv(file="~/Documents/GitHub/OT_analysis/results/sentiment/graph2019_7.csv")
#dataset_ot <- filter(dataset, grepl("orphanage tourism", tweet))
#dataset_ot <- rbind(dataset_ot, filter(dataset, grepl("voluntourism", tweet)))
#dataset_ot <- rbind(dataset_ot, filter(dataset, grepl("volunteer tourism", tweet)))

dataset_n <- rbind(graph2019_7, graph2016_4, graph2013_0)

dataset$sentiment <- dataset_n$predicted
dataset$sentiment1 <- dataset$sentiment
dataset[grep("orphanage tourism", dataset$tweet),]$sentiment1 <- "n"

dataset_n <- dataset[dataset$sentiment=="n",]
dataset_n1 <- dataset[dataset$sentiment1=="n",]
#dataset_n <- rbind(dataset_n, dataset_ot) %>%
#  distinct()

summary(unique(dataset_n$username))

graph_n <- dataset_n %>%
  select(year, username) %>%
  distinct()

graph_n1 <- dataset_n1 %>%
  select(year, username) %>%
  distinct()

graph <- dataset %>%
  select(year, username) %>%
  distinct()

#graph_n[870,]
#View(filter(dataset_n, grepl("arysaparker", username)))

#graph_n[5570,]
#View(filter(dataset_n, grepl("lorihandrahan2", username)))

graph_n <- graph_n %>%
  group_by(year) %>%
  count()

graph_n1 <- graph_n1 %>%
  group_by(year) %>%
  count()

graph <- graph %>%
  group_by(year) %>%
  count()

colnames(graph)[2] <- "total"
graph_n <- inner_join(graph_n, graph)
graph_n$Total <- 34370
graph_n$p <- round(graph_n$n/graph_n$total*100,2)
graph_n$cum <- round(cumsum(graph_n$n)/34370*100,2)
graph_n$cumsum <- cumsum(graph_n$n)

ggplot(graph_n, aes(as.factor(year), y=p)) +
  geom_bar(stat="identity", 
           position=position_dodge()) +
  geom_text(aes(label=p), vjust=-0.5,
            position = position_dodge(0.9), size=4) +
  xlab("Year") + ylab("Percentage of Users") + 
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size=15))

id <- select(dataset, username, year, sentiment) %>%
  distinct()

id <- id[id$sentiment=="p",]

sum(table(id$username)-1)
summary(unique(id$username))

719/10266
2673/26968

write.csv(id, file="~/Downloads/id.csv")
