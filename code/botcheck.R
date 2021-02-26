## select users
network <- read.csv("~/Documents/GitHub/OT_analysis/data/full_network.csv")
userID <- c(as.character(network$user), as.character(network$follower)) %>%
  unique

library(rtweet)
library(ggmap)
library(tidyverse)

userProfile <- lookup_users(userID)
glimpse(userProfile)

register_google(key="AIzaSyD3w87pFMI4mDaBHg9znJjgbOW2bOSr2C0")
ggmap_show_api_key()
has_google_key()

userProfile <- userProfile[userProfile$location!="",]

coded <- userProfile$location %>%
  ggmap::geocode()

userProfile <- cbind(userProfile, coded)

save(userProfile, file="~/Documents/GitHub/OT_analysis/data/user_profiel.RData")

list.of.packages <- c("maps","mapdata","maptools","rgdal","ggmap","leaflet","tigris",
                      "sp","ggplot2","plyr","animation","gridExtra","psych","rstudioapi",
                      "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(rworldmap)

world <- getMap(resolution = "low")
plot(world)
points(userProfile$lon, userProfile$lat, col = "red", cex = .6)

library(usmap)
library(ggplot2)

plot_usmap(regions = "states") +
  theme(panel.background = element_rect(color="black", fill="white"))
