#Importing Libraries
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(teamcolors)
library(ggplot2)

#read the data
data <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/games_data/regular_season/reg_games_2019.csv"))

#Add a new column which combines the away and home team scores on a single matchday
data$pts_scored=rowSums(cbind(data$home_score,data$away_score))
#Average points scored in the 2019 season
mean(data$pts_scored)

#Games where less than 20 points were scored
less_than<- data[which(data$pts_scored < 20),]
#Games where more than 50 points were scored
more_than<- data[which(data$pts_scored > 50),]
#games in which the highest number of points were scored
fifty<-data[which(data$pts_scored > 40 & data$pts_scored <= 50),]

###histogram of number of points scored in the NFL 2019 season.
h <- hist(data$pts_scored, breaks = "FD", plot = FALSE)
gg1<- ggplot(data, aes(x = data$pts_scored)) + geom_histogram(breaks = h$breaks, color="red", fill = "firebrick") 
gg1<- gg1+ geom_vline(aes(xintercept=mean(data$pts_scored)),color="blue", linetype="dashed", size=1)
gg1+ labs(title="Total Points scored", subtitle="in the 2019 NFL Season", y="Frequency", x="Points Scored") 



### Histogram of touchdowns throughout the game
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))
tdn<- pbp[which(pbp$touchdown == 1),]
tdn<-tdn$game_seconds_remaining/60
h <- hist(tdn$game_seconds_remaining/60, breaks = "FD", plot = FALSE)
gg2<- ggplot(tdn, aes(x = tdn$game_seconds_remaining/60)) + geom_histogram(breaks = h$breaks, color="red", fill = "firebrick") 
gg2<- gg2+ geom_vline(aes(xintercept=mean(tdn$game_seconds_remaining/60)),color="blue", linetype="dashed", size=1)
gg2+ labs(title="Touchdown Time", y="Touchdowns", x="Time Remaining in Minutes") 




