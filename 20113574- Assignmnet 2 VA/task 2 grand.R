library(plyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)
library(ggrepel)



#Load dataset
grand <- read.csv("grand_slam_data.csv")
grand[grep("Australian Open", grand$tournament),"tournament"] = "Australian Open"

#Grouping by winner and counting the wins
ss<- grand %>%
  dplyr::group_by(winner) %>%
  dplyr::summarise(NUM_WINS = n())

#ordering by number of wins 
fn<- arrange(ss,desc(NUM_WINS))
fn$winner<- factor(fn$winner, levels = fn$winner[order(fn$NUM_WINS)])
a<-grand%>%filter(winner %in% fn$winner)

#Grouping by tournamnet and winner
sss<- a %>%
  dplyr::group_by(tournament, winner) %>%
  dplyr::summarise(NUM_WINS = n())

#order descending by number of wins
fnn<- arrange(sss,desc(NUM_WINS))
fnn$NUM_WINS<-factor((fnn$NUM_WINS))

unique(fnn$NUM_WINS)
fnn$NUM_WINS<- as.numeric(as.character(fnn$NUM_WINS))

#Selecting top 20 data 
ddd<-as.data.frame(setDT(fnn)[order(tournament,-NUM_WINS), .SD[1:20], by=tournament])


df = ddd %>% 
  group_by(tournament) %>% mutate(NUM_WINS == max(NUM_WINS))
colnames(df) <- c("tournament","winner","NUM_WINS","N")
str(df)
top1<- subset(df,N =="TRUE")

# plot
ggplot(df,aes(x=winner, y=NUM_WINS, fill= N))+
  geom_bar(stat = "identity")+ coord_flip()+
  facet_grid(.~tournament)+ geom_text_repel(data =top1, aes(label = as.character(winner)), size = 3)+ggtitle("Winners of each tournament")+
  theme(legend.position = "none", axis.text.y= element_text(angle = 360, vjust = 0.5, hjust=1,size = 5))


#Part 2 

#filtering data by year and grouping by winner and runner_up columns

mm<-grand %>%
  filter( year>= 2008 & year<=2017) %>%
  group_by(winner,runner_up) %>%
  summarise_each(funs(n())) 

m2 <- merge(x = mm, y = mm, by.x = c("winner","runner_up"), by.y = c("runner_up","winner"), all.x = TRUE)

m2$winner<-as.character(m2$winner)
m2$runner_up<-as.character(m2$runner_up)

#Removing duplicate values

m3 <- m2 %>%
  filter(!duplicated(paste0(pmax(winner, runner_up), pmin(winner, runner_up))))

m3<-subset(m3,select=-c(tournament.x,tournament.y))

colnames(m3)[c(3,4)] <- c("c1", "c2")

m3[is.na(m3)] <- 0
m3$total<-m3$c1+m3$c2

dd <- m3 %>% unite("comb", winner,runner_up, remove = FALSE)

#plot
p <-ggplot(dd, aes(x=total, y=comb,fill = ifelse(total == max(total), "Highlighted", "Normal"))) +
  geom_bar(stat = "identity") + theme(legend.position = "none")+ggtitle("Player meet ups in Tournament ") +
  xlab("Number of meets") + ylab("Players")
p


