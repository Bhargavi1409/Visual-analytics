#covid dataset 

#install.packages('hrbrthemes')
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(hrbrthemes)
require(ggrepel)

#load dataset
covid <- read.csv(file="covid_au_state.csv")

#combining the confirmed cases by date and ordered by date
n <- aggregate(covid["confirmed"], by=covid["date"], sum)
new <- n[order(as.Date(n$date, format = "%d/%m/%y")),]
row.names(new)<- NULL
new$datee <- as.Date(new$date,"%d/%m/%y")



top <- new[ new$confirmed >= new$confirmed[order(new$confirmed, decreasing=TRUE)][3] , ]


#PART 1

p1<- new %>% 
  ggplot(aes(x=datee,y= confirmed)) + geom_line(color = "steelblue")+scale_x_date(date_labels="%b%d",date_breaks  ="1 week")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+geom_text_repel(data = top, aes(label = as.character(datee)), size = 3)+ ggtitle(" Daily Confirmed cases")+geom_point(data= top, aes(x=datee,y=confirmed), color='red',size=2)

# PART 2

covid <- read.csv(file="covid_au_state.csv")
covid$date <- as.Date(covid$date,"%d/%m/%y")


#ordering the dataframe

str(covid)
covid<-with(covid, covid[order(state, date),])
row.names(covid)<- NULL


#calculating growth factor

growth = covid %>% 
  group_by(state) %>% 
  mutate(growth_fac = confirmed/lag(confirmed))

#replace inf with 0 

growth[sapply(growth, is.infinite)] <- 0

#Removing NA values 
growth[sapply(growth, is.na)] <- 0
#growth<-na.omit(growth)

str(growth)

p2<-ggplot(growth, aes(x=date, y=growth_fac,color=state)) +
  geom_line() +ggtitle("Growth factor across 8 states ")+ 
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(limit=c(as.Date("2020-03-17"),as.Date("2020-08-16")),date_labels = "%b%d", date_breaks = "2 week")

p2+facet_wrap(~state,scales = "free_y")

