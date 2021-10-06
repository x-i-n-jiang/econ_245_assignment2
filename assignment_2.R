#Ariel Gao
#ECON245M HW2
#Part2
#1 setting up the data
#(a)
library(tidyverse)
airbnb<-as.tibble(read.csv("assign_2.csv"))
#(b)
View(airbnb)
colnames(airbnb)
#(c)
airbnb<-rename(airbnb, neighborhood = neighbourhood)
#2
#a
neighborhoods<-airbnb %>% count(neighborhood)
#b
neighborhoods<-neighborhoods%>%
                filter(complete.cases(neighborhood)) 
neighborhoods<- neighborhoods %>% 
                arrange(desc(n)) %>%
                head(20) 
              
#c
airbnb_top_neighborhoods<- filter(airbnb, neighborhood %in% neighborhoods$neighborhood)

#d
summary_stats_top_neighborhoods<-airbnb_top_neighborhoods %>% 
  group_by(neighborhood)%>%
  summarise(avg_square_feet=mean(square_feet,na.rm=T),avg_price=mean(price,na.rm=T),
            sd_price=sd(price,na.rm =T),max_price=max(price,na.rm = T),
            min_price=min(price,na.rm = T)) %>%
          arrange(desc(avg_square_feet))
#e  
highest_avg_square_ft<-summary_stats_top_neighborhoods$avg_square_feet[1]
#f
second_avg_price<-summary_stats_top_neighborhoods$avg_price[2]
