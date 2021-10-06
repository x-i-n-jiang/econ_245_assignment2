# assignment 2
# Xin Jiang
# Ariel Gao

# pull first

library(tidyverse)
library(dplyr)

# 1.setting up the data
#(a) read data and save as a tibble
airbnb <- read_csv("assign_2.csv")
airbnb <- tibble(airbnb)

# (b) view data and column names
# View(airbnb)
colnames(airbnb)

# (c) rename a column name and save
airbnb<-
  airbnb %>%
  rename(c("neighborhood"="neighbourhood"))

# check colnames(airbnb)

# 2. piping practice
#(a) count the number of occurrences each neighborhood 
neighborhoods <-
  airbnb %>% count(neighborhood, sort=T)

#(b) get rid of any NA rows
neighborhoods <-
  neighborhoods %>% filter(!is.na(neighborhood))

# get the top 20 most frequently listed neighborhoods in descending order
neighborhoods <-
  neighborhoods %>% arrange(desc(n)) %>% head(20)

#(c) select airbnb data whose neighborhood is in the neighborhoods tibble
airbnb_top_neighborhoods <-
  airbnb %>% filter(neighborhood %in% neighborhoods$neighborhood)

#(d) create summary statistics
summary_stats_top_neighborhoods <-
  airbnb_top_neighborhoods %>% group_by(neighborhood) %>%
  summarize(avg_square_feet = mean(square_feet, na.rm = T),
            avg_price = mean(price, na.rm = T),
            sd_price = sd(price, na.rm = T),
            max_price = max(price, na.rm = T),
            min_price = min(price, na.rm = T))

# avg_square_feet is in descending order
summary_stats_top_neighborhoods <- 
  summary_stats_top_neighborhoods %>% arrange(desc(avg_square_feet))

# (e) save the highest average square feet
highest_avg_square_ft<-summary_stats_top_neighborhoods[1,2]

# (f) save the second highest average price
summary_stats_top_neighborhoods<-summary_stats_top_neighborhoods %>% 
  arrange(desc(avg_price))
second_avg_price<-summary_stats_top_neighborhoods[2,3]
