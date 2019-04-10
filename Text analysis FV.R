install.packages('dplyr')
install.packages('readr')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('tidytext')
install.packages('SnowballC')
install.packages('stringr')
install.packages('rtweet')
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(SnowballC)
library(stringr)
library(rtweet)

install.packages('udpipe')
library(udpipe)

install.packages('tidyr')
library(tidyr)


# import times_up data set
library(readxl)
times_up <- read_excel("times up/data/merged times up.xlsx")
View(times_up)

# import metoo data set
library(readxl)
master_metoo <- read_excel("me too/merge/master metoo.xlsx")
View(master_metoo)

# Table tweets over time timesup
times_up %>% group_by(published_date) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  distinct() %>% head(363) %>%
  ggplot(aes(x = published_date, y = count)) + geom_bar(stat= "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Table tweets over time metoo
master_metoo %>% group_by(published_date) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  distinct() %>% head(430) %>%
  ggplot(aes(x = published_date, y = count)) + geom_bar(stat= "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# line graph tweets over time metoo
master_metoo %>% group_by(published_date) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  distinct() %>% head(430) %>%
  ggplot(aes(x = published_date, y = count, 
             col= "violetred")) +
  geom_line(stat= "identity") + 
  geom_area(aes(fill="violetred")) +
  #axis(y, seq(01-10-2018, 01-01-2018, 02-06-2018, 01-01-2019)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_classic() +
  labs(title = "#MeToo growth over time",
      x = "1st Oct 2017 to 1st Jan 2019",
      y = "Tweets over time",
      font = 2)

# line graph tweets over time timesup
times_up %>% group_by(published_date) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>%
  distinct() %>% head(363) %>%
  ggplot(aes(x = published_date, y = count, 
             col= "dark purple")) +
  geom_area(stat= "identity") + 
 # geom_line(aes(fill="dark purple")) +
  theme(axis.text.x = element_text(xlim(01-01-2018, 01-01-2019), angle = 45, hjust = 1)) +
  theme_classic() +
  labs(title = "#Timesup growth over time",
       x = "1st Jan 2018 to 1st Jan 2019",
       y = "Tweets over time",
       font = 2)






  
  
  
  
  
  
  