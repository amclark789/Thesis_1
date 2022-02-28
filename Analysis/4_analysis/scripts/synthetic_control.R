


library(pacman)


p_load(tidyverse, Synth, lubridate)




load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/stonks/stonks_ALL_Feb_24.RData")

stonks = stonks_ALL_Feb_24


stonks = stonks %>%
  mutate(return = (close/open) - 1) %>%
  mutate( date_time = paste(date, time, sep = " ")) %>%
  mutate(date_time = mdy_hm(date_time) ) %>%
  mutate(date_time = as.numeric(as.POSIXct(date_time, tz = "EST"))) %>%
  mutate(date_time = date_time + 18000)

stonks = stonks %>%
  select(-c(low, high)) %>%
  mutate(return = ifelse(is.na(return), 0, return)) %>%
  mutate(date_time = date_time - (date_time%%86400))

stonks1 = stonks %>%
  mutate(date_time_symbol = paste(date_time, symbol, sep = " "))
  
stonks1 = stonks1 %>%
  mutate(return = return + 1) %>%
  group_by(date_time_symbol) %>%
  summarize(volume = sum(volume), vol = sd(return), return = prod(return) ) %>%
  mutate(return = return-1) %>%
  separate(date_time_symbol, into = c("date_time", "symbol"), sep = " ")






