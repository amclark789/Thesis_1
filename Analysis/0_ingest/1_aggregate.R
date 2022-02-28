

library(pacman)

p_load(tidyverse, lubridate, RPostgres, SciViews)



load("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_1.RData")

analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(volume = Volume) %>%
  select( -Volume)


analysis_1_df_5 = analysis_1_df_1 %>%
  mutate(return_SPY = (close_SPY - open_SPY) / open_SPY) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(minute = minute - (minute%%300)) %>%
  mutate(return = return + 1) %>%
  mutate(return = log(return, base = 10)) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(return_SPY = log(return_SPY, base = 10)) %>%
  group_by(minute) %>%
  summarize(volume = sum(volume), cashtags_per_min = sum(cashtags_per_min),hashtags_per_min = sum(hashtags_per_min),  coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), return = sum(return), return_SPY = sum(return_SPY), cash_hash_per_min = sum(cash_hash_per_min)) %>%
  mutate(return = 10^return) %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = 10^return) %>%
  mutate(return_SPY = return_SPY - 1)


analysis_1_df_10 = analysis_1_df_1 %>%
  mutate(return_SPY = (close_SPY - open_SPY) / open_SPY) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(minute = minute - (minute%%600)) %>%
  mutate(return = return + 1) %>%
  mutate(return = log(return, base = 10)) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(return_SPY = log(return_SPY, base = 10)) %>%
  group_by(minute) %>%
  summarize(volume = sum(volume), cashtags_per_min = sum(cashtags_per_min),hashtags_per_min = sum(hashtags_per_min),  coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), return = sum(return), return_SPY = sum(return_SPY), cash_hash_per_min = sum(cash_hash_per_min)) %>%
  mutate(return = 10^return) %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = 10^return) %>%
  mutate(return_SPY = return_SPY - 1)

analysis_1_df_60 = analysis_1_df_1 %>%
  mutate(return_SPY = (close_SPY - open_SPY) / open_SPY) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(minute = minute - (minute%%3600)) %>%
  mutate(return = return + 1) %>%
  mutate(return = log(return, base = 10)) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(return_SPY = log(return_SPY, base = 10)) %>%
  group_by(minute) %>%
  summarize(volume = sum(volume), cashtags_per_min = sum(cashtags_per_min),hashtags_per_min = sum(hashtags_per_min),  coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), return = sum(return), return_SPY = sum(return_SPY), cash_hash_per_min = sum(cash_hash_per_min)) %>%
  mutate(return = 10^return) %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = 10^return) %>%
  mutate(return_SPY = return_SPY - 1)

analysis_1_df_daily = analysis_1_df_1 %>%
  mutate(return_SPY = (close_SPY - open_SPY) / open_SPY) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(minute = minute - (minute%%86400)) %>%
  mutate(return = return + 1) %>%
  mutate(return = log(return, base = 10)) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(return_SPY = log(return_SPY, base = 10)) %>%
  group_by(minute) %>%
  summarize(volume = sum(volume), cashtags_per_min = sum(cashtags_per_min),hashtags_per_min = sum(hashtags_per_min),  coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), return = sum(return), return_SPY = sum(return_SPY), cash_hash_per_min = sum(cash_hash_per_min)) %>%
  mutate(return = 10^return) %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = 10^return) %>%
  mutate(return_SPY = return_SPY - 1)

analysis_1_df_1 = analysis_1_df_1 %>% 
  mutate(return_SPY = (close_SPY - open_SPY) / open_SPY) %>%
  select( -c(Open, High, Low, Close, high_SPY, low_SPY,  open_SPY, close_SPY ,date))





### GET FF DATA ###########################################

analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST")) %>%
  separate(date, into = c("date", NA), sep = " ") %>%
  mutate(date = as.Date(date))
analysis_1_df_5 = analysis_1_df_5 %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))%>%
  separate(date, into = c("date", NA), sep = " ")%>%
  mutate(date = as.Date(date))
analysis_1_df_10 = analysis_1_df_10 %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))%>%
  separate(date, into = c("date", NA), sep = " ")%>%
  mutate(date = as.Date(date))
analysis_1_df_60 = analysis_1_df_60 %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))%>%
  separate(date, into = c("date", NA), sep = " ")%>%
  mutate(date = as.Date(date))
analysis_1_df_daily = analysis_1_df_daily %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))%>%
  separate(date, into = c("date", NA), sep = " ")%>%
  mutate(date = as.Date(date))

############################################################################
# 
# drv <- dbDriver("Postgres")
# 
# con <- dbConnect(drv, user='postgres', password='montanaecon', dbname='reddit', host = 'localhost')
# 
# 
# res = dbSendQuery(conn = con, "SELECT * FROM daily_short_data_r3000")
# 
# short_int = dbFetch(res)
# 
# short_int = short_int[,c(1,9,10,13)]
# 
# 
# short_int = short_int %>%
#   mutate(FF = (100*ESI)/ESI_of_FF) %>%
#   separate(date, into = c("day", "month", "year"), sep = "-") %>%
#   mutate(year = as.numeric(year)) %>%
#   arrange(year)
# 
# short_int = short_int[3135123:nrow(short_int), ]
# 
# 
# short_int = short_int %>%
#   mutate(date = paste(year, month, day, sep = "-")) %>%
#   mutate(date =  ymd(date)) %>%
#   mutate(symbol = as.factor(symbol))
# 
# short_int_GME = subset(short_int, symbol =="GME")
# 
# save(short_int, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/short_int.RData")
# save(short_int_GME, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/short_int_GME.RData")

load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/short_int.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/short_int_GME.RData")


analysis_1_df_1 = left_join(analysis_1_df_1, short_int_GME, by = c("date"))
analysis_1_df_5 = left_join(analysis_1_df_5, short_int_GME, by = c("date"))
analysis_1_df_10 = left_join(analysis_1_df_10, short_int_GME, by = c("date"))
analysis_1_df_60= left_join(analysis_1_df_60, short_int_GME, by = c("date"))
analysis_1_df_daily = left_join(analysis_1_df_daily, short_int_GME, by = c("date"))


analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(relative_volume = volume/FF) %>%
  mutate(asinh_rel_vol = asinh(relative_volume))
analysis_1_df_5 = analysis_1_df_5 %>%
  mutate(relative_volume = volume/FF)%>%
  mutate(asinh_rel_vol = asinh(relative_volume))
analysis_1_df_10 = analysis_1_df_10 %>%
  mutate(relative_volume = volume/FF)%>%
  mutate(asinh_rel_vol = asinh(relative_volume))
analysis_1_df_60 = analysis_1_df_60 %>%
  mutate(relative_volume = volume/FF)%>%
  mutate(asinh_rel_vol = asinh(relative_volume))
analysis_1_df_daily = analysis_1_df_daily %>%
  mutate(relative_volume = volume/FF)%>%
  mutate(asinh_rel_vol = asinh(relative_volume))


### CALCULATE MARKET INDEX 

# LOAD STOCK RETURN DATA

# 
# drv <- dbDriver("Postgres")
# 
# con <- dbConnect(drv, user='postgres', password='montanaecon', dbname='reddit', host = 'localhost')
# 
# 
# res = dbSendQuery(conn = con, "SELECT utc, symbol, date, volume FROM stonks_r_3000")
# 
# market_index = dbFetch(res)
# 
# market_index = market_index %>%
#   mutate(date = mdy(date))
# 
# save(market_index, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/market_index_unfinished.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/market_index_unfinished.RData")





comb= left_join(market_index, short_int, by = c("date", "symbol"))


comb = comb %>%
  mutate(utc = utc - 7200)

comb = comb %>%
  filter(!is.na(FF))

x = comb %>%
  group_by(date, symbol) %>%
  summarize(count = n()) %>%
  group_by(symbol) %>%
  summarize(sum_obs = sum(count)) 

x= x[x$sum_obs ==23225,]

symbols_to_keep = x$symbol




save(analysis_1_df_1, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_1.RData" )
save(analysis_1_df_5, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_5.RData" )
save(analysis_1_df_10, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_10.RData" )
save(analysis_1_df_60, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_60.RData" )
save(analysis_1_df_daily, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_daily.RData" )

save(analysis_1_df_1, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/1_machine_analysis/data/analysis_1_df_1.RData" )
save(analysis_1_df_5, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/1_machine_analysis/data/analysis_1_df_5.RData" )
save(analysis_1_df_10, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/1_machine_analysis/data/analysis_1_df_10.RData" )
save(analysis_1_df_60, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/1_machine_analysis/data/analysis_1_df_60.RData" )
save(analysis_1_df_daily, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/1_machine_analysis/data/analysis_1_df_daily.RData" )

save(analysis_1_df_1, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data/analysis_1_df_1.RData" )
save(analysis_1_df_5, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data/analysis_1_df_5.RData" )
save(analysis_1_df_10, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data/analysis_1_df_10.RData" )
save(analysis_1_df_60, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data/analysis_1_df_60.RData" )
save(analysis_1_df_daily, file ="C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data/analysis_1_df_daily.RData" )











