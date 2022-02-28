






##### PREAMBLE STUFF ######
library(pacman)
p_load(tidyverse, data.table, RPostgres, lubridate, parsedate, shrink_TVP)



######## LOAD EXTENDED GME DATA ##### ######################################

GME = read_csv(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/GME_extended.csv")

GME = GME %>%
  mutate(date = as.POSIXct(Date, format = "%m/%d/%Y %H:%M", tz = "EST")) %>%
  mutate(minute = as.numeric(date))





######## LOAD ALL WSB COMMENTS ###########################
# 
# drv <- dbDriver("Postgres")
# 
# con <- dbConnect(drv, user='postgres', password='montanaecon', dbname='reddit', host = 'localhost')
# 
# 
# res = dbSendQuery(conn = con, "SELECT id, created_utc FROM wsb_comments_combined")
# 
# coms = dbFetch(res)

##### AGGREGATE


coms = coms %>%
  mutate(minute = created_utc  - (created_utc %% 60 )) %>%
  group_by(minute) %>%
  summarize(coms_per_min = n())

### CREATE MINUTES DF ###

mins = as.data.frame(seq(from = 1606798800, to = 1614574740, by = 60))
names(mins)[1] = "minute"

wsb = left_join(mins, coms, by = "minute") %>%
  mutate(coms_per_min = ifelse(is.na(coms_per_min), 0 , coms_per_min))


####### LOAD COUNT OF ALL TWEETS CONTAINING #GME #####

load("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/GME_hashtag_volume.RData")

names(hashtag_volume)[3] = "hashtags_per_min"

hashtag_volume = hashtag_volume %>%
  select(start, hashtags_per_min)


##### LOAD COUNT OF ALL TWEETS CONTAINING $GME ####

load("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/GME_cashtag_volume.RData")

names(cashtag_volume)[3] = "cashtags_per_min"

cashtag_volume = cashtag_volume %>%
  select(start, cashtags_per_min)


##### LOAD COUNT OF ALL TWEETS CONTAINING BOTH ####

load("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/GME_hash_cash_volume.RData")

names(hash_and_cash_volume)[3] = "cash_hash_per_min"

hash_and_cash_volume = hash_and_cash_volume %>%
  select(start, cash_hash_per_min)


##### MERGE TWITTER DATA #####

twitter_data = left_join(cashtag_volume, hashtag_volume, by = "start")

twitter_data = left_join(twitter_data, hash_and_cash_volume, by = "start")

twitter_data = twitter_data %>%
  mutate(start = parse_iso_8601(start)) %>%
  mutate(start = as.numeric(as.POSIXct(start, origin = "1970-01-01", tz = "EST"))) %>%
  mutate(minute = start) %>%
  select(-c(start))




###### MERGE THEM ALL 


analysis_1_df_1 = left_join(twitter_data, wsb, by = "minute")

analysis_1_df_1 = left_join(analysis_1_df_1, GME, by = "minute")


######### SMOOTH OUT THE GME STUFF #################

analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume )) %>%
  mutate(Symbol = ifelse(is.na(Symbol), "GME", Symbol )) %>%
  mutate(Volume = ifelse(is.na(Volume), 0, Volume ))


# IMPUTE MISSING VALUES

analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(return = (Close-Open)/Open) %>%
  mutate(return = ifelse(is.na(return), 0, return))


############## IMPORT SPY

SPY = read_csv(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/SPY_dec_mar.csv")

names(SPY) = c("Date", "Symbol", "open_SPY", "high_SPY", "low_SPY", "close_SPY", "volume_SPY")

SPY = SPY %>%
  mutate(minute = as.numeric(as.POSIXct(Date, format = "%m/%d/%Y %H:%M", tz = "EST")))

SPY = left_join(mins, SPY, by = "minute")

SPY = SPY  %>%
  mutate(Volume = ifelse(is.na(volume_SPY), 0, volume_SPY )) %>%
  mutate(Symbol = ifelse(is.na(Symbol), "SPY", Symbol )) %>%
  select(-Volume)


analysis_1_df_1 = left_join(analysis_1_df_1, SPY, by = "minute")


analysis_1_df_1 = analysis_1_df_1 %>%
  select(-c(Date.y, Symbol.y, Date.x, Symbol.x))


names(analysis_1_df_1)

x = c("cashtags_per_min",  "hashtags_per_min",  "cash_hash_per_min", "minute","coms_per_min", "Open_GME","High_GME","Low_GME","Close_GME","Volume_GME","date","return","open_SPY","high_SPY","low_SPY","close_SPY","volume_SPY")



load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/wallstreetbets_submissions_hourly_Dec_Mar.RData")


subs_hourly = submissions_hourly %>%
  mutate(created = created - (created%%60))


subs_hourly = subs_hourly %>%
  group_by(created) %>%
  summarize(subs_per_min = n())

names(subs_hourly)[1] = "minute"

analysis_1_df_1 = left_join(analysis_1_df_1, subs_hourly, by = "minute")


analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(subs_per_min = ifelse(is.na(subs_per_min), "0", subs_per_min))



save(analysis_1_df_1, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_1.RData")
save(analysis_1_df_5, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_5.RData")
save(analysis_1_df_10, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_10.RData")
save(analysis_1_df_60, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_60.RData")
save(analysis_1_df_daily, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/in_data/analysis_1_df_daily.RData")









