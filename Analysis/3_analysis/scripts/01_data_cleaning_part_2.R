


### PREAMBLE STUFF

library(pacman)

p_load(tidyverse, RPostgres, httr, data.table, lubridate, gtrendsR)


### LOAD ALL STOCKS 

load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/stonks/stonks_ALL_Feb_24.RData")



# LIST OF STOCKS TO SELECT IS 
#  AMC, TR, BBBY, BB, EXPR, NKD, AAL, TRVG, SNDL, NOK, CTRM 

AMC = stonks_ALL_Feb_24 %>%
  filter(symbol =="AMC")
TR = stonks_ALL_Feb_24 %>%
  filter(symbol =="TR")
BBBY = stonks_ALL_Feb_24 %>%
  filter(symbol =="BBBY")
EXPR = stonks_ALL_Feb_24 %>%
  filter(symbol =="EXPR")
AAL = stonks_ALL_Feb_24 %>%
  filter(symbol =="AAL")



### LOAD GME 

GME = read_csv(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/stonks/GME_extended.csv")

GME = GME %>%
  mutate(utc = as.numeric(as.POSIXct(Date, format = "%m/%d/%Y %H:%M", tz = "EST"))) %>%
  separate(Date, into = c("date", "time"), sep = " ")

### CHANGE NAMES
names = names(AAL)

names(GME) = names(AAL)


### CREATE LIST OF TARGETED STOCKS
targeted = rbind(GME, AMC, TR, BBBY, EXPR, AAL) %>%
  mutate(return = (open/close) - 1) %>%
  mutate(minute = utc) %>%
  select(-utc)

remove(GME, AMC, TR, BBBY, EXPR, AAL)

### LOAD WSB DATA

load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_1.RData")

analysis_1_df_1 = analysis_1_df_1 %>%
  select(c(minute, coms_per_min, subs_per_min))

targeted = left_join(targeted, analysis_1_df_1, by = "minute")

### OBTAIN TWEET METRICS


# start_utc = 1606798800
# 
# 
# # DEFINE BEARER TOKEN
# bearer_token = "AAAAAAAAAAAAAAAAAAAAAHNkWwEAAAAAK9oQFWwXzb2gqzO8FBkyhWtDgrA%3DgOFMpY4ACvXgo3MscuRE8w1WSaoHVUVn06GkHIKKu7m6585z3V"
# 
# 
# 
# timestamp = as.data.frame(seq(from = 1606798800, to = (1606798800+(86400*90)), by = 86400))
# names(timestamp) = "utc"
# 
# timestamp = timestamp %>%
#   mutate(start_date = as.Date(as.POSIXct(utc, origin = "1970-01-01", tz = "EST"))) %>%
#   mutate(end_date = start_date + 1) %>%
#   mutate(start_date = paste(start_date, "T00:00:00Z", sep = "") ) %>%
#   mutate(end_date = paste(end_date, "T00:00:00Z", sep = "" )) 
# 
# 
# 
# headers = c(
#   `Authorization` = sprintf('Bearer %s', bearer_token)
# )
# 
# n = list(NULL)
# 
# queries = c('#AMC','$AMC', '#TR', '$TR', '#BBBY', '$BBBY', '#AAL', '$AAL', '#EXPR', '$EXPR')
# 
# for(j in 1:length(queries)){
# 
#   q = queries[j]
# 
# for(i in 1:length(timestamp$start_date)){
# 
#   print(paste("i = ", i , sep = ""))
# 
#   params = list(
#     `query` = q ,
#     `granularity` = 'minute',
#     start_time = timestamp$start_date[i],
#     end_time= timestamp$end_date[i]
#   )
# 
#   response <- httr::GET(url = 'https://api.twitter.com/2/tweets/counts/all', httr::add_headers(.headers=headers), query = params)
# 
#   body <-
#     content(
#       response,
#       as = 'parsed',
#       type = 'application/json',
#       simplifyDataFrame = TRUE
#     )
# 
#   x = as.data.frame(body[[1]]) %>%
#     mutate(query = params$query)
# 
#   if(nrow(x) == 1){break}
# 
# n[[length(n)+1]] = x
# 
#   date_time<-Sys.time()
#   while((as.numeric(Sys.time()) - as.numeric(date_time))<4){}
# 
# 
# }}
# 
# tweets =rbindlist(n)
# 
# 
# save(tweets, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/Twitter/targeted_stocks_twitter.RData")

### LOAD TARGETED STOCKS
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/Twitter/targeted_stocks_twitter.RData")

# CREATE UTC VARIABLE
tweets = tweets %>%
  mutate(minute = ymd_hms(start))  %>%
  mutate(minute = as.numeric(as.POSIXct(minute, format = "%Y-%m-%d %H:%M:%S")))


# SEPARATE DF FOR HASHTAG DATA
hashtags = tweets %>%
  filter(grepl(x = query, pattern = "#")) %>%
  separate(query, into = c(NA, "symbol"), sep = "#") %>%
  select(-c(start, end))

# SEPARATE DF FOR CASHTAG DATA
cashtags = tweets %>%
  filter(!grepl(x = query, pattern = "#"))  %>%
  mutate(symbol = substring(query, first = 2)) %>%
  select(-c(start, end, query))

# RENAME FOR CLARITY
names(cashtags)[1] = 'cashtag_count'
names(hashtags)[1] = 'hashtag_count'

# MERGE 
tweets1 = merge(cashtags, hashtags, by = c("minute", "symbol"))

# FILTER OUT GME
targeted = targeted %>%
  filter(!grepl(x = symbol, pattern = "GME"))


# LEFT JOIN THE DFS 
analysis_2_df_1 = left_join(targeted, tweets1, by = c("minute", "symbol"))


### BRING IN THE SPY FILE 
SPY = read_csv(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/stonks/SPY_dec_mar.csv")

# CREATE DF OF MINUTES
mins = as.data.frame(seq(from = 1606798800, to = 1614574740, by = 60))
names(mins)[1] = "minute"


names(SPY) = c("Date", "Symbol", "open_SPY", "high_SPY", "low_SPY", "close_SPY", "volume_SPY")

SPY = SPY %>%
  mutate(minute = as.numeric(as.POSIXct(Date, format = "%m/%d/%Y %H:%M", tz = "EST")))

SPY = left_join(mins, SPY, by = "minute")

SPY = SPY  %>%
  mutate(Volume = ifelse(is.na(volume_SPY), 0, volume_SPY )) %>%
  mutate(Symbol = ifelse(is.na(Symbol), "SPY", Symbol )) 

# IMPUTE MISSING VALUES IN SPY

for(j in 553: length(SPY$open_SPY)){
  print(j)
  
  if(is.na(SPY$open_SPY[j])){   SPY$open_SPY[j] = SPY$close_SPY[j-1]}
  
  if(is.na(SPY$close_SPY[j])){SPY$close_SPY[j] = SPY$open_SPY[j]}
  
}


analysis_2_df_1 = left_join(analysis_2_df_1, SPY, by = "minute")

analysis_2_df_1 = analysis_2_df_1 %>%
  mutate(return_SPY = (close_SPY/open_SPY)-1) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(return = ifelse(is.na(return), 0, return)) %>%
  select(-c(open_SPY, high_SPY, low_SPY, close_SPY, Volume, open, high, low, close, Date, Symbol)) %>%
  mutate(volume_SPY = ifelse(is.na(volume_SPY), 0, volume_SPY)) %>%
  mutate(volume = ifelse(is.na(volume), 0, volume)) %>%
  mutate(cashtag_count = ifelse(is.na(cashtag_count), 0, cashtag_count)) %>%
  mutate(hashtag_count = ifelse(is.na(hashtag_count), 0, hashtag_count)) %>%
  mutate(subs_per_min  = ifelse(is.na(subs_per_min), 0, subs_per_min)) %>%
  mutate(coms_per_min  = ifelse(is.na(coms_per_min), 0, coms_per_min))



# AGGREGATE #####################################################################################
analysis_2_df_5 = analysis_2_df_1 %>%
  mutate(minute = minute - (minute%%300)) %>%
  mutate(return = return + 1) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(minute_symbol = paste(minute, symbol, sep = "_")) %>%
  group_by(minute_symbol) %>%
  summarize( volume = sum(volume), cashtags_per_min = sum(cashtag_count),hashtags_per_min = sum(hashtag_count),  
            coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), 
            return = prod(return), return_SPY = prod(return_SPY))  %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = return_SPY - 1) %>%
  separate(minute_symbol, into = c("minute", "symbol"), sep = "_")

analysis_2_df_10 = analysis_2_df_1 %>%
  mutate(minute = minute - (minute%%600)) %>%
  mutate(return = return + 1) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(minute_symbol = paste(minute, symbol, sep = "_")) %>%
  group_by(minute_symbol) %>%
  summarize( volume = sum(volume), cashtags_per_min = sum(cashtag_count),hashtags_per_min = sum(hashtag_count),  
             coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), 
             return = prod(return), return_SPY = prod(return_SPY))  %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = return_SPY - 1) %>%
  separate(minute_symbol, into = c("minute", "symbol"), sep = "_")

analysis_2_df_60 = analysis_2_df_1 %>%
  mutate(minute = minute - (minute%%3600)) %>%
  mutate(return = return + 1) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(minute_symbol = paste(minute, symbol, sep = "_")) %>%
  group_by(minute_symbol) %>%
  summarize( volume = sum(volume), cashtags_per_min = sum(cashtag_count),hashtags_per_min = sum(hashtag_count),  
             coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), 
             return = prod(return), return_SPY = prod(return_SPY))  %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = return_SPY - 1) %>%
  separate(minute_symbol, into = c("minute", "symbol"), sep = "_")

analysis_2_df_daily = analysis_2_df_1 %>%
  mutate(minute = minute - (minute%%86400)) %>%
  mutate(return = return + 1) %>%
  mutate(return_SPY = return_SPY + 1) %>%
  mutate(minute_symbol = paste(minute, symbol, sep = "_")) %>%
  group_by(minute_symbol) %>%
  summarize( volume = sum(volume), cashtags_per_min = sum(cashtag_count),hashtags_per_min = sum(hashtag_count),  
             coms_per_min = sum(coms_per_min), subs_per_min = sum(subs_per_min), volume_SPY = sum(volume_SPY), 
             return = prod(return), return_SPY = prod(return_SPY))  %>%
  mutate(return = return - 1) %>%
  mutate(return_SPY = return_SPY - 1) %>%
  separate(minute_symbol, into = c("minute", "symbol"), sep = "_") %>%
  mutate(minute = as.numeric(minute)) %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "UTC")) %>%
  mutate(date = ymd(date))






# load rfr data

rfr = read_csv(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Data/stonks/risk_free/risk_free.csv") %>%
  mutate(rfr = DTB3) %>%
  mutate(date = as.Date( DATE)) %>%
  select(-c(DTB3, DATE)) %>%
  mutate(date = ymd(date))

for(j in 2:nrow(rfr)){
  rfr1 = (as.numeric(rfr$rfr[j-1]) + as.numeric(rfr$rfr[j+1]))/2
  if (rfr$rfr[j]== "."){  rfr$rfr[j] = rfr1 }
  
}


analysis_2_df_daily = left_join( analysis_2_df_daily, rfr, by = "date")

# get google data


search_params = c("AMC", "AAL", "BBBY", "EXPR", "TR")


search_list = list(NULL);for(j in 1: length(search_params)){
  
  GME_search = gtrends(
    keyword = search_params[j],
    time = "2020-09-01 2021-4-01",
    gprop = "web"  
  )
  
  
  search_list[[j]] = GME_search[[1]]
  
  google_search <<- rbindlist(search_list)
  google_search = google_search %>%
    mutate(date = ymd(date))
}

google_search_1 = google_search  %>%
  select(-c(geo, time, gprop, category)) %>%
  mutate(date = ymd(date))

names(google_search_1) = c("date", "google", "symbol")

analysis_2_df_daily = analysis_2_df_daily %>%
  mutate(date = ymd(date))


daily_data = left_join(analysis_2_df_daily, google_search_1, by = c( "symbol","date"))


analysis_2_df_1$minute = as.numeric(analysis_2_df_1$minute)
analysis_2_df_5$minute = as.numeric(analysis_2_df_5$minute)
analysis_2_df_10$minute = as.numeric(analysis_2_df_10$minute)
analysis_2_df_60$minute = as.numeric(analysis_2_df_60$minute)
analysis_2_df_daily$minute = as.numeric(analysis_2_df_daily$minute)





# SAVE THAT 
save(analysis_2_df_1, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_1.RData")
save(analysis_2_df_5, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_5.RData")
save(analysis_2_df_10, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_10.RData")
save(analysis_2_df_60, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_60.RData")
save(analysis_2_df_daily, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_daily.RData")


### GET GME RETURNS #########################




















