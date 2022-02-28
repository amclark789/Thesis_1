

### PREAMBLE STUFF #########################################

# LOAD PACMAN
library(pacman)

source("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/functions/my_LPDS.R")

# LOAD EVERYTHING ELSE
p_load(tidyverse, tidymodels, shrinkTVP,fastDummies, doParallel, foreach, xts, tvReg)

# SET WD 
setwd("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_in")



### DAILY ###################################################

# LOAD DAILY DATA
load(file = "analysis_1_df_daily.RData")

# SORT BY MINUTE (DAY) 
# GET RID OF SPY VOLUME
# TURN EVERYTHING INTO ASINH
# CREATE LAGS



analysis_1_df_daily = analysis_1_df_daily %>%
  arrange(minute) %>%
  select(-c(volume_SPY)) %>%
  mutate(volume = asinh(volume)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  # mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  # mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  # mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  
  mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
  mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
  # mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
  # mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
  # mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>%
  
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  # mutate(return_lag_3 = lag(return, 3)) %>%
  # mutate(return_lag_4 = lag(return, 4)) %>%
  # mutate(return_lag_5 = lag(return, 5)) %>%
  
  mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
  mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
  # mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
  # mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
  # mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
  
  mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
  mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
  # mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
  # mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
  # mutate(cash_lag_5 = lag(cashtags_per_min, 5) ) %>%
  
  mutate(return_SPY_1 = lag(return_SPY, 1)) %>%
  mutate(return_SPY_2 = lag(return_SPY, 2))# %>%
# mutate(return_SPY_3 = lag(return_SPY, 3)) %>%
# mutate(return_SPY_4 = lag(return_SPY, 4)) %>%
# mutate(return_SPY_5 = lag(return_SPY, 5)) 

# TURN ROWNAMES TO COLUMN

names = analysis_1_df_daily$minute

rownames(analysis_1_df_daily) = names



analysis_1_df_daily = analysis_1_df_daily %>%
  # CREATE DATE VARIABLE
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))  %>%
  # GET RID OF CONTEMPORANEOUS STUFF
  select(-c(cashtags_per_min, hashtags_per_min, coms_per_min, subs_per_min, cash_hash_per_min)) %>%
  # SEPARATE DATE INTO HOUR
  separate(date, into = c("date", NA), sep = " ") %>%
  # GET RID OF MINUTES AND SECONDS
  # separate(hour, into = c("hour", NA,NA), sep = ":") %>%
  # TURN DATE INTO A DATE
  mutate(date = as.Date(date)) %>%
  # TURN HOUR INTO A NUMERIC
  #mutate(hour = as.numeric(hour)) %>%
  # FILTER OUT HOURS BEFORE 9 
  #filter(hour > 8) %>%
  # FILTER OUT HOURS AFTER 6 
  #filter(hour < 18 ) %>%
  # DO NOT FILTER OUT DATES BEFORE JAN 1 
  # filter(date > "2020-12-20") %>%
  # FILTER OUT DATES AFTER JAN 5
  filter(date < "2021-02-06") %>%
  # FILTER OUT SUNDAYS AND SATURDAYS
  mutate(DOTW = weekdays(date)) %>%
  filter(DOTW != "Sunday") %>%
  filter(DOTW != "Saturday") %>%
  # DROP DAY OF THE WEEK 
  select(-DOTW) %>%
  # DUMMY THE DATE COLUMN 
  #dummy_cols("date") %>%
  # SORT BY MINUTE, GET RID OF DATE AND HOUR
  arrange(minute) %>%
  # GET RID OF DATE AND HOUR
  # select(-c(hour)) %>%
  # GET RID OF NAs IN RETURN
  filter(!is.na(return))

analysis_1_df_daily = as.data.frame(analysis_1_df_daily, row.names = as.character( analysis_1_df_daily$minute))

analysis_1_df_daily = analysis_1_df_daily %>%
  select(-minute)

gc()

# FINAL DF SHOULD HAVE 
# RETURN 
# RETURN SPY, RETURN SPY LAG 1, RETURN SPY LAG 2
# COMMENTS LAG 1 AND 2
# SUBS LAG 1 AND 2 
# RETURNS LAG 1 AND 2
# HASH LAG 1 AND 2
# CASHTAG LAG 1 AND 2 
# 24 DATE DUMMIES 
# 1 RESPONSE VAR
# 13 NUMERIC IVs 
# 9 HOURS OF THE DAY, 9AM - 5PM, INCLUSIVE
# 216 OBS 

# str(analysis_1_df_daily)

#stats = analysis_1_df_daily %>%
#  group_by(date) %>%
#  summarize(daily_mean = mean(return))

#analysis_1_df_daily = left_join(analysis_1_df_daily, stats)
analysis_1_df_daily = analysis_1_df_daily 



# model_1_daily = shrinkTVP(data = analysis_1_df_daily, return ~ ., sv = F, niter = 50000,nburn = 10000, nthin = 2)

# summary(model_1_daily)

# plot(model_1_daily)

# save(model_1_daily, file = "D:/Anthony/temp/2_TVP_analysis/output/model_1_60.RData")

rfr = read_csv(file = "risk_free.csv")

rfr = rfr %>%
  mutate(date = as.Date(DATE)) %>%
  select(-DATE) %>%
  filter(date>"2020-01-10") %>%
  filter(date<"2021-06-01")

names(rfr) = c("rfr", "date")

rfr = rfr %>%
  mutate(rfr =as.numeric(ifelse(rfr==".", NA, rfr))) %>%
  mutate(rfr_lag = lag(rfr,1)) %>%
  mutate(rfr_lead = lead(rfr,1)) %>%
  mutate(rfr_avg = (rfr_lead + rfr_lag)/2) %>%
  mutate(rfr = ifelse(is.na(rfr), rfr_avg, rfr)) %>%
  select(c(rfr, date))
  


analysis_1_df_daily = left_join(analysis_1_df_daily, rfr, by = "date")

analysis_1_df_daily = analysis_1_df_daily %>%
  mutate(rfr = as.numeric(rfr)) %>%
  select(-c( dividends, hidden, ERSI, ESI, ESI_of_FF, days_to_cover_3m_on_loan)) %>%
  mutate(CAPM = return_SPY - rfr)


### 60 MINUTE ##################################################################


# LOAD HOURLY DATA
load(file = "analysis_1_df_60.RData")




# SORT BY MINUTE 
# GET RID OF SPY VOLUME
# TURN EVERYTHING INTO ASINH
# CREATE LAGS

analysis_1_df_60 = analysis_1_df_60 %>%
  arrange(minute) %>%
  select(-c(volume_SPY)) %>%
  mutate(volume = asinh(volume)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  # mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  # mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  # mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  
  mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
  mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
  # mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
  # mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
  # mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>% 
  
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  # mutate(return_lag_3 = lag(return, 3)) %>%
  # mutate(return_lag_4 = lag(return, 4)) %>%
  # mutate(return_lag_5 = lag(return, 5)) %>%
  
  mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
  mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
  # mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
  # mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
  # mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
  
  mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
  mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
  # mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
  # mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
  # mutate(cash_lag_5 = lag(cashtags_per_min, 5) ) %>%
  
  mutate(return_SPY_1 = lag(return_SPY, 1)) %>%
  mutate(return_SPY_2 = lag(return_SPY, 2)) #%>%
  # mutate(return_SPY_3 = lag(return_SPY, 3)) %>%
  # mutate(return_SPY_4 = lag(return_SPY, 4)) %>%
  # mutate(return_SPY_5 = lag(return_SPY, 5))

# TURN ROWNAMES TO COLUMN

names = analysis_1_df_60$minute

rownames(analysis_1_df_60) = names



analysis_1_df_60 = analysis_1_df_60 %>%
  # CREATE DATE VARIABLE
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))  %>%
  # GET RID OF CONTEMPORANEOUS STUFF
  select(-c(cashtags_per_min, hashtags_per_min, coms_per_min, subs_per_min, cash_hash_per_min)) %>%
  # SEPARATE DATE INTO HOUR
  separate(date, into = c("date", "hour"), sep = " ") %>%
  # GET RID OF MINUTES AND SECONDS
  separate(hour, into = c("hour", NA,NA), sep = ":") %>%
  # TURN DATE INTO A DATE
  mutate(date = as.Date(date)) %>%
  # TURN HOUR INTO A NUMERIC
  mutate(hour = as.numeric(hour)) %>%
  # FILTER OUT HOURS BEFORE 9 
  filter(hour > 8) %>%
  # FILTER OUT HOURS AFTER 6 
  filter(hour < 18 ) %>%
  # FILTER OUT DATES BEFORE JAN 1 
  filter(date > "2020-12-28") %>%
  # FILTER OUT DATES AFTER JAN 5
  filter(date < "2021-02-06") %>%
  # FILTER OUT SUNDAYS AND SATURDAYS
  mutate(DOTW = weekdays(date)) %>%
  filter(DOTW != "Sunday") %>%
  filter(DOTW != "Saturday") %>%
  # DROP DAY OF THE WEEK 
  select(-DOTW) %>%
  # DUMMY THE DATE COLUMN 
  #dummy_cols("date") %>%
  # SORT BY MINUTE, GET RID OF DATE AND HOUR
  arrange(minute) %>%
  # GET RID OF DATE AND HOUR
  select(-c(hour)) %>%
  # GET RID OF NAs IN RETURN
  filter(!is.na(return))

analysis_1_df_60 = as.data.frame(analysis_1_df_60, row.names = as.character( analysis_1_df_60$minute))

analysis_1_df_60 = analysis_1_df_60 %>%
  select(-minute)

gc()

# FINAL DF SHOULD HAVE 
# RETURN 
# RETURN SPY, RETURN SPY LAG 1, RETURN SPY LAG 2
# COMMENTS LAG 1 AND 2
# SUBS LAG 1 AND 2 
# RETURNS LAG 1 AND 2
# HASH LAG 1 AND 2
# CASHTAG LAG 1 AND 2 
# 24 DATE DUMMIES 
# 1 RESPONSE VAR
# 13 NUMERIC IVs 
# 9 HOURS OF THE DAY, 9AM - 5PM, INCLUSIVE
# 216 OBS 

# str(analysis_1_df_60)

stats = analysis_1_df_60 %>%
  group_by(date) %>%
  summarize(daily_mean = mean(return))

analysis_1_df_60 = left_join(analysis_1_df_60, stats)
analysis_1_df_60 = analysis_1_df_60 %>%
  mutate(demeaned_return = return - daily_mean) %>%
  select(-c(return, daily_mean))



# model_1_60 = shrinkTVP(data = analysis_1_df_60, demeaned_return ~ ., sv = F, niter = 50000,nburn = 10000, nthin = 2)
# 
# summary(model_1_60)
# 
# plot(model_1_60)
# 
# save(model_1_60, file = "D:/Anthony/temp/2_TVP_analysis/output/model_1_60.RData")
# 


### 10 MINUTE ############################################################################################################


load(file = "analysis_1_df_10.RData")


analysis_1_df_10 = analysis_1_df_10 %>%
  arrange(minute) %>%
  select(-c(volume_SPY)) %>%
  mutate(volume = asinh(volume)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  # mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  # mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  # mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  
  mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
  mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
  # mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
  # mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
  # mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>% 
  
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  # mutate(return_lag_3 = lag(return, 3)) %>%
  # mutate(return_lag_4 = lag(return, 4)) %>%
  # mutate(return_lag_5 = lag(return, 5)) %>%
  
  mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
  mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
  # mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
  # mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
  # mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
  
  mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
  mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
  # mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
  # mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
  # mutate(cash_lag_5 = lag(cashtags_per_min, 5) ) %>%
  
  mutate(return_SPY_1 = lag(return_SPY, 1)) %>%
  mutate(return_SPY_2 = lag(return_SPY, 2))# %>%
# mutate(return_SPY_3 = lag(return_SPY, 3)) %>%
# mutate(return_SPY_4 = lag(return_SPY, 4)) %>%
# mutate(return_SPY_5 = lag(return_SPY, 5)) 

# TURN ROWNAMES TO COLUMN

names = analysis_1_df_10$minute

rownames(analysis_1_df_10) = names



analysis_1_df_10 = analysis_1_df_10 %>%
  # CREATE DATE VARIABLE
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))  %>%
  # GET RID OF CONTEMPORANEOUS STUFF
  select(-c(cashtags_per_min, hashtags_per_min, coms_per_min, subs_per_min, cash_hash_per_min)) %>%
  # SEPARATE DATE INTO HOUR
  separate(date, into = c("date", "hour"), sep = " ") %>%
  # GET RID OF MINUTES AND SECONDS
  separate(hour, into = c("hour", NA,NA), sep = ":") %>%
  # TURN DATE INTO A DATE
  mutate(date = as.Date(date)) %>%
  # TURN HOUR INTO A NUMERIC
  mutate(hour = as.numeric(hour)) %>%
  # FILTER OUT HOURS BEFORE 9 
  filter(hour > 8) %>%
  # FILTER OUT HOURS AFTER 6 
  filter(hour < 18 ) %>%
  # FILTER OUT DATES BEFORE JAN 1 
  filter(date > "2020-12-28") %>%
  # FILTER OUT DATES AFTER JAN 5
  filter(date < "2021-02-06") %>%
  # FILTER OUT SUNDAYS AND SATURDAYS
  mutate(DOTW = weekdays(date)) %>%
  filter(DOTW != "Sunday") %>%
  filter(DOTW != "Saturday") %>%
  # DROP DAY OF THE WEEK 
  select(-DOTW) %>%
  # DUMMY THE DATE COLUMN 
  #dummy_cols("date") %>%
  # SORT BY MINUTE, GET RID OF DATE AND HOUR
  arrange(minute) %>%
  # GET RID OF DATE AND HOUR
  select(-c(hour)) %>%
  # GET RID OF NAs IN RETURN
  filter(!is.na(return))

analysis_1_df_10 = as.data.frame(analysis_1_df_10, row.names = as.character( analysis_1_df_10$minute))

analysis_1_df_10 = analysis_1_df_10 %>%
  select(-minute)

gc()

# FINAL DF SHOULD HAVE 
# RETURN 
# RETURN SPY, RETURN SPY LAG 1, RETURN SPY LAG 2
# COMMENTS LAG 1 AND 2
# SUBS LAG 1 AND 2 
# RETURNS LAG 1 AND 2
# HASH LAG 1 AND 2
# CASHTAG LAG 1 AND 2 
# 24 DATE DUMMIES 
# 1 RESPONSE VAR
# 13 NUMERIC IVs 
# 9 HOURS OF THE DAY, 9AM - 5PM, INCLUSIVE
# 216 OBS 

# str(analysis_1_df_10)

stats = analysis_1_df_10 %>%
  group_by(date) %>%
  summarize(daily_mean = mean(return))

analysis_1_df_10 = left_join(analysis_1_df_10, stats, by = "date")
analysis_1_df_10 = analysis_1_df_10 %>%
  mutate(demeaned_return = return - daily_mean) %>%
  select(-c(return, daily_mean))



# model_1_10 = shrinkTVP(data = analysis_1_df_10, demeaned_return ~ ., sv = F, niter = 50000,nburn = 10000, nthin = 2)
# 
# summary(model_1_10)
# 
# plot(model_1_10)
# 
# save(model_1_10, file = "D:/Anthony/temp/2_TVP_analysis/output/model_1_10.RData")
# 

### 5 MINUTE ############################################################################################################


load(file = "analysis_1_df_5.RData")


analysis_1_df_5 = analysis_1_df_5 %>%
  arrange(minute) %>%
  select(-c(volume_SPY)) %>%
  mutate(volume = asinh(volume)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  
  mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
  mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
  mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
  mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
  mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>%
  
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  mutate(return_lag_3 = lag(return, 3)) %>%
  mutate(return_lag_4 = lag(return, 4)) %>%
  mutate(return_lag_5 = lag(return, 5)) %>%
  
  mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
  mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
  mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
  mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
  mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
  
  mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
  mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
  mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
  mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
  mutate(cash_lag_5 = lag(cashtags_per_min, 5) ) %>%
  
  mutate(return_SPY_1 = lag(return_SPY, 1)) %>%
  mutate(return_SPY_2 = lag(return_SPY, 2)) %>%
  mutate(return_SPY_3 = lag(return_SPY, 3)) %>%
  mutate(return_SPY_4 = lag(return_SPY, 4)) %>%
  mutate(return_SPY_5 = lag(return_SPY, 5))

# TURN ROWNAMES TO COLUMN

names = analysis_1_df_5$minute

rownames(analysis_1_df_5) = names



analysis_1_df_5 = analysis_1_df_5 %>%
  # CREATE DATE VARIABLE
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))  %>%
  # GET RID OF CONTEMPORANEOUS STUFF
  select(-c( cashtags_per_min, hashtags_per_min, coms_per_min, subs_per_min, cash_hash_per_min)) %>%
  # SEPARATE DATE INTO HOUR
  separate(date, into = c("date", "hour"), sep = " ") %>%
  # GET RID OF MINUTES AND SECONDS
  separate(hour, into = c("hour", NA,NA), sep = ":") %>%
  # TURN DATE INTO A DATE
  mutate(date = as.Date(date)) %>%
  # TURN HOUR INTO A NUMERIC
  mutate(hour = as.numeric(hour)) %>%
  # MAKE DATE_HOUR
  mutate(date_hour = paste(date, hour)) %>%
  # FILTER OUT HOURS BEFORE 9 
  filter(hour > 8) %>%
  # FILTER OUT HOURS AFTER 6 
  filter(hour < 18 ) %>%
  # FILTER OUT DATES BEFORE JAN 1 
  filter(date > "2020-12-28") %>%
  # FILTER OUT DATES AFTER JAN 5
  filter(date < "2021-02-06") %>%
  # FILTER OUT SUNDAYS AND SATURDAYS
  mutate(DOTW = weekdays(date)) %>%
  filter(DOTW != "Sunday") %>%
  filter(DOTW != "Saturday") %>%
  # DROP DAY OF THE WEEK 
  select(-DOTW) %>%
  # DUMMY THE DATE COLUMN 
  #dummy_cols("date") %>%
  # SORT BY MINUTE, GET RID OF DATE AND HOUR
  arrange(minute) %>%
  # GET RID OF DATE AND HOUR
  #select(-c(hour)) %>%
  # GET RID OF NAs IN RETURN
  filter(!is.na(return))

analysis_1_df_5 = as.data.frame(analysis_1_df_5, row.names = as.character( analysis_1_df_5$minute))

analysis_1_df_5 = analysis_1_df_5 %>%
  select(-minute)

gc()

# FINAL DF SHOULD HAVE 
# RETURN 
# RETURN SPY, RETURN SPY LAG 1, RETURN SPY LAG 2
# COMMENTS LAG 1 AND 2
# SUBS LAG 1 AND 2 
# RETURNS LAG 1 AND 2
# HASH LAG 1 AND 2
# CASHTAG LAG 1 AND 2 
# 24 DATE DUMMIES 
# 1 RESPONSE VAR
# 13 NUMERIC IVs 
# 9 HOURS OF THE DAY, 9AM - 5PM, INCLUSIVE
# 216 OBS 

# str(analysis_1_df_5)

stats = analysis_1_df_5 %>%
  group_by(date_hour) %>%
  summarize(daily_mean = mean(return))

analysis_1_df_5 = left_join(analysis_1_df_5, stats, by = "date_hour")
analysis_1_df_5 = analysis_1_df_5 %>%
  mutate(demeaned_return = return - daily_mean) %>%
  select(-c(return, daily_mean, hour, date_hour))


# 
# model_1_5 = shrinkTVP(data = analysis_1_df_5, demeaned_return ~ ., sv = F, niter = 50000,nburn = 10000, nthin = 2)
# 
# summary(model_1_5)
# 
# plot(model_1_5)
# 
# save(model_1_5, file = "D:/Anthony/temp/2_TVP_analysis/output/model_1_10.RData")


### 1 MINUTE ############################################################################################################


load(file = "analysis_1_df_1.RData")


analysis_1_df_1 = analysis_1_df_1 %>%
#  mutate(return_SPY = close_SPY - open_SPY) %>%
  
  arrange(minute) %>%
  select(-c(volume_SPY)) %>%
  mutate(volume = asinh(volume)) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  # mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  # mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  # mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  
  mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
  mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
  # mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
  # mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
  # mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>% 
  
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  # mutate(return_lag_3 = lag(return, 3)) %>%
  # mutate(return_lag_4 = lag(return, 4)) %>%
  # mutate(return_lag_5 = lag(return, 5)) %>%
  
  mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
  mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
  # mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
  # mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
  # mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
  
  mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
  mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
  # mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
  # mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
  # mutate(cash_lag_5 = lag(cashtags_per_min, 5) ) %>%
  
  mutate(return_SPY_1 = lag(return_SPY, 1)) %>%
  mutate(return_SPY_2 = lag(return_SPY, 2))# %>%
# mutate(return_SPY_3 = lag(return_SPY, 3)) %>%
# mutate(return_SPY_4 = lag(return_SPY, 4)) %>%
# mutate(return_SPY_5 = lag(return_SPY, 5)) 

# TURN ROWNAMES TO COLUMN

names = analysis_1_df_1$minute

rownames(analysis_1_df_1) = names



analysis_1_df_1 = analysis_1_df_1 %>%
  # CREATE DATE VARIABLE
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST"))  %>%
  # GET RID OF CONTEMPORANEOUS STUFF
  select(-c( cashtags_per_min, hashtags_per_min, coms_per_min, subs_per_min, cash_hash_per_min)) %>%
  # SEPARATE DATE INTO HOUR
  separate(date, into = c("date", "hour"), sep = " ") %>%
  # GET RID OF MINUTES AND SECONDS
  separate(hour, into = c("hour", NA,NA), sep = ":") %>%
  # TURN DATE INTO A DATE
  mutate(date = as.Date(date)) %>%
  # TURN HOUR INTO A NUMERIC
  mutate(hour = as.numeric(hour)) %>%
  # FILTER OUT HOURS BEFORE 9 
  filter(hour > 8) %>%
  # FILTER OUT HOURS AFTER 6 
  filter(hour < 18 ) %>%
  # FILTER OUT DATES BEFORE JAN 1 
  filter(date > "2020-12-28") %>%
  # FILTER OUT DATES AFTER JAN 5
  filter(date < "2021-02-06") %>%
  # FILTER OUT SUNDAYS AND SATURDAYS
  mutate(DOTW = weekdays(date)) %>%
  filter(DOTW != "Sunday") %>%
  filter(DOTW != "Saturday") %>%
  # DROP DAY OF THE WEEK 
  select(-DOTW) %>%
  # DUMMY THE DATE COLUMN 
  #dummy_cols("date") %>%
  # SORT BY MINUTE, GET RID OF DATE AND HOUR
  arrange(minute) %>%
  # GET RID OF DATE AND HOUR
  select(-c(hour)) %>%
  # GET RID OF NAs IN RETURN
  filter(!is.na(return))

analysis_1_df_1 = as.data.frame(analysis_1_df_1, row.names = as.character( analysis_1_df_1$minute))

analysis_1_df_1 = analysis_1_df_1 %>%
  select(-minute)

gc()

# FINAL DF SHOULD HAVE 
# RETURN 
# RETURN SPY, RETURN SPY LAG 1, RETURN SPY LAG 2
# COMMENTS LAG 1 AND 2
# SUBS LAG 1 AND 2 
# RETURNS LAG 1 AND 2
# HASH LAG 1 AND 2
# CASHTAG LAG 1 AND 2 
# 24 DATE DUMMIES 
# 1 RESPONSE VAR
# 13 NUMERIC IVs 
# 9 HOURS OF THE DAY, 9AM - 5PM, INCLUSIVE
# 216 OBS 

# str(analysis_1_df_1)

stats = analysis_1_df_1 %>%
  group_by(date) %>%
  summarize(daily_mean = mean(return))

analysis_1_df_1 = left_join(analysis_1_df_1, stats, by = "date")
analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(demeaned_return = return - daily_mean) %>%
  select(-c(return, daily_mean, dividends, hidden, ERSI, ESI, ESI_of_FF, days_to_cover_3m_on_loan)) %>%
  mutate(date = as.POSIXct(date))

remove(stats)





save(analysis_1_df_1, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out/analysis_1_df_1.RData")
save(analysis_1_df_5, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out/analysis_1_df_5.RData")
save(analysis_1_df_10, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out/analysis_1_df_10.RData")
save(analysis_1_df_60, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out/analysis_1_df_60.RData")
save(analysis_1_df_daily, file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out/analysis_1_df_daily.RData")














