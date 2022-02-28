

### PREAMBLE STUFF ##########

library(pacman)


p_load(tidyverse, shrinkTVP)



# LOAD DATA ####################### 
load( file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_1.RData")
load( file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_5.RData")
load( file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_10.RData")
load( file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_60.RData")
load( file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/3_analysis/data/analysis_2_df_daily.RData")


# LOAD GME RETURNS
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_1.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_5.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_10.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_60.RData")
load(file = "C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_daily.RData")

# GET RID OF FLUFF 
GME_1 = analysis_1_df_1 %>%
  select(return, minute) %>%
  filter(!is.na(return))
GME_5 = analysis_1_df_5 %>%
  select(return, minute) %>%
  mutate(minute = as.numeric(minute))
  filter(!is.na(return))
GME_10 = analysis_1_df_10 %>%
  select(return, minute) %>%
  filter(!is.na(return))
GME_60 = analysis_1_df_60 %>%
  select(return, minute) %>%
  filter(!is.na(return))
GME_daily = analysis_1_df_daily %>%
  select(return, minute) %>%
  filter(!is.na(return))

# CHANGE NAMES 
names(GME_1)[1] = "return_GME"
names(GME_5)[1] = "return_GME"
names(GME_10)[1] = "return_GME"
names(GME_60)[1] = "return_GME"
names(GME_daily)[1] = "return_GME"

remove(analysis_1_df_1, analysis_1_df_5, analysis_1_df_10, analysis_1_df_60, analysis_1_df_daily)

analysis_2_df_1 = left_join(analysis_2_df_1, GME_1, by = "minute")
analysis_2_df_5 = left_join(analysis_2_df_5, GME_5, by = "minute")
analysis_2_df_10 = left_join(analysis_2_df_10, GME_10, by = "minute")
analysis_2_df_60 = left_join(analysis_2_df_60, GME_60, by = "minute")
analysis_2_df_daily = left_join(analysis_2_df_daily, GME_daily, by = "minute")


# ANALYSIS ########################################

# AAL DAILY ######################################
AAL_daily = analysis_2_df_daily %>%
  filter(symbol =="AAL") %>%
  select(-c(minute, symbol, volume, volume_SPY, date)) %>%
  filter(!is.na(return_GME)) %>%
  mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
  mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
  mutate(coms_per_min = asinh(coms_per_min)) %>%
  mutate(subs_per_min = asinh(subs_per_min)) %>%
  select(-return_GME)
  

m1_AAL_daily = shrinkTVP(data = AAL_daily, return ~ ., sv = F)
# EACH COLUMN IS 1 TIME PERIOD'S SIMULATED RESIDUALS
resid = residuals(m1_AAL_daily)
# THIS TAKES THE MEAN OF EVERY COLUMN 
resid = as.data.frame(apply(resid, 2, FUN = mean))
names(resid) = "resid"

AAL_daily = cbind(AAL_daily, resid, GME_daily) %>%
  mutate(sq_resid = resid^2) %>%
  select(return_GME, sq_resid) %>%
  mutate(sq_resid_1 = lag(sq_resid, 1)) %>%
  mutate(sq_resid_2 = lag(sq_resid, 2)) %>%
  filter(!is.na(sq_resid_1)) %>%
  filter(!is.na(sq_resid_2))

# HYPOTHESIS TESTING

 res_AAL_daily = my_LPDS(data = AAL_daily, 
              vars_to_keep = c("sq_resid", "sq_resid_1", "sq_resid_2"), 
              y = "sq_resid", 
              start = 5, 
              end = 56, 
              sv = F)
 

 rtf = tvLM(data = AAL_daily, sq_resid ~ return_GME )
 summary(rtf)
 rtf1 = confint(rtf)
 plot(rtf1)
 

# BBBY DAILY ###################################################
 
 BBBY_daily = analysis_2_df_daily %>%
   filter(symbol =="BBBY") %>%
   select(-c(minute, symbol, volume, volume_SPY, date)) %>%
   filter(!is.na(return_GME)) %>%
   mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
   mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
   mutate(coms_per_min = asinh(coms_per_min)) %>%
   mutate(subs_per_min = asinh(subs_per_min)) %>%
   select(-return_GME)
 

 
 
 m1_BBBY_daily = shrinkTVP(data = BBBY_daily, return ~ ., sv = F)
 # EACH COLUMN IS 1 TIME PERIOD'S SIMULATED RESIDUALS
 resid = residuals(m1_BBBY_daily)
 # THIS TAKES THE MEAN OF EVERY COLUMN 
 resid = as.data.frame(apply(resid, 2, FUN = mean))
 names(resid) = "resid"
 
 BBBY_daily = cbind(BBBY_daily, resid, GME_daily) %>%
   mutate(sq_resid = resid^2) %>%
   select(return_GME, sq_resid) %>%
   mutate(sq_resid_1 = lag(sq_resid, 1)) %>%
   mutate(sq_resid_2 = lag(sq_resid, 2)) %>%
   filter(!is.na(sq_resid_1)) %>%
   filter(!is.na(sq_resid_2))
 
 # HYPOTHESIS TESTING
 
 res_BBBY_daily = my_LPDS(data = BBBY_daily, 
                         vars_to_keep = c("sq_resid", "sq_resid_1", "sq_resid_2"), 
                         y = "sq_resid", 
                         start = 5, 
                         end = 56, 
                         sv = F)

rtf = tvLM(data = BBBY_daily, sq_resid ~ return_GME )
summary(rtf)
rtf1 = confint(rtf)
 plot(rtf1)
 #############################################
 
 # BBBY DAILY ###################################################
 
 BBBY_60 = analysis_2_df_60 %>%
   filter(symbol =="BBBY") %>%
   select(-c(minute, symbol, volume, volume_SPY)) %>%
   filter(!is.na(return_GME)) %>%
   mutate(cashtags_per_min = asinh(cashtags_per_min)) %>%
   mutate(hashtags_per_min = asinh(hashtags_per_min)) %>%
   mutate(coms_per_min = asinh(coms_per_min)) %>%
   mutate(subs_per_min = asinh(subs_per_min)) 
 
 
 m1_BBBY_60 = shrinkTVP(data = BBBY_60, return ~ cashtags_per_min +hashtags_per_min +coms_per_min    
 +subs_per_min    + return       +    return_SPY      , sv = F)
 # EACH COLUMN IS 1 TIME PERIOD'S SIMULATED RESIDUALS
 resid = residuals(m1_BBBY_60)
 # THIS TAKES THE MEAN OF EVERY COLUMN 
 resid = as.data.frame(apply(resid, 2, FUN = mean))
 names(resid) = "resid"
 
 BBBY_60 = cbind(BBBY_60, resid) %>%
   mutate(sq_resid = resid^2) %>%
   select(return_GME, sq_resid) %>%
   mutate(sq_resid_1 = lag(sq_resid, 1)) %>%
   mutate(sq_resid_2 = lag(sq_resid, 2)) %>%
   filter(!is.na(sq_resid_1)) %>%
   filter(!is.na(sq_resid_2))
 
 # HYPOTHESIS TESTING
 
 res_BBBY_60 = my_LPDS(data = BBBY_60, 
                          vars_to_keep = c("sq_resid", "sq_resid_1", "sq_resid_2"), 
                          y = "sq_resid", 
                          start = 5, 
                          end = 56, 
                          sv = F)
 
 
 






















