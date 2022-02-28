



# LOAD PACMAN
library(pacman)

source("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/functions/my_LPDS.R")

# LOAD EVERYTHING ELSE
p_load(tidyverse, tidymodels, shrinkTVP,fastDummies, doParallel, foreach, xts, tvReg)

setwd("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/data_out")

load("analysis_1_df_daily.RData")
load("analysis_1_df_60.RData")
load("analysis_1_df_10.RData")



######### WINDOW FOR ALL DATASETS = JAN 4 TO FEB 5 INCLUSIVE


niter = 20000
nburn = 10000
nthin = 2
ncores = 4

setwd("C:/Users/m28h791/Dropbox/Anthony_Clark_Thesis/Analysis/2_analysis/results")


### DAILY RETURN ########################################################

# REGRESS RETURN ON RETURN_SPY, 

# j = 25 means a start date of jan 1

data = xts(analysis_1_df_daily, order.by = analysis_1_df_daily$date)

data = analysis_1_df_daily %>%
  select(  -c(volume, date, symbol, FF, relative_volume, asinh_rel_vol, return_lag_2, return_SPY_2));
data = na.omit(data)



T1 = Sys.time()
res_daily = my_LPDS(data = data, 
                    niter = niter, 
                    nburn = nburn, 
                    nthin = nthin, 
                    vars_to_keep = c("return", "return_SPY", "return_SPY_1", "return_lag_1", "rfr", "CAPM"), 
                    y = "return", 
                    start = 25, 
                    end = 26,
                    sv = F, 
                    ncores = ncores);
T2 = Sys.time();
(T2-T1);
save(res_daily, file = "res_daily.RData")

# TOOK 3.2 MINUTES


### HOURLY RETURN #######################################################

# j = 37 means a start date of jan 4

data = analysis_1_df_60 %>%
  select(  -c(date, volume));
data = na.omit(data);


T3 = Sys.time()

res_hourly = my_LPDS(data = data, 
                     niter = niter, 
                     nburn = nburn, 
                     nthin = nthin, 
                     vars_to_keep = c("demeaned_return", "return_SPY", "return_SPY_1", "return_SPY_2", "return_lag_1", "return_lag_2"), 
                     y = "demeaned_return", 
                     start = 37, 
                     end = 260)
T4 = Sys.time()

(T4 - T3)


save(res_hourly, file = "res_hourly.RData")


### 10-MINUTE RETURN ####################################################

data = analysis_1_df_10 %>% 
  select( -c(date, volume));
data = na.omit(data);

# j = 217 means a start date of jan 4

T5 = Sys.time()

RES_10 = my_LPDS(data = data, 
                 niter = niter, 
                 nburn = nburn, 
                 nthin = nthin, 
                 vars_to_keep = c("demeaned_return", "return_SPY", "return_SPY_1", "return_SPY_2", "return_lag_1", "return_lag_2"), 
                 y = "demeaned_return", 
                 start = 1565, 
                 end = 420, 
                 sv = F)




T6 = Sys.time()

(T6-T5)


save(RES_10, file = "res_10.RData")


### 5-MINUTE RETURN #######################################################


data = analysis_1_df_5 %>% 
  select( -c(date, volume));
data = na.omit(data);


# j = 433 means a start date of jan 4

my_LPDS(data = data, 
        niter = niter, 
        nburn = nburn, 
        nthin = nthin, 
        vars_to_keep = c("demeaned_return", "return_SPY", "return_SPY_1", "return_SPY_2", "return_SPY_3", "return_SPY_4", "return_SPY_5", "return_lag_1", "return_lag_2", "return_lag_4", "return_lag_5", ), 
        y = "demeaned_return", 
        start = 433, 
        end = nrow(data))

results_5 = list(BF_HA_H0, BF_HA_H0_log, probs, product_pred)

save(results_5, file = "results_5.RData")

###  DAILY VOLUME ##################################################

# j = 25 means a start date of jan 1

data  = analysis_1_df_daily %>% 
  select(-c(return, date))
data = na.omit(data);

T1 = Sys.time()
res_daily_vol = my_LPDS(data = data, 
                    niter = 20000, 
                    nburn = 5000, 
                    nthin = nthin, 
                    vars_to_keep = c("volume", "return_SPY", "return_SPY_1", "return_SPY_2", "rfr", "CAPM"), 
                    y = "volume", 
                    start = 25, 
                    end = 46,
                    sv = F);
T2 = Sys.time();
(T2-T1);
save(res_daily_vol, file = "res_daily_vol.RData")

###  HOURLY VOLUME ##################################################

# j = 37 means a start date of jan 1

data = analysis_1_df_60 %>% 
  select( -c(return, date));
data = na.omit(data);

T1 = Sys.time()

res_60_vol = my_LPDS(data = analysis_1_df_60, 
                        niter = niter, 
                        nburn = nburn, 
                        nthin = nthin, 
                        vars_to_keep = c("volume", "return_SPY", "return_SPY_1", "return_SPY_2", "rfr", "CAPM"), 
                        y = "volume", 
                        start = 25, 
                        end = 46,
                        sv = F);
T2 = Sys.time();
(T2-T1);
save(res_60_vol, file = "res_60_vol.RData")

###  10-MINUTE VOLUME ##################################################

# j = 217 means a start date of jan 1

data = analysis_1_df_10 %>%
  select ( -c(date, return));
data = na.omit(data);

T1 = Sys.time()
res_10_vol = my_LPDS(data = analysis_1_df_10, 
                        niter = niter, 
                        nburn = nburn, 
                        nthin = nthin, 
                        vars_to_keep = c("volume", "return_SPY", "return_SPY_1", "return_SPY_2", "rfr", "CAPM"), 
                        y = "volume", 
                        start = 25, 
                        end = 46,
                        sv = F);
T2 = Sys.time();
(T2-T1);
save(res_10_vol, file = "res_10_vol.RData")

### 5-MINUTE VOLUME #######################################################


data = analysis_1_df_5 %>%
  select ( -c(date, return));
data = na.omit(data);

# j = 433 means a start date of jan 4

res_5_vol = my_LPDS(data = analysis_1_df_5, 
        niter = niter, 
        nburn = nburn, 
        nthin = nthin, 
        vars_to_keep = c("demeaned_return", "return_SPY", "return_SPY_1", "return_SPY_2", "return_SPY_3", "return_SPY_4", "return_SPY_5", "return_lag_1", "return_lag_2", "return_lag_4", "return_lag_5", ), 
        y = "demeaned_return", 
        start = 433, 
        end = nrow(data))

results_5 = list(BF_HA_H0, BF_HA_H0_log, probs, product_pred)

save(results_5, file = "results_5.RData")






