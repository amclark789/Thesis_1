library(pacman)

p_load(
  tidyverse, modeldata, skimr, janitor, summarytools,
  tidymodels, magrittr, glmnet, ISLR, data.table
)



load(file ="C:/Users/aclar/Dropbox/Anthony_Clark_Thesis/Analysis/0_ingest/out_data/analysis_1_df_1.RData")


analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(date = as.POSIXct(minute, origin = "1970-01-01", tz = "EST")) %>%
  mutate(subs_per_min = as.numeric(subs_per_min)) %>%
  separate(date, into = c("date", "hour"), sep = " ") %>%
  separate(hour, into = c("hour",NA,  NA), sep =":") %>%
  mutate(subs_per_min = as.numeric(subs_per_min))

#analysis_1_df_1 = analysis_1_df_1 %>%
#  select(-c("Symbol"))

analysis_1_df_1 = analysis_1_df_1 %>%
  # LAG COMS PER MINUTE
  mutate(coms_lag_1 = lag(coms_per_min, 1)) %>%
  mutate(coms_lag_2 = lag(coms_per_min, 2)) %>%
  mutate(coms_lag_3 = lag(coms_per_min, 3)) %>%
  mutate(coms_lag_4 = lag(coms_per_min, 4)) %>%
  mutate(coms_lag_5 = lag(coms_per_min, 5)) %>%
  mutate(coms_lag_6 = lag(coms_per_min, 6)) %>%
  mutate(coms_lag_7 = lag(coms_per_min, 7)) %>%
  mutate(coms_lag_8 = lag(coms_per_min, 8)) %>%
  mutate(coms_lag_9 = lag(coms_per_min, 9)) %>%
  mutate(coms_lag_10 = lag(coms_per_min, 10)) %>%  
  # LAG HASHTAGS PER MINUTE
mutate(hash_lag_1 = lag(hashtags_per_min, 1)) %>%
mutate(hash_lag_2 = lag(hashtags_per_min, 2)) %>%
mutate(hash_lag_3 = lag(hashtags_per_min, 3)) %>%
mutate(hash_lag_4 = lag(hashtags_per_min, 4)) %>%
mutate(hash_lag_5 = lag(hashtags_per_min, 5)) %>% 
mutate(hash_lag_6 = lag(hashtags_per_min, 6)) %>%
mutate(hash_lag_7 = lag(hashtags_per_min, 7)) %>%
mutate(hash_lag_8 = lag(hashtags_per_min, 8)) %>%
mutate(hash_lag_9 = lag(hashtags_per_min, 9)) %>%
mutate(hash_lag_10 = lag(hashtags_per_min, 10)) %>%
  # LAG CASHTAGS PER MINUTE
 mutate(cash_lag_1 = lag(cashtags_per_min, 1)) %>%
 mutate(cash_lag_2 = lag(cashtags_per_min, 2) )%>%
 mutate(cash_lag_3 = lag(cashtags_per_min, 3) )%>%
 mutate(cash_lag_4 = lag(cashtags_per_min, 4) )%>%
 mutate(cash_lag_5 = lag(cashtags_per_min, 5) )%>%
 mutate(cash_lag_6 = lag(cashtags_per_min, 6) )%>%
 mutate(cash_lag_7 = lag(cashtags_per_min, 7) )%>%
 mutate(cash_lag_8 = lag(cashtags_per_min, 8) )%>%
 mutate(cash_lag_9 = lag(cashtags_per_min, 9) )%>% 
 mutate(cash_lag_10 = lag(cashtags_per_min, 10)) %>%  
  # LAG HASHTAGS AND CASHTAGS PER MINUTE
mutate(cash_hash_lag_1 = lag(cash_hash_per_min, 1)) %>%
mutate(cash_hash_lag_2 = lag(cash_hash_per_min, 2) )%>%
mutate(cash_hash_lag_3 = lag(cash_hash_per_min, 3) )%>%
mutate(cash_hash_lag_4 = lag(cash_hash_per_min, 4) )%>%
mutate(cash_hash_lag_5 = lag(cash_hash_per_min, 5) )%>%
mutate(cash_hash_lag_6 = lag(cash_hash_per_min, 6) )%>%
mutate(cash_hash_lag_7 = lag(cash_hash_per_min, 7) )%>%
mutate(cash_hash_lag_8 = lag(cash_hash_per_min, 8) )%>%
mutate(cash_hash_lag_9 = lag(cash_hash_per_min, 9) )%>%
mutate(cash_hash_lag_10 = lag(cash_hash_per_min, 10)) %>%
  # CREATE CHANGE IN COMMENTS PER  MINUTE, LAG IT
  mutate(change_coms = coms_per_min - coms_lag_1) %>%
  mutate(change_coms_1 = lag(change_coms, 1)) %>%
  mutate(change_coms_2 = lag(change_coms, 2)) %>%
  mutate(change_coms_3 = lag(change_coms, 3)) %>%
  mutate(change_coms_4 = lag(change_coms, 4)) %>%
  mutate(change_coms_5 = lag(change_coms, 5)) %>%
  mutate(change_coms_6 = lag(change_coms, 6)) %>%
  mutate(change_coms_7 = lag(change_coms, 7)) %>%
  mutate(change_coms_8 = lag(change_coms, 8)) %>%
  mutate(change_coms_9 = lag(change_coms, 9)) %>%
  mutate(change_coms_10 = lag(change_coms, 10)) %>%
  # CREATE CHANGE IN HASHTAGS PER MINUTE, LAG IT
mutate(change_hash = hashtags_per_min - hash_lag_1) %>%
mutate(change_hash_1 = lag(change_hash, 1)) %>%
mutate(change_hash_2 = lag(change_hash, 2)) %>%
mutate(change_hash_3 = lag(change_hash, 3)) %>%
mutate(change_hash_4 = lag(change_hash, 4)) %>%
mutate(change_hash_5 = lag(change_hash, 5)) %>%
mutate(change_hash_6 = lag(change_hash, 6)) %>%
mutate(change_hash_7 = lag(change_hash, 7)) %>%
mutate(change_hash_8 = lag(change_hash, 8)) %>%
mutate(change_hash_9 = lag(change_hash, 9)) %>%
mutate(change_hash_10 = lag(change_hash, 10)) %>%
  # CREATE CHANGE IN CASHTAGS, LAG IT
  mutate(change_cash = cashtags_per_min - cash_lag_1) %>%
  mutate(change_cash_1 = lag(change_cash, 1)) %>%
  mutate(change_cash_2 = lag(change_cash, 2)) %>%
  mutate(change_cash_3 = lag(change_cash, 3)) %>%
  mutate(change_cash_4 = lag(change_cash, 4)) %>%
  mutate(change_cash_5 = lag(change_cash, 5)) %>%
  mutate(change_cash_6 = lag(change_cash, 6)) %>%
  mutate(change_cash_7 = lag(change_cash, 7)) %>%
  mutate(change_cash_8 = lag(change_cash, 8)) %>%
  mutate(change_cash_9 = lag(change_cash, 9)) %>%
  mutate(change_cash_10 = lag(change_cash, 10)) %>%
  # CREATE CHANGE IN HASHTAGS AND CASHTAGS, LAG IT
mutate(change_hash_cash = cash_hash_per_min - cash_hash_lag_1) %>%
mutate(change_hash_cash_1 = lag(change_hash_cash, 1)) %>%
mutate(change_hash_cash_2 = lag(change_hash_cash, 2)) %>%
mutate(change_hash_cash_3 = lag(change_hash_cash, 3)) %>%
mutate(change_hash_cash_4 = lag(change_hash_cash, 4)) %>%
mutate(change_hash_cash_5 = lag(change_hash_cash, 5)) %>%
mutate(change_hash_cash_6 = lag(change_hash_cash, 6)) %>%
mutate(change_hash_cash_7 = lag(change_hash_cash, 7)) %>%
mutate(change_hash_cash_8 = lag(change_hash_cash, 8)) %>%
mutate(change_hash_cash_9 = lag(change_hash_cash, 9)) %>%
mutate(change_hash_cash_10 = lag(change_hash_cash, 10)) %>%
  # LAG RETURNS
  mutate(return_lag_1 = lag(return, 1)) %>%
  mutate(return_lag_2 = lag(return, 2)) %>%
  mutate(return_lag_3 = lag(return, 3)) %>%
  mutate(return_lag_4 = lag(return, 4)) %>%
  mutate(return_lag_5 = lag(return, 5)) %>%
  mutate(return_lag_6 = lag(return, 6)) %>%
  mutate(return_lag_7 = lag(return, 7)) %>%
  mutate(return_lag_8 = lag(return, 8)) %>%
  mutate(return_lag_9 = lag(return, 9)) %>%
  mutate(return_lag_10 = lag(return, 10)) %>%
  # LAG SUBS PER MINUTE
mutate(subs_lag_1 = lag(subs_per_min, 1)) %>%
mutate(subs_lag_2 = lag(subs_per_min, 2)) %>%
mutate(subs_lag_3 = lag(subs_per_min, 3)) %>%
mutate(subs_lag_4 = lag(subs_per_min, 4)) %>%
mutate(subs_lag_5 = lag(subs_per_min, 5)) %>%
mutate(subs_lag_6 = lag(subs_per_min, 6)) %>%
mutate(subs_lag_7 = lag(subs_per_min, 7)) %>%
mutate(subs_lag_8 = lag(subs_per_min, 8)) %>%
mutate(subs_lag_9 = lag(subs_per_min, 9)) %>%
mutate(subs_lag_10 = lag(subs_per_min, 10)) %>%
  # CREATE CHANGE IN SUBS PER MINUTE
  mutate(change_subs = subs_per_min - subs_lag_1) %>%
  mutate(change_subs_1 = lag(change_subs, 1)) %>%
  mutate(change_subs_2 = lag(change_subs, 2)) %>%
  mutate(change_subs_3 = lag(change_subs, 3)) %>%
  mutate(change_subs_4 = lag(change_subs, 4)) %>%
  mutate(change_subs_5 = lag(change_subs, 5)) %>%
  mutate(change_subs_6 = lag(change_subs, 6)) %>%
  mutate(change_subs_7 = lag(change_subs, 7)) %>%
  mutate(change_subs_8 = lag(change_subs, 8)) %>%
  mutate(change_subs_9 = lag(change_subs, 9)) %>%
  mutate(change_subs_10 = lag(change_subs, 10)) %>%
  # CREATE CHANGE IN VOLUME, LAG IT
mutate(change_volume = Volume - lag(Volume,1)) %>%
mutate(change_volume_1 = lag(change_volume, 1)) %>%
mutate(change_volume_2 = lag(change_volume, 2)) %>%
mutate(change_volume_3 = lag(change_volume, 3)) %>%
mutate(change_volume_4 = lag(change_volume, 4)) %>%
mutate(change_volume_5 = lag(change_volume, 5)) %>%
mutate(change_volume_6 = lag(change_volume, 6)) %>%
mutate(change_volume_7 = lag(change_volume, 7)) %>%
mutate(change_volume_8 = lag(change_volume, 8)) %>%
mutate(change_volume_9 = lag(change_volume, 9)) %>%
mutate(change_volume_10 = lag(change_volume, 10)) %>% 
  # LAG VOLUME
  mutate(volume_lag_1 = lag(Volume, 1)) %>%
  mutate(volume_lag_2 = lag(Volume, 2)) %>%
  mutate(volume_lag_3 = lag(Volume, 3)) %>%
  mutate(volume_lag_4 = lag(Volume, 4)) %>%
  mutate(volume_lag_5 = lag(Volume, 5)) %>%
  mutate(volume_lag_6 = lag(Volume, 6)) %>%
  mutate(volume_lag_7 = lag(Volume, 7)) %>%
  mutate(volume_lag_8 = lag(Volume, 8)) %>%
  mutate(volume_lag_9 = lag(Volume, 9)) %>%
  mutate(volume_lag_10 = lag(Volume, 10)) 
  
  


analysis_1_df_1 = analysis_1_df_1 %>%
  mutate(hour = as.numeric(hour)) %>%
  filter(hour >3) %>%
  filter(hour <21) %>%
  filter(date > "2020-12-31") %>%
  filter(date < "2021-2-10")
  
  
  
split = analysis_1_df_1 %>%initial_split(prop = .8)

data_train = split %>%
  training()
data_test = split %>%
  testing()


recipe =  recipe(return ~ ., data = data_train) %>%
  update_role(minute, new_role = "ID") %>%
  #step_dummy(all_nominal_predictors()) %>%
  #step_hyperbolic(all_numeric_predictors()) %>%
  #step_log(all_numeric_predictors()) %>%
  step_impute_mean(return) %>%
  step_impute_mean(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(date) %>%
  step_interact(~starts_with("date_"):all_numeric_predictors())


data_clean = recipe %>% prep() %>%juice()



# SET THE SEED
set.seed(1001)

##### SPECIFY MODEL
##### CHOOSE ELASTICNET

# DEFINE CROSS-VALIDATION STRATEGY
cv = data_train %>%
  vfold_cv(v = 2)

# CREATE VECTORS OF LAMBDAS AND ALPHAS
lambdas = 10 ^ seq(from = 5, to = -2, length = 50)
#alphas =  seq(from = 0, to = 1, by = 0.15)

# SPECIFY MODEL
model = linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# SPECIFY WORKFLOW
workflow = workflow() %>%
  add_recipe(recipe)%>%
  add_model(model)

# PERFORM CROSS VALIDATION AND MODEL TUNING
cv_lasso = 
  workflow %>%
  tune_grid(
    cv,
    grid = data.frame( penalty = lambdas),
    metrics = metric_set(rmse)
  )

# SHOW THE BEST ESTIMATES
cv_net %>% show_best

##### ESTIMATE THE FINAL MODEL ON THE TESTING DATA

# FINALIZE THE WORKFLOW, SELECT BEST MODEL FROM NFL_CV BASED ON RMSE
final_net = 
  workflow_net %>%
  finalize_workflow(select_best(cv_net, metric = "rmse"))

# FIT THE BEST MODEL TO THE DATA
final_fit_net = final_net %>%
  last_fit(nfl_split)

# COLLECT THE METRICS TO SEE HOW THE MODEL DID
final_fit_net %>% collect_metrics()



save()








############################################################### LINEAR REG

model = lm(data = analysis_1_df_1, return)













