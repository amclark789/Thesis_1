my_LPDS = function(data, start, end, niter = 20000, nburn = 5000, nthin = 2, vars_to_keep, y , sv = F,ncores = 4){
  
  require(shrinkTVP)
  require(doParallel)
  require(foreach)
  require(RhpcBLASctl)
  require(tidyverse)
  require(data.table)
  
  # if( !is.integer(start)){stop("start must be an integer")}
  # if( !is.integer(end)){stop("end must be an integer")}
  # if( !is.integer(niter)){stop("niter must be an integer")}
  # if( !is.integer(nburn)){stop("nburn must be an integer")}
  # if( !is.integer(nthin)){stop("nthin must be an integer")}
  # if( !is.character(vars_to_keep)){stop("vars_to_keep must be a vector of column names")}
  # if( !is.character(y)){stop("y must be a character")}
  
  
  formula = paste(y, " ~ .", sep = "")
  formula = as.formula(formula)
  
  ncores = ncores
  cl = makeCluster(ncores)
  registerDoParallel(cl)
  
  
  probs <<- foreach ( t = start:end, 
                      .packages = c("RhpcBLASctl", "doParallel", "shrinkTVP", "tidyverse", "graphics", "data.table"), 
                      .combine = "rbind", 
                      .errorhandling = "pass") %dopar% {
                        
                        # PRINT T
                        print(t)
                        
                        # SET NUMBER OF THREADS
                        blas_set_num_threads(1)
                        
                        # SET SEED
                        set.seed(t)
                        
                        # CREATE DATAFRAMES TO ESTIMATE MODEL ON 
                        data_hA = data[1:t, ]
                        data_h0 = data[1:t, vars_to_keep ]
                        
                        # CREATE TEST DATA 
                        data_test_hA = data[t+1,]
                        data_test_h0 = data[t+1, vars_to_keep]
                        
                        # RUN MODELS
                        model_hA = shrinkTVP(niter = niter, nburn = nburn, nthin = nthin, data = data_hA, formula = formula, sv = sv)
                        model_h0 = shrinkTVP(niter = niter, nburn = nburn, nthin = nthin, data = data_h0, formula = formula, sv = sv)
                        
                        
                        # pred_dens_hA = eval_pred_dens(mod = model_hA, x = data_test_hA[,y], data_test = data_test_hA, log = F)
                        # pred_dens_h0 = eval_pred_dens(mod = model_h0, x = data_test_h0[,y],data_test = data_test_h0, log = F)
                        
                        
                        # OBTAIN FORECASTS OF THE DATA
                        # SHOULD OBTAIN NUMBER OF FORECASTS EQUAL TO (NITER-NBURN)/2
                        g_hA = forecast_shrinkTVP(mod = model_hA, newdata = data_test_hA, n.ahead = 1)
                        g_h0 = forecast_shrinkTVP(mod = model_h0, newdata = data_test_h0, n.ahead = 1)
                        
                        # DEFINE BREAKS
                        max = c(max(g_hA$y_pred), max(g_h0$y_pred))
                        min = c(min(g_hA$y_pred), min(g_h0$y_pred))
                        breaks2 = seq(from = (min(min)-1), to = (max(max)+1), by = .001)
                        
                        # GET THE PREDICTIONS AS DATAFRAMES
                        r_hA = as.data.frame(g_hA$y_pred)
                        r_h0 = as.data.frame(g_h0$y_pred)
                        
                        # CREATE HISTOGRAM OF THOSE PREDICTIONS, THIS IS THE PDF OF THE NEXT VALUE BASED ON THE MODEL
                        hist_hA = hist(r_hA$V1, breaks = breaks2, freq = F)
                        hist_h0 = hist(r_h0$V1, breaks = breaks2, freq = F)
                        
                        # OBTAIN THE SUMS OF THOSE TO DEFLATE WITH LATER
                        sum_dens_hA = sum(hist_hA$density)
                        sum_dens_h0 = sum(hist_h0$density)
                        
                        # OBTAIN THE ACTUAL NEXT VALUE
                        actual_val = data_test_h0[,y]
                        
                        # CREATE A DATAFRAME OF EACH HISTOGRAM
                        hist_data_h0 = data.frame(hist_h0$breaks[1:(length(hist_h0$breaks)-1)])
                        hist_data_h0$counts = hist_h0$counts
                        hist_data_h0$density = hist_h0$density
                        names(hist_data_h0) = c("breaks", "count_old", "density")
                        
                        hist_data_hA = data.frame(hist_hA$breaks[1:(length(hist_h0$breaks)-1)])
                        hist_data_hA$counts = hist_hA$counts
                        hist_data_hA$density = hist_hA$density
                        names(hist_data_hA) = c("breaks", "count_old", "density")
                        
                        # CREATE NEW DF WITH THE ACTUAL VALUE
                        preds_new_h0 = rbind(as.data.frame(g_h0$y_pred), actual_val)
                        preds_new_hA = rbind(as.data.frame(g_hA$y_pred), actual_val)
                        
                        
                        # CREATE HISTOGRAM WITH NEW DATA           
                        hist_new_h0 = hist(preds_new_h0$V1, breaks = breaks2)
                        hist_data_new_h0 = as.data.frame(hist_new_h0$breaks[1:(length(hist_new_h0$breaks)-1)])
                        hist_data_new_h0$counts = hist_new_h0$counts
                        names(hist_data_new_h0) = c("breaks", "count_new")
                        
                        hist_new_hA = hist(preds_new_hA$V1, breaks = breaks2)
                        hist_data_new_hA = as.data.frame(hist_new_hA$breaks[1:(length(hist_new_hA$breaks)-1)])
                        hist_data_new_hA$counts = hist_new_hA$counts
                        names(hist_data_new_hA) = c("breaks", "count_new")
                        
                        # CREATE FINAL DF WITH HISTOGRAMS AND DENSITIES
                        hist_data_final_hA = merge(hist_data_hA, hist_data_new_hA, by = "breaks")
                        for(j in 2:(nrow(hist_data_final_hA) - 1)){ 
                          x = ((hist_data_final_hA$density[j-1] + hist_data_final_hA$density[j+1])/2)
                          if(hist_data_final_hA$density[j] ==0){hist_data_final_hA$density[j] = x } }
                        for(j in (nrow(hist_data_final_hA) - 1):2){ 
                          x = ((hist_data_final_hA$density[j-1] + hist_data_final_hA$density[j+1])/2)
                          if(hist_data_final_hA$density[j] ==0){hist_data_final_hA$density[j] = x } }
                        x = sum_dens_hA/sum(hist_data_final_hA$density)
                        hist_data_final_hA$density = hist_data_final_hA$density*x
                        hist_data_final_hA$count_change = hist_data_final_hA$count_new - hist_data_final_hA$count_old 
                        hist_data_final_hA =    filter(hist_data_final_hA, count_change==1)
                        prob_density_hA = (hist_data_final_hA$density[1])/sum_dens_hA
                        
                        
                        
                        hist_data_final_h0 = merge(hist_data_h0, hist_data_new_h0, by = "breaks") 
                        for(j in 2:(nrow(hist_data_final_h0)-1)){ 
                          x = (hist_data_final_h0$density[j-1] + hist_data_final_h0$density[j+1])/2
                          if(hist_data_final_h0$density[j] ==0){hist_data_final_h0$density[j] = x } }
                        for(j in (nrow(hist_data_final_h0) - 1):2){ 
                          x = ((hist_data_final_h0$density[j-1] + hist_data_final_h0$density[j+1])/2)
                          if(hist_data_final_h0$density[j] ==0){hist_data_final_h0$density[j] = x } }
                        x = sum_dens_h0/sum(hist_data_final_h0$density)
                        hist_data_final_h0$density = hist_data_final_h0$density*x
                        hist_data_final_h0$count_change = hist_data_final_h0$count_new - hist_data_final_h0$count_old
                        hist_data_final_h0 =    subset(hist_data_final_h0, count_change==1)
                        prob_density_h0 = (hist_data_final_h0$density[1])/sum_dens_h0
                        
                        
                        probs = cbind(prob_density_h0, prob_density_hA)
                        
                        probs1 = as.data.frame(probs)
                        
                        
                        
                        return(probs)
                        
                        
                        remove(data_hA, data_h0, probs1,
                               data_test_hA, data_test_h0, g_hA, g_h0, max, min, breaks2, 
                               r_hA, r_h0,hist_hA, hist_h0, sum_dens_hA, sum_dens_h0, 
                               actual_val, hist_data_hA, hist_data_h0,  preds_new_hA, 
                               preds_new_h0, hist_new_h0, hist_new_hA, hist_data_new_h0, 
                               hist_data_final_h0, hist_data_final_hA, prob_density_hA, prob_density_h0, hist_data_new_hA, model_h0, model_hA, x) 
                        
                        
                        
                        
                        
                      }
  stopCluster(cl)
  # product_pred <<- apply(FUN = log, MARGIN = 1, probs)
  
  probs <<- as.data.frame(probs)
  log_probs <<- log(probs)
  
  
  BF_HA_H0_log <<- sum(log_probs[,2]) -  sum(log_probs[,1])
  BF_HA_H0_text <<- paste("The Bayes Factor is equal to 10^", BF_HA_H0_log, sep = "")
  
  
  results = list(BF_HA_H0_log, BF_HA_H0_text, probs, log_probs)
  
  names(results) = c("BF_HA_H0_log", "BF_HA_H0_text", "probs", "log_probs")
  
  
  return(results)
  
}
