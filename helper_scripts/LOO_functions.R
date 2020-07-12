# Functions to be called by the LOO.R script. 

# Load libraries 
library(arm)
library(parallel)

#This function does random ICM, given the training data frame and a random seed to initialize the algorithm.
#We only use BIC here, but the function enables the usage of AIC or BIC through the criterion argument. 
random_ICM=function(train_data, criterion="BIC", seed) {
  
  if (criterion == "BIC") {
    penalty_string = "2 * (log(n) - 2) * (length(index))"
  } else if (criterion == "AIC"){
    penalty_string = "0"
  }
  
  set.seed(seed)
  
  varlist = c()
  varnames = names(train_data)
  numF = ncol(train_data)
  n = nrow(train_data)
  minCrit = Inf
  noChange = F
  while (!noChange) {
    noChange = T
    for (i in sample(2:numF)) { 
      
      if (i %in% varlist & length(varlist) > 1) {
        index = c(1, varlist[varlist != i]) 
        train.set = train_data[, index]
        
        fit = bayesglm(M0 ~., data = train.set, family = binomial, prior.scale = prior.scale, prior.df = prior.df)
        
        if (fit$converged & fit$aic + eval(parse(text = penalty_string)) < minCrit) {
          minCrit = fit$aic + eval(parse(text = penalty_string))
          varlist = varlist[varlist != i]
          print(paste0(round(minCrit), " ", paste0(varlist, collapse = " ")))
          best.model = fit
          noChange = F
        }
        
      } else if (!i %in% varlist) {
        index = c(1, varlist, i) 
        train.set = train_data[, index]
        
        fit = bayesglm(M0 ~., data = train.set, family = binomial, prior.scale = prior.scale, prior.df = prior.df)
        
        
        if (fit$converged & fit$aic + eval(parse(text=penalty_string))  < minCrit) {
          minCrit = fit$aic + eval(parse(text = penalty_string))
          varlist = c(varlist, i)
          best.model = fit
          noChange = F
        }    
        
      }
    }
  }
  index=c(1, varlist)
  return(list(minCrit, index))
}


#This function calls random ICM multiple times, once for each randomly generated seed. 
run_random_ICM = function(train_data, criterion = "BIC", seeds, parallel = F) {
  if (parallel) {
    results = mclapply(seeds, function(x) random_ICM(train_data, criterion, x), mc.cores = num_cores)
  } else {
    results = lapply(seeds, function(x) random_ICM(train_data, criterion, x))
  }
  return(results)
}


# This function performs modified correlation ranking to reduce the feature set, if cor_threshold < 1. 
filter_vars = function(train, cor_threshold = .5) {
  keep = c()
  group_cors = cor(train[,-1])
  cors = abs(apply(train[,-1], 2, function(x) cor(train[, 1], x)))
  keepers = c(2:ncol(train))[sort_f(corf = group_cors, goodf=cors, cor_threshold = cor_threshold)]
  keep = c(keep, keepers)
  
  keep=c(1, keep)
  return(keep)
}

#This function is a helper function for the filter_vars function
sort_f=function(corf, goodf, cor_threshold){
  keep = rep(TRUE, length(goodf))
  for (i in order(goodf, decreasing = T)) {
    if (!keep[i]) {
      next
    }
    
    keep[abs(corf[i, ]) > cor_threshold] = FALSE
    keep[i] = TRUE
    
  }
  return(keep)
}


#This is a general function that converts probabilities to classes for binary classification problems and computes the classification accuracy, sensitivity, and specificity. 
#There are numerous ways to set thresholds that are built-in to this function.
classify_f = function(response, pred, lev=c(-1,1), try=NULL, sep.try=NULL, return_class=F){
  if (is.null(try) & (is.null(sep.try))){ #This is for the case that you have predictions (not probabilities) already and only need to calculate the accuracy, sensitivity, and specificity.
    response = factor(response, levels = lev)
    pred = factor(pred, levels = lev)
    tab = table(response, pred)
    acc = (tab[2, 2] + tab[1, 1])/sum(tab)
    spec = tab[1, 1]/sum(tab[1, ])
    sens = tab[2, 2]/sum(tab[2, ])
    return(list(acc, sens, spec))
  } else if (!is.null(try)){ #This is for the case that you have predicted probabilities and a range of cutoff values you want to try
    ACC=c(); SENS=c(); SPEC=c()
    for (j in try) {
      class = ifelse(pred >= j, lev[2], lev[1])
      response = factor(response, levels = c(lev[1], lev[2]))
      class = factor(class, levels = c(lev[1], lev[2]))
      tab = table(response, class)
      acc = (tab[2, 2] + tab[1, 1])/sum(tab)
      spec = tab[1, 1]/sum(tab[1, ])
      sens = tab[2, 2]/sum(tab[2, ])
      ACC = c(ACC, acc)
      SENS = c(SENS, sens)
      SPEC = c(SPEC, spec)
      
    }
    return(list(ACC, SENS, SPEC))
  } else { #This is for the case that you have a separate desired cutoff per predicted probability
    class = unlist(lapply(1:length(pred), function(x) ifelse(pred[x] >= sep.try[x], lev[2], lev[1])))
    response = factor(response, levels = c(lev[1], lev[2]))
    class = factor(class, levels = c(lev[1], lev[2]))
    tab = table(response, class)
    acc = (tab[2, 2] + tab[1, 1])/sum(tab)
    spec = tab[1, 1]/sum(tab[1, ])
    sens = tab[2, 2]/sum(tab[2, ])
    if (return_class == T){
      return(list(acc, sens, spec, class))
    } else {
      return(list(acc, sens, spec))
    }
  }
}

#If there are multiple "best" cutoffs in classification, this function will return the one that's closest to .5 
one_cutoff = function(cutoff_vec) {
  if (length(cutoff_vec) > 1){
    dist = abs(cutoff_vec - .5)
    cutoff_ind = which(dist == min(dist))[1]
    cutoff = cutoff_vec[cutoff_ind]
  } else {
    cutoff = cutoff_vec
  }
  return(cutoff)
}

#This function trains and tests bayesglm and obtains the best probability threshold from training
bayesglm_f=function(train.set, test.set){
  train.set$M0 = as.factor(train.set$M0)
  test.set$M0 = as.factor(test.set$M0)
  
  log.fit = bayesglm(M0~., data = train.set, family=binomial, prior.scale = prior.scale, prior.df = prior.df)
  
  test.probs = predict(log.fit, type = "response", newdata=test.set)
  train.probs = predict(log.fit, type = "response", newdata=train.set)
  
  if (dataset == "HM107") {
    # HM107 has approximately equal composer class sizes, so use the standard cutoff of 0.5. 
    try = .5
  } else {
    # HM285 has imbalanced composer classes, so the cutoff will be treated as a parameter to be tuned in training. 
    try = c(1:100) * .01
  }
  train.results = classify_f(train.set$M0, train.probs, lev=c(0, 1), try = try)
  
  return(list(train.results, test.probs, log.fit))
}

#This function calls bayesglm for LOO
call_bayesglm_f = function(Data, i, index){
  LOO_ind = LOO_inds[[i]]
  train = Data[-LOO_ind, index]
  test = Data[LOO_ind, index]
  results = bayesglm_f(train, test)
  return(results)
}

#This function runs feature selection on each LOO training fold
LOO_ICM_i = function(Features = Feature, LOO_ind, threshold = .5, seeds){
  test.ID = LOO_ind
  train.set = Features[-test.ID, ]
  
  if (threshold != 1) {
    index = filter_vars(train.set, threshold)
    train.set = train.set[, index]
  }
  
  train.set$M0 = as.factor(train.set$M0)
  
  ICM_results = run_random_ICM(train.set, criterion="BIC", seeds = seeds)
  
  BICs = lapply(ICM_results, "[[", 1)
  vars_index = lapply(ICM_results, "[[", 2)
  
  if (threshold != 1) {
    vars_index = lapply(vars_index, function(x) index[x]) #gets the indices of selected variables (in the original Feature set, not the reduced feature set)
  }
  
  results = lapply(1:length(seeds), function(x) list(BICs[[x]], vars_index[[x]]))
  
  return(results)
}




#This function parses the lists generated from calling LOO_ICM, writes an output of the selected variables from each fold, and returns the testing results 
parse_results = function(Features = Feature, LOO_results, filename) {
  Features_M0 = factor(Features$M0, levels = c(0, 1))
  num_trials = length(LOO_results[[1]])
  num_folds = length(LOO_results)
  
  BICs = c()
  VARs = c()
  for (i in 1:num_trials){
    trial_i = lapply(LOO_results, "[[", i)
    BICs = c(BICs, unlist(lapply(trial_i, "[[", 1)))
    VARs = c(VARs, lapply(trial_i, "[[", 2))
  }
  BIC_mat = data.frame(matrix(BICs, nrow = num_folds, ncol = num_trials))
  names(BIC_mat) = paste0("Trial", c(1:num_trials))
  BIC_min = apply(BIC_mat, 1, min)
  BIC_inds = lapply(BIC_min, function(j) which(BIC_mat == j, arr.ind=T)[1, ])
  
  #From 10 trials of random ICM, chooses the variables from the trial that achieved minimum BIC
  get_vars = lapply(1:num_folds, function(i) LOO_results[[i]][[BIC_inds[[i]][2]]][[2]] )
  names_vars = lapply(get_vars, function(x) names(Features)[sort(x)])
  
  results = lapply(1:num_folds, function(i) call_bayesglm_f(Features, i, get_vars[[i]])) #refits "best model" for each training fold and gets corresponding testing fold prediction
  cat(capture.output(print(names_vars), file = paste0(output_path, "variables", filename, ".txt") ))
  
  test.pred = lapply(results, "[[", 2) #gets test probabilities
  train.results = lapply(results, "[[", 1)
  
  if (LOO_type == "LOO") {
    # regular LOO
    test.pred = unlist(test.pred)
    train.accuracy = sapply(train.results, function(x) max(x[[1]]))
    
    if (dataset == "HM107") {
      train.cutoff = rep(.5, length(Features$M0))
      test.results = classify_f(Features$M0, test.pred, lev=c(0,1), sep.try = train.cutoff, return_class = T)
     
    } else {
      cutoff_vec = c(1:100) * .01
      train.cutoff = sapply(train.results, function(x) one_cutoff(cutoff_vec[which(x[[1]] == max(x[[1]]))])) #gets best cutoff (one that maximizes training classification accuracy) from each training fold
      test.results = classify_f(Features$M0, test.pred, lev=c(0,1), sep.try = train.cutoff, return_class = T)
    }
    results_df = data.frame(train.accuracy = train.accuracy, train.cutoff = train.cutoff, 
                            test.pred = test.pred, test.class = test.results[[4]])
    acc_df = data.frame(Test_Accuracy = test.results[[1]], Test_Haydn_Accuracy = test.results[[2]], Test_Mozart_Accuracy = test.results[[3]])
    row.names(acc_df) = c(LOO_type)
    
  } else {
    # LOQO
    if (dataset == "HM285") {
      cutoff_vec = c(1:100) * .01
      train.cutoff = sapply(train.results, function(x) one_cutoff(cutoff_vec[which(x[[1]] == max(x[[1]]))])) #gets best cutoff (one that maximizes training classification accuracy) from each training fold
      train.cutoff = unlist(lapply(1:num_folds, function(i) rep(train.cutoff[i], length(LOO_inds[[i]]))))
      train.accuracy = sapply(train.results, function(x) max(x[[1]]))
      train.accuracy = unlist(lapply(1:num_folds, function(i) rep(train.accuracy[i], length(LOO_inds[[i]]))))
        
      test.results = lapply(1:num_folds, function(i) classify_f(Features$M0[LOO_inds[[i]]], test.pred[[i]], lev = c(0, 1), sep.try = rep(train.cutoff[i], length(LOO_inds[[i]])), return_class = T))
      
      # extract test results
      test.accuracy = sapply(test.results, "[[", 1)
      test.class = unlist(lapply(test.results, "[[", 4))
      test.pred = unlist(test.pred)
      
      # final accuracy is mean over the testing folds
      test.acc = mean(sapply(test.results, "[[", 1))
      # reformat test accuracy to be of appropriate length for writing to csv 
      test.accuracy = unlist(lapply(1:num_folds, function(i) rep(test.accuracy[i], length(LOO_inds[[i]]))))
      
      results_df = data.frame(train.accuracy = train.accuracy, train.cutoff = train.cutoff, 
                              test.accuracy = test.accuracy, test.pred = test.pred, test.class = test.class)
      acc_df = data.frame(Test_Accuracy = test.acc)
      row.names(acc_df) = c(LOO_type)
      
      
    }
  }
  
  write.csv(acc_df, paste0(output_path, "accuracy", filename, ".csv"))
  write.csv(results_df, paste0(output_path, "predictions", filename, ".csv"), row.names = F)
  
}

# This function fits one model of composer (on all movements), following the same process as before but without LOO or other CV. 
# The purpose is to fit one model that can be interpreted.
Full_results = function(train.set, threshold, seeds) {
  if (threshold != 1) {
    index = filter_vars(train.set, threshold)
    train.set = train.set[, index]
  } 
  
  ICM_results = run_random_ICM(train.set, criterion = "BIC", seeds, parallel = T)
  
  BICS = unlist(lapply(ICM_results, "[[", 1))
  vars_index = lapply(ICM_results, "[[", 2)
  
  best_ind = which(BICS == min(BICS))[1]
  best_BIC = BICS[best_ind]
  
  if (threshold != 1) {
    vars_index = lapply(vars_index, function(x) index[x]) #gets the indices of selected variables (in the original Feature set, not the reduced feature set)
  }
  
  best_vars = vars_index[[best_ind]]
  
  full_results = bayesglm_f(train.set[, best_vars], train.set[, best_vars])
  full_probs = full_results[[2]]
  full_model = full_results[[3]]
  
  return(list(full_model, best_BIC, full_probs, vars_index, best_vars))
}
  
