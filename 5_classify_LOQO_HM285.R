# This script classifies the composer of each movement in the HM285 dataset using the leave-one-quartet (LOQO) CV scheme,
# by calling the main LOO.R script with settings defined for this task. 
# For each CV fold, 
#     1) feature selection (random ICM for 10 trials) identifies a subset of features
#     2) those features are used as predictors in a Bayesian logistic regression model (from the arm package)
#     3) the predicted probabilities from the model are converted to classes. 
#
# Note that this script uses task parallelism via the package "parallel" to reduce runtime. Set the number of CPU cores to use on line 22.
# To disable parallelism, set "parallel = F" in line 20. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/")

# If these packages are not already installed, they will be automatically installed here. 
packages = c("arm", "parallel")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Specify dataset, whether task parallelism is to be used, and if so, how many cores.
dataset = "HM285"
parallel = T
if (parallel) {
  num_cores = 8
}

# Set various hyperparameter values 
filter_threshold = 1  # no correlation filtering will be performed on the feature set prior to running the random ICM algorithm 
prior.scale = .6      # scale for Cauchy prior in bayesglm function in arm package 
prior.df = 1          # df for Cauchy prior in bayesglm function in arm package 
simple = F            # if T, we will use only "simple" features (basic and interval features) for feature selection and classification
LOO_type = "LOQO"     # if LOO_type = "LOO", regular leave-one-out CV will be done. If LOO_type = "LOQO", leave-one-quartet-out CV will be done. 

# Read in feature dataframe
Feature = read.csv(paste0(output_path, "Feature_", dataset, ".csv"))[,-1]

if (LOO_type == "LOQO") {
  # For LOQO CV, read in the quartet categories. 
  LOO_cats = read.csv(paste0(output_path, "categories_", dataset, ".csv"))
}

if (dataset == "HM285") {
  #round(runif(10) * 1000000)
  random_seeds = c(690893, 965097, 659011, 253073, 834997, 386972, 190566, 257693, 181779, 329323) # save ten random seeds, which will be used for the ten repetitions of random ICM
  
  bad_vars = grep("voicepair_int_dist_0_2.4", names(Feature)) # This feature causes numerical problems (due to a large magnitude of coefficient of variation) for the bayesglm fitting, so it will be removed.
  
} else if (dataset == "HM107") {
  #round(runif(10) * 1000000)
  random_seeds = c(497699, 717619, 991906, 380035, 777445, 934705, 212143, 651674, 125555, 267221) # save ten random seeds, which will be used for the ten repetitions of random ICM
  
  bad_vars = grep("voicepair_int_dist_1_1.2", names(Feature)) # This feature causes numerical problems (due to a large magnitude of coefficient of variation) for the bayesglm fitting, so it will be removed.
}

if (simple == T) {
  sonata_vars = c(grep("Recap", colnames(Feature)), grep("Dev", colnames(Feature)), grep("Expo", colnames(Feature)))
  bad_vars = c(bad_vars, sonata_vars)
}

Feature = Feature[, -bad_vars]

source(paste0(home_path, "helper_scripts/LOO.R"))
