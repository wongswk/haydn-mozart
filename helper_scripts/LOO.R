# This script does LOO and variants by calling LOO_functions.R. 

# Load functions
source(paste0(home_path, "helper_scripts/LOO_functions.R"))
#______________________________________________________________________________________________________________________________________________________________________________
#______________________________________________ Run LOO feature selection and composer classification    ______________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________________________

# Specify an extra_string for naming output files
if (simple == T) {
  extra_string = "_simple"
} else {
  extra_string = ""
}

# Set up CV folds for variants of LOO 
if (LOO_type == "LOO") {
  # regular leave-one-out (LOO) 
  LOO_inds = lapply(1:nrow(Feature), function(x) x)
} else {
  if (LOO_type == "LOQO") {
    # leave-one-quartet out (LOQO)
    quartet_names = unlist(lapply(1:nrow(LOO_cats), function(i) rep(LOO_cats$quartet_name[i], LOO_cats$num_mvmts[i])))
    LOO_inds = lapply(1:length(unique(quartet_names)), function(i) which(quartet_names == unique(quartet_names)[i]))
  }
}

if (parallel) {
  LOO_results = mclapply(LOO_inds, function(x) LOO_ICM_i(Feature, x, threshold =  filter_threshold, seeds = random_seeds), mc.cores = num_cores)
} else {
  LOO_results = lapply(LOO_inds, function(x) LOO_ICM_i(Feature, x, threshold =  filter_threshold, seeds = random_seeds))
}

parse_results(Feature, LOO_results, filename = paste0("_scale", prior.scale, "_df", prior.df, "_thresh", filter_threshold, "_", LOO_type, extra_string, "_", dataset))
