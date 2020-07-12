# This script produces some plots, tables, and other summaries of results on the HM107 and HM285 datasets. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/")

# If these packages are not already installed, they will be automatically installed here. 
packages = c("RColorBrewer", "ggplot2", "ggpubr", "stringr")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Set various hyperparameter values (for reading in files that were saved from previous scripts)
filter_threshold = 1  # no correlation filtering will be performed on the feature set prior to running the random ICM algorithm 
prior.scale = .6      # scale for Cauchy prior in bayesglm function in arm package 
prior.df = 1          # df for Cauchy prior in bayesglm function in arm package 
simple = F            # if T, we will use only "simple" features (basic and interval features) for feature selection and classification
LOO_type = "LOO"      # if LOO_type = "LOO", regular leave-one-out CV will be done. If LOO_type = "LOQO", leave-one-quartet-out CV will be done. 
extra_str = ""

# Read in dataframes
Feature_HM285 = read.csv(paste0(output_path, "Feature_HM285.csv"))[,-1] # feature set
Feature_HM107 = read.csv(paste0(output_path, "Feature_HM107.csv"))[,-1] # feature set
filename = paste0("_scale", prior.scale, "_df", prior.df, "_thresh", filter_threshold, "_", LOO_type, extra_str)
filename_HM285 = paste0(filename, "_HM285")
filename_HM107 = paste0(filename, "_HM107")
LOO_HM285 = read.csv(paste0(output_path, "predictions", filename_HM285, ".csv"))
LOO_HM107 = read.csv(paste0(output_path, "predictions", filename_HM107, ".csv"))


# Load functions
source(paste0(home_path, "helper_scripts/plot_functions.R"))


# Make confusion matrix for each dataset 
mat_HM285 = make_confusion_mat(Feature_HM285, LOO_HM285)
mat_HM107 = make_confusion_mat(Feature_HM107, LOO_HM107)
write.csv(mat_HM285, paste0(output_path, "confusion_matrix", filename_HM285, ".csv"))
write.csv(mat_HM107, paste0(output_path, "confusion_matrix", filename_HM107, ".csv"))

# Summarize selected features over the folds for HM285
vars = parse_vars_txt(paste0(output_path, "variables", filename_HM285, ".txt")) 

# Get category of each feature in HM285
names = names(Feature_HM285)[-1]
categories = c("Basic", "Interval", "Expo", "Dev", "Recap")
inds_sonata = lapply(categories[3:5], function(str) grep(str, names))
inds_interval = unique(c(grep("int", names), grep("m3", names)))
names[inds_sonata[[1]]] = rep("Exposition", length(inds_sonata[[1]]))
names[inds_sonata[[2]]] = rep("Development", length(inds_sonata[[2]]))
names[inds_sonata[[3]]] = rep("Recapitulation", length(inds_sonata[[3]]))
names[inds_interval] = rep(categories[2], length(inds_interval))
names[-c(unlist(inds_sonata), inds_interval )] = rep(categories[1], length(names) - length(c(unlist(inds_sonata), inds_interval)))

# Summarize representation of selected features in HM285 by category 
freq_vars = table(unlist(vars))
sort(freq_vars, decreasing = T)
cat(capture.output(print(sort(freq_vars, decreasing = T)), 
                   file = paste0(output_path, "Variables_folds", filename_HM285, ".txt")))
make_barplot()

#Make plot of estimated probabilities 
make_probability_plot()




