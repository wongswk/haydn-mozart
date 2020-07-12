# For each dataset, this script will clean the data using clean_music_data.R then form an expanded feature set using "compute_expanded_music_features.R". 
# The feature set is expanded in that segment lengths m = 8, 9, 10, .., 18 are used for all segment features (as opposed to only m = 8, 10, 12, ..., 18). 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/S1/")
dir.create(output_path, showWarnings = F, recursive = T)

# If these packages are not already installed, they will be automatically installed here. 
packages = c("stringr", "Hmisc")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Run scripts for HM107 dataset
dataset = "HM107"
source(paste0(home_path, "helper_scripts/clean_music_data.R"))
source(paste0(home_path, "helper_scripts/compute_expanded_music_features.R"))

rm(list = ls())

# Run scripts for HM285 dataset
home_path = "./" # Reset path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/S1/")

dataset = "HM285"
source(paste0(home_path, "helper_scripts/clean_music_data.R"))
source(paste0(home_path, "helper_scripts/compute_expanded_music_features.R"))


rm(list = ls())

