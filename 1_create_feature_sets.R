# For each dataset, this script will clean the data using clean_music_data.R then form a feature set using "compute_music_features.R". 
# The resulting feature sets will be used in the other R scripts. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)

# If these packages are not already installed, they will be automatically installed here. 
packages = c("stringr", "Hmisc")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Run scripts for HM107 dataset
dataset = "HM107"
source(paste0(home_path, "helper_scripts/clean_music_data.R"))
source(paste0(home_path, "helper_scripts/compute_music_features.R"))

rm(list = ls())

# Run scripts for HM285 dataset
home_path = "./" # Reset path to the haydn-mozart directory (if not current directory)

dataset = "HM285"
source(paste0(home_path, "helper_scripts/clean_music_data.R"))
source(paste0(home_path, "helper_scripts/compute_music_features.R"))

rm(list = ls())

