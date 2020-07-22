# This script fits one model on all 285 movements in the HM285 dataset for interpretation purposes,
# by calling the main LOO.R script with settings defined for this task. 
# The process is
#     1) feature selection (random ICM for 10 trials) identifies a subset of features
#     2) those features are used as predictors in a Bayesian logistic regression model (from the arm package)
#     3) the Hosmer-Lemeshow test is done to check goodness of fit 
#     4) plots are made that summarize the features in the model 
# Note that this script uses task parallelism via the package "parallel" to reduce runtime. 
# To disable parallelism, set "parallel = F" in line 20. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/")

# If these packages are not already installed, they will be automatically installed here. 
packages = c("arm", "ggplot2", "ggpubr", "parallel", "ResourceSelection")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Specify dataset, whether task parallelism is to be used, and if so, how many cores.
dataset = "HM285"
parallel = T
if (parallel) {
  num_cores = 6
}

# Set various hyperparameter values for random ICM on the full dataset 
filter_threshold = 1  # no correlation filtering will be performed on the feature set prior to running the random ICM algorithm 
prior.scale = .6      # scale for Cauchy prior in bayesglm function in arm package 
prior.df = 1          # df for Cauchy prior in bayesglm function in arm package 
simple = F            # if T, we will use only "simple" features (basic and interval features) for feature selection and classification
LOO_type = "LOO"      # if LOO_type = "LOO", regular leave-one-out CV will be done. If LOO_type = "LOQO", leave-one-quartet-out CV will be done. 

if (simple) {
  extra_str = "_simple"
} else {
  extra_str = ""
}

# Read in dataframes
Feature = read.csv(paste0(output_path, "Feature_", dataset, ".csv"))[,-1] # feature set
filename = paste0("_scale", prior.scale, "_df", prior.df, "_thresh", filter_threshold, "_", LOO_type, extra_str, "_", dataset)

# Load functions
source(paste0(home_path, "helper_scripts/LOO_functions.R"))
library(ggplot2)
library(ggpubr)
library(ResourceSelection)


if (dataset == "HM285") {
  random_seeds = c(690893, 965097, 659011, 253073, 834997, 386972, 190566, 257693, 181779, 329323) # specify ten random seeds, which will be used for the ten repetitions of random ICM
  bad_vars = grep("voicepair_int_dist_0_2.4", names(Feature)) # This feature causes numerical problems (due to a large magnitude of coefficient of variation) for the bayesglm fitting, so it will be removed.
  
} else if (dataset == "HM107") {
  random_seeds = c(497699, 717619, 991906, 380035, 777445, 934705, 212143, 651674, 125555, 267221) # specify ten random seeds, which will be used for the ten repetitions of random ICM
  
  bad_vars = grep("voicepair_int_dist_1_1.2", names(Feature)) # This feature causes numerical problems (due to a large magnitude of coefficient of variation) for the bayesglm fitting, so it will be removed.
}

if (simple == T) {
  sonata_vars = c(grep("Recap", colnames(Feature)), grep("Dev", colnames(Feature)), grep("Expo", colnames(Feature)))
  bad_vars = c(bad_vars, sonata_vars)
}

Feature = Feature[, -bad_vars]

# Fit model on all movements
full_results = Full_results(Feature, 1, random_seeds)
full_model = full_results[[1]]
best_BIC = full_results[[2]]
full_probs = full_results[[3]]
index = full_results[[4]]
best_vars = full_results[[5]]

# Summarize the model numerically 
summary(full_model)
cat(capture.output(print(summary(full_model)),
                  file = paste0(output_path, "Full_model", filename, ".txt")))
  
#Test goodness of fit using Hosmer-Lemeshow test for a range of group sizes (from 20 to 100) for the one model 
g_val = c(20:100)
pval = c()
for (j in g_val){
  test = hoslem.test(Feature[, 1], full_probs, g = j)
  pval = c(pval, test$p.value)
}
  
write.csv(data.frame(group_size = g_val, pvalue = pval), paste0(output_path, "Hosmer_Lemeshow", filename, ".csv"), row.names = F)
summary(pval)
  
# Produce the relative frequency plots 
make_relfreq_plot = function() {
  Mozart_num = length(Feature$M0[Feature$M0 == 0])
  Haydn_num = length(Feature$M0[Feature$M0 == 1])
  
  best_vars = best_vars[c(1, 7, 5, 8, 3, 6, 2, 4)]
  Feature_subset=cbind(Feature$M0, 
                       c(rep("Mozart", Mozart_num),rep("Haydn", Haydn_num)), 
                       Feature[, best_vars[-1]])
  names(Feature_subset)=c("name", "Composer", names(Feature_subset)[3:ncol(Feature_subset)])
  Feature_subset$Composer = factor(Feature_subset$Composer, levels=c("Mozart", "Haydn"))
  Feature_subset = Feature_subset[]
  plot_names=c("SD of duration for Violin 1", 
               "Prop. of desc. intervals for Violin 1","Diff. in prop. of m3 intervals for Viola & Cello", "Mean prop. of m3 intervals for Viola",
               "SD count for pitch & Viola", "SD count for pitch & Cello",
               "Max. fraction for duration & Viola")
  feature_names=c(rep("(Basic)", 1), rep("(Interval)", 3), rep("(Development)", 2), rep("(Recapitulation)", 1))
  
  make_plot = function(i) {
    data=Feature_subset[, c(2, 2 + i)]
    names(data) = c("Composer", names(data)[2])
    varname = names(data)[2]
    plot = ggplot(data, aes_string(varname, fill="Composer")) + 
      geom_histogram(aes(y=2*(..density..)/sum(..density..)), position="dodge")+ 
      labs(y = "Relative Frequency", x = plot_names[i], title = feature_names[i]) +
      scale_fill_manual(values = c("gray60", "black")) + 
      theme(axis.title=element_text(size=8), axis.text=element_text(size=6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"),
            plot.title=element_text(size=10)) 
    
    return(plot)
  }
  
  plots = lapply(1:(length(best_vars)-1), make_plot)
  pdf(paste0(output_path, "relfreq", filename, ".pdf"), onefile = F)   
  print(ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], plots[[7]],
                  font("x.text", size = 8), ncol=2, nrow=4, common.legend = TRUE, legend="bottom",
                  labels=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)"),
                  font.label = list(size = 10, color = "black", face = "bold", family = NULL, labels=c("Mozart","Haydn"))))
  dev.off()
}

make_relfreq_plot()
