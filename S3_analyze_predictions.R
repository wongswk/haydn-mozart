# For the HM285 dataset, this script analyzes the estimated probabilities, classes, and selected features 
# from LOO, LOQO, and LOO on the reduced/simple feature set. 
# Several plots, descriptive statistics, and tables are produced. 


home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/")
dir.create(paste0(output_path, "S3/"), showWarnings = F, recursive = T)

# If these packages are not already installed, they will be automatically installed here. 
packages = c("ggplot2", "viridis", "ggpubr", "stringr", "RColorBrewer", "pheatmap")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Summary of classification results 
dataset = "HM285"

# Load functions
source(paste0(home_path, "helper_scripts/S2_functions.R"))
source(paste0(home_path, "helper_scripts/plot_functions.R"))

# Read in feature set 
Feature = read.csv(paste0(output_path, "Feature_", dataset, ".csv"))[, -1]
bad_vars = grep("voicepair_int_dist_0_2.4", names(Feature)) 
Feature = Feature[, -bad_vars]

# Set various hyperparameter values (these are for reading in files that have already been produced with these hyperparameters)
filter_threshold = 1  # no correlation filtering will be performed on the feature set prior to running the random ICM algorithm 
prior.scale = .6      # scale for Cauchy prior in bayesglm function in arm package 
prior.df = 1          # df for Cauchy prior in bayesglm function in arm package 

# Read in predicted probabilities and classes from LOO, LOQO, and LOO on simple features
file_start = paste0("_scale", prior.scale, "_df", prior.df, "_thresh", filter_threshold, "_")
LOO_df = read.csv(paste0(output_path, "predictions", file_start, "LOO_", dataset, ".csv"))
LOQO_df = read.csv(paste0(output_path, "predictions", file_start, "LOQO_", dataset, ".csv"))
LOO_simple_df = read.csv(paste0(output_path, "predictions", file_start, "LOO_simple_", dataset, ".csv"))

Mozart_num = length(Feature$M0[Feature$M0 == 0])
Haydn_num = length(Feature$M0[Feature$M0 == 1])

Mozart_inds = 1:Mozart_num
Haydn_inds = (Mozart_num + 1):nrow(Feature)

# Read in categories of movements (set, quartet) and order by date composed
Cats_list = order_Cats()
Cats = Cats_list$Cats
Cats_order = Cats_list$order

# Rename Prussian quartets by Mozart and Haydn so that there are unique names 
prussian_H = intersect(which(Cats$set_name == "Prussian"), 28:nrow(Cats))
prussian_M = intersect(which(Cats$set_name == "Prussian"), 1:27)
Cats$set_name = as.character(Cats$set_name)
Cats$set_name[prussian_M] = rep("Prussian (M)", length(prussian_M))
Cats$set_name[prussian_H] = rep("Prussian (H)", length(prussian_H))

# Order LOO_df and LOQO_df by date composed
LOO_list = order_data(LOO_df)
LOO_df = LOO_list$data
mvmt_order = LOO_list$order
LOQO_df = order_data(LOQO_df)$data

quartet = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$quartet_name[i], Cats$num_mvmts[i]))) # if you want to use actual names of quartets (categorical)
quartet_numeric = unlist(lapply(1:nrow(Cats), function(i) rep(i, Cats$num_mvmts[i]))) # if you want to use numbers for quartets (quantitative)
set = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$set_name[i], Cats$num_mvmts[i])))
set_abb = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$set_abb[i], Cats$num_mvmts[i])))
num_sets = sapply(unique(set), function(i) length(which(set == i)))

set = factor(set, levels = unique(Cats$set_name))
set_abb = factor(set_abb, levels = unique(Cats$set_abb))

#______________________________________________________________________________________________________________________________________________________________________________
#______________________________________________ Make plots of estimated probabilities    ______________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________________________

make_plot = function(data, composer, other_str) {
  if (composer == "Haydn") {
    inds = Haydn_inds
    ind_mean = 2
    letter_size = 3
  } else {
    inds = Mozart_inds
    ind_mean = 1
    letter_size = 4
  }
  
 
  df = data.frame(prob = data$test.pred, Quartet = quartet_numeric, set = set, set_abb = set_abb, 
                  composer = Feature$M0, index = 1:285) 
  
  set_mean = sapply(1:length(unique(df$set)), function(i) mean(df$prob[df$set == unique(df$set)[i]]))
  quartet_mean =  sapply(1:length(unique(df$Quartet)), function(i) mean(df$prob[df$Quartet == unique(df$Quartet)[i]]))
  global_mean = c(mean(df$prob[1:82]), mean(df$prob[83:285]))
  
  set_mean2 = unlist(lapply(1:length(unique(df$set)), function(i) rep(set_mean[i], num_sets[i])))
  quartet_mean2 = unlist(lapply(1:length(unique(df$Quartet)), function(i) rep(quartet_mean[i], Cats$num_mvmts[i])))
  global_mean2 = c(rep(global_mean[1], Mozart_num), rep(global_mean[2], Haydn_num))
  
  df = cbind(df, quartet_mean = quartet_mean2, global_mean = global_mean2, set_mean = set_mean2)
  df = df[inds, ]
  
  plot = ggplot(df) + #geom_point(aes(x=index, y=prob,  color = quartet)) + 
    geom_text(aes(x = Quartet, y = prob, label = set_abb, color = Quartet), size = letter_size) +
    theme_minimal() + ggtitle(paste(composer, other_str)) +
    geom_line(aes(x = Quartet, y = global_mean), color = "gray") +
    theme(legend.text= element_text(size=7)) + 
    scale_discrete_identity(aesthetics = "label", name = "Set", breaks = df$set_abb, 
                            labels = df$set, guide = "legend") +
    scale_color_viridis(discrete = F, option = "plasma") +
    ylab("Estimated Probability") + ylim(0, 1) 
  
  return(plot)
  
}

arrange_plots = function(composer, output_path) {
  plot_LOO = make_plot(LOO_df, composer, "LOO")
  plot_LOQO = make_plot(LOQO_df, composer, "LOQO")
  plot_simple_LOO = make_plot(LOO_simple_df, composer, "LOO_reduced")
  
  pdf(paste0(output_path, "probs_LOO-LOQO_", composer, ".pdf"), onefile = F)
  plots = ggarrange(plot_LOO, plot_LOQO, common.legend = T, ncol = 2, nrow = 1)
  print(plots)
  dev.off()
  
  pdf(paste0(output_path, "probs_LOO-LOO-reduced_", composer, ".pdf"), onefile = F)
  plots = ggarrange(plot_LOO, plot_simple_LOO, common.legend = T, ncol = 2, nrow = 1)
  print(plots)
  dev.off()

}


arrange_plots("Haydn", paste0(output_path, "S3/"))
arrange_plots("Mozart", paste0(output_path, "S3/"))


#______________________________________________________________________________________________________________________________________________________________________________
#_________________________________ Numeric analyses of estimated probabilities, classes, and selected features   ______________________________________________________________
#______________________________________________________________________________________________________________________________________________________________________________

make_tables = function() {
  prob_LOO = LOO_df$test.pred
  prob_LOQO = LOQO_df$test.pred
  class_LOO = LOO_df$test.class
  class_LOQO = LOQO_df$test.class
  prob_LOO_simple = LOO_simple_df$test.pred
  class_LOO_simple = LOO_simple_df$test.class
  
  make_table = function(composer, vec1, vec2, str) {
    if (composer == "Haydn") {
      inds = Haydn_inds
    } else if (composer == "Mozart") {
      inds = Mozart_inds
    } else {
      inds = 1:length(vec1)
    }
    
    vec1 = round(vec1, 2)
    vec2 = round(vec2, 2)
    
    greater = unlist(lapply(unique(quartet[inds]), function(q) vec1[quartet == q] > vec2[quartet == q]))
    equal = unlist(lapply(unique(quartet[inds]), function(q) vec1[quartet == q] == vec2[quartet == q]))
    lesser = unlist(lapply(unique(quartet[inds]), function(q) vec1[quartet == q] < vec2[quartet == q]))
    df = data.frame(Sign = c(paste0("LOO > ", str), paste0("LOO = ", str), paste0("LOO < ", str)), 
                     Mean = c(mean(greater), mean(equal), mean(lesser)))
    return(df)
  }
  
  
  # Mozart
  print("Mozart: LOO vs. LOQO probabilities")
  print(make_table("Mozart", prob_LOO, prob_LOQO, "LOQO"))
  print("Mozart: LOO vs. LOQO classes")
  print(make_table("Mozart", class_LOO, class_LOQO, "LOQO"))
  
  # Haydn
  print("Haydn: LOO vs. LOQO probabilities")
  print(make_table("Haydn", prob_LOO, prob_LOQO, "LOQO"))
  print("Haydn: LOO vs. LOQO classes")
  print(make_table("Haydn", class_LOO, class_LOQO, "LOQO"))
  
  # Both composers
  print("Both composers: LOO vs. LOQO probabilities")
  print(make_table("", prob_LOO, prob_LOQO, "LOQO"))
  print("Both composers: LOO vs. LOQO classes")
  print(make_table("", class_LOO, class_LOQO, "LOQO"))
  
  # Repeat for LOO on the reduced feature set 
  # Mozart
  print("Mozart: LOO vs. LOO-reduced probabilities")
  print(make_table("Mozart", prob_LOO, prob_LOO_simple, "LOO_simple"))
  print("Mozart: LOO vs. LOO-reduced classes")
  print(make_table("Mozart", class_LOO, class_LOO_simple, "LOO_simple"))
  
  # Haydn
  print("Haydn: LOO vs. LOO-reduced probabilities")
  print(make_table("Haydn", prob_LOO, prob_LOO_simple, "LOO_simple"))
  print("Haydn: LOO vs. LOO-reduced classes")
  print(make_table("Haydn", class_LOO, class_LOO_simple, "LOO_simple"))
  
  # Both composers
  print("Both composers: LOO vs. LOO-reduced probabilities")
  print(make_table("", prob_LOO, prob_LOO_simple, "LOO_simple"))
  print("Both composers: LOO vs. LOO-reduced classes")
  print(make_table("", class_LOO, class_LOO_simple, "LOO_simple"))
  
}

# Within this function, a number of descriptive statistics are printed out that summarize the LOO results on the reduced (aka simple) vs. full feature sets.
compare_simple_LOO = function() {
  mis_simple = which(LOO_simple_df$test.class != Feature$M0) # misclassified movements when using reduced feature set
  mis_LOO = which(LOO_df$test.class != Feature$M0)           # misclassified movements when using full feature set
  
  # Which movements are missed for both the reduced and full feature sets? Which are missed on one but not the other? 
  both = mis_simple[mis_simple %in% mis_LOO] 
  simple = mis_simple[!mis_simple %in% mis_LOO] 
  LOO = mis_LOO[!mis_LOO %in% mis_simple] 
  print(paste0("# of movements misclassified on both full and reduced feature sets: ", length(both)))
  print(paste0("# of movements misclassified on reduced feature set but not full : ", length(simple)))
  print(paste0("# of movements misclassified on full feature set but not reduced : ", length(LOO)))
  
  # Selected features over the folds 
  vars_LOO = parse_vars_txt(paste0(output_path, "variables", file_start, "LOO_", dataset, ".txt"))
  vars_simple = parse_vars_txt(paste0(output_path, "variables", file_start, "LOO_simple_", dataset, ".txt"))
  vars_missed = lapply(simple, function(i) vars_LOO[[i]])
  freq_vars = table(unlist(vars_missed))
  print("For the movements misclassified on the reduced feature set & correctly classified on the full feature set, which features were selected on the full feature set?")
  print(sort(freq_vars, decreasing = T)) # (number of folds a feature was selected in)
  vars_missed2 = lapply(simple, function(i) vars_simple[[i]])
  print("For the movements misclassified on the reduced feature set & correctly classified on the full feature set, which features were selected on the reduced feature set?")
  print(sort(table(unlist(vars_missed2)), decreasing = T)) # (number of folds a feature was selected in)
  
  #Summarize features for the movements misclassified on the reduced feature set & correctly classified on the full feature set
  unique(vars_missed) # list of models for each fold/movement
  recap_t = mean(sapply(vars_missed, function(i) "Recap_t_acc_8.1" %in% i))
  print("Proportion of folds/movements (out of those misclassified on reduced feature set & correctly clasified on full feature set) that selected the Recap_t_acc_8.1 feature:")
  print(recap_t)
  dev_count = mean(sapply(vars_missed, function(i) "Dev_count_14_thresh4.244.1" %in% i))
  print("Proportion of folds/movements (out of those misclassified on reduced feature set & correctly clasified on full feature set) that selected the Dev_count_14_thresh4.244.1 feature:")
  print(dev_count)
  recap = sapply(vars_missed, function(i) length(grep("Recap", i)))
  dev = sapply(vars_missed, function(i) length(grep("Dev", i)))
  expo = sapply(vars_missed, function(i) length(grep("Expo", i)))
  print("For each fold/movement (out of those misclassified on reduced feature set & correctly classified on full feature set), how many sonata-inspired features were selected?")
  print(data.frame(Exposition = expo, Development = dev, Recapitulation = recap), row.names = paste0("Movement ", 1:length(expo)))
  
  # Proportion of Mozart movements belonging to each group
  len_simple = length(which(simple <= 82))/length(simple)
  len_both = length(which(both <= 82))/length(both)
  len_LOO = length(which(LOO <= 82))/length(LOO)
  
}


compare_LOQO = function() {
  mis_LOQO = which(LOQO_df$test.class != Feature$M0)
  mis_LOO = which(LOO_df$test.class != Feature$M0)
  
  both = mis_LOQO[mis_LOQO %in% mis_LOO] # both
  LOQO = mis_LOQO[!mis_LOQO %in% mis_LOO] # missed in LOQO but not LOO
  LOO = mis_LOO[!mis_LOO %in% mis_LOQO] # missed in LOO but not LOQO
  
  print(paste0("# of movements misclassified in both LOO and LOQO: ", length(both)))
  print(paste0("# of movements misclassified in LOQO but not LOO : ", length(LOQO)))
  print(paste0("# of movements misclassified in LOO but not LOQO : ", length(LOO)))
  
  # Selected features over the folds 
  vars_LOO = parse_vars_txt(paste0(output_path, "variables", file_start, "LOO_", dataset, ".txt"))
  vars_LOQO = parse_vars_txt(paste0(output_path, "variables", file_start, "LOQO_", dataset, ".txt"))
  
  print("Most commonly selected features in LOO & proportion of times they were selected: ")
  print(sort(table(unlist(vars_LOQO)), decreasing = T)[1:8]/86)
  print("Most commonly selected features in LOQO & proportion of times they were selected: ")
  print(sort(table(unlist(vars_LOO)), decreasing = T)[1:8]/285)
}


make_tables() 
compare_simple_LOO()
compare_LOQO()


