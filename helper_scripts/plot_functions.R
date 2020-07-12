# Load libraries
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(stringr)

# This function generates a confusion matrix, which compares the number of classified and misclassified movements for each composer.
make_confusion_mat = function(Feature, df) {
  tab = table(Feature$M0, df$test.class)
  confusion_matrix = data.frame(Predicted_Mozart = tab[, 1], Predicted_Haydn = tab[, 2])
  row.names(confusion_matrix) = c("Observed_Mozart", "Observed_Haydn")
  return(confusion_matrix)
}

# For the HM285 dataset, this function makes a barplot summarizing the selected features from feature selection by category. 
make_barplot = function(Feature = Feature_HM285, filename = filename_HM285, tab = freq_vars) {
  category_names = c("Basic", "Interval", "Exposition", "Development", "Recapitulation")
  
  tab = tab[-which(names(tab) == "M0")]
  df = data.frame(Features = names(tab), Count = as.vector(tab))
  
  Category = sapply(names(tab), function(j) names[grep(j, names(Feature)[-1])]) 
  df = cbind(df, Category)
  
  gray_colors = gray.colors(5)[5:1]
  color_inds = c(rep(gray_colors[1], 3), rep(gray_colors[2], 14), gray_colors[3], rep(gray_colors[4], 6), gray_colors[5])
  
  x_names = paste0(c(1:nrow(df)))
  df_ord = lapply(category_names, function(c) {
    ord = order(df$Count[df$Category == c], decreasing = F)
    df_cat = df[df$Category == c, ][ord, ]
    return(df_cat)
  })  #variables are ordered by increasing frequency within each category
  df_new = rbind(df_ord[[5]], df_ord[[4]], df_ord[[3]], df_ord[[2]], df_ord[[1]])
  df_new$Features = factor(c(1:nrow(df)), levels=c(1:nrow(df)))
  df_new$Category = factor(df_new$Category, 
                           levels = unique(df_new$Category))
  sample_size = nrow(Feature)
  
  feat_labels = c("Standard deviation of duration for Violin 1", 
               "Prop. of pairwise descending intervals for Violin 1","Difference in prop. of semitone-3 intervals for Viola & Cello", "Mean prop. of m3 intervals for Viola",
               "Standard deviation count at threshold 4.244 (A) for pitch, m = 14, & Viola", "Standard deviation count at threshold 4.024 (B) for pitch, m = 8, & Cello",
               "Maximum fraction of overlap for duration, m = 8, & Viola")
  labels = rep("", nrow(df_new))
  labels[df_new$Count > 189] = feat_labels[7:1]
  
  abb_labels = labels 
  abb_labels[df_new$Count > 189] =  c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)", "(G)")[7:1]
  
  df_new = cbind(df_new, Labels = labels, Abb = abb_labels)
  
  pdf(paste0(output_path, "barplot", filename, ".pdf"), onefile = F)
  plot = ggplot(df_new, aes(x = Features, y = Count, fill = Category, label = Abb)) +
    geom_bar(stat = "identity", width=0.8, position = position_dodge(width=0.3)) + 
    theme_minimal() + 
    scale_fill_manual(values = gray_colors, guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous(breaks = c(seq(0.0, sample_size, 50), sample_size), limits = c(0, sample_size)) +
    theme(axis.text = element_text(size = 10, face = "bold")) +
    xlab("Feature") + scale_x_discrete(labels = df_new$Abb) +
    coord_flip() 
  print(plot)
  dev.off()

  return(df_new)
}

# This function reads in the text files of selected variables (produced from other scripts) and outputs it in list format.
parse_vars_txt = function(filename) {
  lines  = readLines(filename, encoding="UTF-16")
  lines2 = gsub("\\[{2}\\d+\\]{2}", "@", lines)          # Replace [[*]] with '@'
  lines3 = gsub("\\[\\d+\\]\\s", "", lines2)[-1]         # Remove all [*]
  lines4 = paste(lines3, collapse=" ")                   # Paste together into one string
  lines5 = strsplit(lines4, "@")[[1]]                    # Break into list
  
  # Each list entry now requires further parsing
  lines_clean = lapply(lines5, parse_vars)
  return(lines_clean)
}

# This is a helper function for parse_vars_txt. 
parse_vars = function(vars) {
  vars2 = strsplit(vars, split = " ")[[1]]          # Break each variable into a separate string by splitting on " "
  vars3 = vars2[vars2 != ""]                        # Remove white space
  vars3 = str_extract_all(vars3, "[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_.]") # Only keep numbers, letters, underscores, and periods, so that backslashes are removed
  vars4 = sapply(vars3, function(str) paste0(str, collapse = "")) # Paste into final correct form
  return(vars4)
}

# This functions creates a plots (for both the HM285 and HM107 datasets) of estimated probabilities for each movement. 
# For each movement, the composer and whether it was classified correctly or incorrectly is denoted. 
make_probability_plot = function() {
  correct_HM285 = ifelse(LOO_HM285$test.class == Feature_HM285$M0, "Correct", "Incorrect")
  correct_HM107 = ifelse(LOO_HM107$test.class == Feature_HM107$M0, "Correct", "Incorrect")
  mean_107 = c(mean(LOO_HM107$test.pred[1:53]), mean(LOO_HM107$test.pred[54:107])) # mean estimated probability for each composer
  mean_285 = c(mean(LOO_HM285$test.pred[1:82]), mean(LOO_HM285$test.pred[83:285])) # mean estimated probability for each composer
  
  df = data.frame(Probability = c(LOO_HM107$test.pred, LOO_HM285$test.pred), 
                  Composer = c(rep("Mozart", 53), rep("Haydn", 54), rep("Mozart", 82), rep("Haydn", 203)),
                  Dataset = c(rep("HM107", nrow(LOO_HM107)), rep("HM285", nrow(LOO_HM285))), 
                  Movement = c(1:107, 1:285),
                  Classification = c(correct_HM107, correct_HM285),
                  Mean = c(rep(mean_107[1], 53), rep(mean_107[2], 54), rep(mean_285[1], 82), rep(mean_285[2], 203)))
  
  plot_HM107 = ggplot(df[1:107, ]) + 
    geom_point(aes(x = Movement, y = Probability, color = Composer, shape = Classification, size = Classification)) + 
    theme_minimal() + ggtitle("HM107") + scale_size_manual(values = c(3, 3)) +
    scale_shape_manual(values = c(20, 7)) + 
    scale_color_manual(values = c("black", "gray60")) + 
    ylab("Estimated Probability") + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
  
  plot_HM285 = ggplot(df[108:392, ]) +
    geom_point(aes(x = Movement, y = Probability, color = Composer, shape = Classification, size = Classification)) + 
    theme_minimal() + ggtitle("HM285") + 
    scale_shape_manual(values = c(20, 7)) + scale_size_manual(values = c(3,3)) +
    scale_color_manual(values = c("black", "gray60")) +
    ylab("Estimated Probability") + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank())
  
  pdf(paste0(output_path, "probability_plot", filename, ".pdf"), onefile = F)
  plot = ggarrange(plot_HM107, plot_HM285, common.legend = T, nrow = 2, ncol = 1)
  print(plot)
  dev.off()
  
}