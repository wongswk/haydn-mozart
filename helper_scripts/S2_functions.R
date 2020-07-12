# Functions for making heatmaps that are called from the S2_make_feature_heatmaps.R script. 

library(pheatmap)
library(viridis)
library(RColorBrewer)

font_size = 10

make_heatmap = function() {
  pdf(paste0(heatmap_path, "heatmap", "_", dataset,  ".pdf"))
  map = pheatmap(Feature2, 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F, show_rownames = F,
                 fontsize = 8, border_color = NA)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_cluster", "_", dataset,  ".pdf"))
  map = pheatmap(Feature2, 
                 cluster_rows = T, cluster_cols = T, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F, show_rownames = F,
                 fontsize = 8, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_cluster_row", "_", dataset,  ".pdf"))
  map = pheatmap(Feature2, 
                 cluster_rows = T, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F, show_rownames = F, 
                 fontsize = 8, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
}


make_heatmap_feature  = function(feature_name) {
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_", dataset,  ".pdf"))
  map = pheatmap(Feature2[, eval(as.symbol(feature_name))], 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col),
                 show_colnames = F, show_rownames = F, 
                 fontsize = font_size, border_color = NA)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_cluster", "_", dataset,  ".pdf"))
  map = pheatmap(Feature2[, eval(as.symbol(feature_name))], 
                 cluster_rows = T, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col),
                 show_colnames = F, show_rownames = F, 
                 fontsize = font_size, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_cluster_row", "_", dataset,  ".pdf"))
  map = pheatmap(Feature2[, eval(as.symbol(feature_name))], 
                 cluster_rows = T, cluster_cols = T, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno,
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col, "Set" = set_col),
                 show_colnames = F,  show_rownames = F, 
                 fontsize = font_size, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
}

make_heatmap_composer = function(composer) {
  if (composer == "Mozart") {
    inds = 1:Mozart_num
  } else {
    inds = (Mozart_num + 1):nrow(Feature2)
  }
  
  comp_quartet_col = viridis(n = length(unique(Anno$Quartet[inds])))
  names(comp_quartet_col) = unique(Anno$Quartet[inds])
  comp_set_col = magma(n = length(unique(Anno$Set[inds])))
  names(comp_set_col) = unique(Anno$Set[inds])
  
  data = Feature2[inds, ]
  qnt = quantile(as.numeric(data.matrix(data)), c(0.01, .99))
  brks = seq(qnt[1], qnt[2], length=20)
  
  viridis_col = plasma(n = length(brks - 1))
  
  pdf(paste0(heatmap_path, "heatmap", "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data, 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F, show_rownames = F,
                 fontsize = 8, border_color = NA)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_cluster", "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data, 
                 cluster_rows = T, cluster_cols = T, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F,  show_rownames = F, 
                 fontsize = 8, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_cluster_row", "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data, 
                 cluster_rows = T, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col, "Category" = category_col),
                 annotation = Category,
                 show_colnames = F,  show_rownames = F, 
                 fontsize = 8, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
}


make_heatmap_feature_composer  = function(feature_name, composer) {
  if (composer == "Mozart") {
    inds = 1:Mozart_num
  } else {
    inds = (Mozart_num + 1):nrow(Feature2)
  }
  
  comp_quartet_col = viridis(n = length(unique(Anno$Quartet[inds])))
  names(comp_quartet_col) = unique(Anno$Quartet[inds])
  comp_set_col = magma(n = length(unique(Anno$Set[inds])))
  names(comp_set_col) = unique(Anno$Set[inds])
  
  data = Feature2[inds, ]
  qnt = quantile(as.numeric(data.matrix(data)), c(0.01, .99))
  brks = seq(qnt[1], qnt[2], length=20)
  
  viridis_col = plasma(n = length(brks - 1))
  
  
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data[, eval(as.symbol(feature_name))], 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col),
                 show_colnames = F, show_rownames = F,
                 fontsize = font_size, border_color = NA)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_cluster", "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data[, eval(as.symbol(feature_name))], 
                 cluster_rows = T, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col),
                 show_colnames = F,  show_rownames = F, 
                 fontsize = font_size, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
  
  pdf(paste0(heatmap_path, "heatmap_", feature_name, "_cluster_row", "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(data[, eval(as.symbol(feature_name))], 
                 cluster_rows = T, cluster_cols = T, 
                 color = viridis_col, 
                 breaks = brks,
                 annotation_row = Anno[inds, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col),
                 show_colnames = F,  show_rownames = F, 
                 fontsize = font_size, border_color = NA,
                 treeheight_row = 0, treeheight_col = 0)
  print(map)
  dev.off()
}


make_cor_heatmap = function() {
  pdf(paste0(heatmap_path, "heatmap_", "correlation", "_", dataset,  ".pdf"))
  map = pheatmap(Gram, 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks2,
                 annotation_row = Anno,
                 annotation_col = Anno_col[, -1],
                 annotation_colors = list("Composer" = composer_col, "Quartet" = quartet_col2, "Set" = set_col2),
                 show_colnames = F,  show_rownames = F, 
                 fontsize = 5, border_color = NA)
  print(map)
  dev.off()
}


make_cor_heatmap_feature_composer = function(feature_name, composer) {
  if (composer == "Mozart") {
    inds = 1:Mozart_num
  } else {
    inds = (Mozart_num + 1):nrow(Feature2)
  }
  
  # Set up colors for heatmaps made separately for composers (quartet and set colors defined to be easier to see)
  set.seed(6)
  comp_quartet_col = viridis(n = length(unique(Anno$Quartet[inds])))
  comp_quartet_col = sample(comp_quartet_col)
  names(comp_quartet_col) = unique(Anno$Quartet[inds])
  comp_set_col = magma(n = length(unique(Anno$Set[inds])))
  comp_set_col = sample(comp_set_col)
  names(comp_set_col) = unique(Anno$Set[inds])
  
  Gram = cor(t(Feature2[inds, eval(as.symbol(feature_name))]))
  qnt2 = quantile(as.numeric(data.matrix(Gram)), c(0.01, .99))
  brks2 = seq(qnt2[1], qnt2[2], length=20)
  
  # duplicate row annotation to have column annotation too (makes correlation matrix easier to interpet)
  Anno_col = Anno[inds, ] 
  rownames(Anno_col) = colnames(Gram)
  
  
  pdf(paste0(heatmap_path, "gram_", feature_name, "_", composer, "_", dataset,  ".pdf"))
  map = pheatmap(Gram, 
                 cluster_rows = F, cluster_cols = F, 
                 color = viridis_col, 
                 breaks = brks2,
                 annotation_row = Anno[, -1],
                 annotation_col = Anno_col[, -1],
                 annotation_colors = list("Quartet" = comp_quartet_col, "Set" = comp_set_col),
                 show_colnames = F, show_rownames = F,
                 fontsize = 8, border_color = NA
                 )
  print(map)
  dev.off()
}

order_Cats = function() {
  Cats = read.csv(paste0(home_path, "outputs/categories_", dataset, ".csv")) 
  Cats = Cats[, -ncol(Cats)] # remove "notes" column
  if (dataset == "HM285") {
    Cats_M = Cats[1:27, ]
    Cats_H = Cats[28:86, ]
  }
  
  
  order_M = order(Cats_M$date)
  order_H = order(Cats_H$date)
  order = c(order_M, order_H + nrow(Cats_M))
  
  Cats = Cats[order, ]
  return(list(Cats = Cats, order = order))
}

order_data = function(data) {
  # data should have rows corresponding to movements 
  Cats = read.csv(paste0(home_path, "outputs/categories_", dataset, ".csv"))[, -6]
  date = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$date[i], Cats$num_mvmts[i])))
  order = c(order(date[Mozart_inds]), order(date[Haydn_inds]) + length(Mozart_inds))
  data = data[order, ]
  return(list(data = data, order = order))
}