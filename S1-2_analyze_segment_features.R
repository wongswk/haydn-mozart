# This script makes heatmaps of correlation matrices for sets of segment features, so that we can 
# assess similarity between segment features across segment lengths m = 8, 9, 10, ..., 17, 18. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
output_path = paste0(home_path, "outputs/S1/")

# If these packages are not already installed, they will be automatically installed here. 
packages = c("pheatmap", "viridis")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

library(pheatmap)
library(viridis)

compare_segment_features = function(dataset) {
  df = read.csv(paste0(output_path, "Feature_expanded_", dataset, ".csv"))[, -c(1:2)]
  
  # We will parse the names of the dataframe to detect which columns contain the segment features. 
  # First, remove any features that could be confused with segment features (due to containing numbers 8:18 in their names)
  pair = grep("pair", names(df))
  pair2 = grep("Pair", names(df))
  df = df[,-c(pair, pair2)] 
  
  # Define local function to make a heatmap 
  make_heatmap = function(keyword) {
    if (keyword == "Dev_SD") {
      mark1 = grep("Dev_SD_t", names(df))
      mark = grep(keyword, names(df[-mark1]))
    } else if (keyword == "Dev_perc") {
      mark1 = grep("Dev_perc_t", names(df))
      mark = grep(keyword, names(df[-mark1]))
    } else if (keyword == "Dev_count") {
      mark1 = grep("Dev_count_t", names(df))
      mark = grep(keyword, names(df[-mark1]))
    } else {
      mark = grep(keyword, names(df))
    }
    
    df_red = df[, mark]
    cors = data.frame(cor(df_red))
    matches = regmatches(names(df_red), gregexpr("[[:digit:]]+", names(df_red)))
    names = unlist(lapply(matches, function(x) {
      m = x[[1]]
      part = x[[2]]
      parts = c("Viola", "Cello", "Violin 2", "Violin 1")
      part = parts[as.numeric(part)]
      name = paste0(part, ", ", "m = ", m)
      return(name)
    }))
    
    # Set up names for heatmap 
    if (length(unique(names)) == length(names)) {
      row.names(cors) = names
      names(cors) = names
    }
    
    # Set up colors for heatmap
    qnt = quantile(as.numeric(data.matrix(cors)),c(0.01,.99), na.rm = T)
    brks = seq(qnt[1],qnt[2],length=20)
    head(brks)
    viridis_col = viridis(n = length(brks - 1))
    
    
    pdf(paste0(output_path, keyword, "_", dataset,  ".pdf"))
    map = pheatmap(cors, 
                   cluster_rows = F, cluster_cols = F,
                   color = viridis_col, 
                   breaks = brks,
                   border_color = NA)
    print(map)
    dev.off()
    
  }
  
  # Make heatmaps for each group of features 
  make_heatmap("Recap_acc")
  make_heatmap("Recap_perc")
  make_heatmap("Recap_t_acc")
  make_heatmap("Recap_t_perc")
  make_heatmap("Recap_match")
  make_heatmap("Recap_t_match")
  
  make_heatmap("Dev_SD")
  make_heatmap("Dev_perc")
  make_heatmap("Dev_SD_t")
  make_heatmap("Dev_perc_t")
  make_heatmap("Dev_count")
  make_heatmap("Dev_count_t")
  
  make_heatmap("Expo_acc")
  make_heatmap("Expo_perc")
  make_heatmap("Expo_t_acc")
  make_heatmap("Expo_count")
  make_heatmap("Expo_t_count")
  
  make_heatmap("Prop_m3_mean")
  make_heatmap("Prop_m3_sd")
  make_heatmap("Prop_m3_num_0")
  make_heatmap("Prop_m3_num_.6")
  make_heatmap("Prop_m3_med")
  make_heatmap("Prop_m3_q3")
  
  
}

# Make heatmaps for the HM107 dataset
compare_segment_features("HM107")

# Make heatmaps for the HM285 dataset 
compare_segment_features("HM285")
