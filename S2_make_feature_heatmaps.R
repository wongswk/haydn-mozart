# For the HM285 dataset, this script generates heatmaps of the feature matrix X, the Gram matrix XX', and their variants, 
# so that we can analyze possible dependencies among movements. 

home_path = "./" # Set path to the haydn-mozart directory (if not current directory)
heatmap_path = paste0(home_path, "outputs/S2/")
dir.create(heatmap_path, showWarnings = F, recursive = T)

# If these packages are not already installed, they will be automatically installed here. 
packages = c("pheatmap", "viridis", "RColorBrewer")
lapply(packages, function(p) if(!p %in% rownames(installed.packages())) install.packages(p))

# Load functions
source(paste0(home_path, "helper_scripts/S2_functions.R"))

dataset = "HM285"
Feature = read.csv(paste0(home_path, "outputs/Feature_", dataset, ".csv")) # read in feature set
Feature2 = Feature[, -c(1:2)]

Mozart_num = length(which(Feature$M0 == 0))
Haydn_num = length(which(Feature$M0 == 1))

Mozart_inds = 1:Mozart_num
Haydn_inds = (Mozart_num + 1):nrow(Feature)

# Read in set and quartet categories and order by date of composition 
Cats = order_Cats()$Cats

prussian_H = intersect(which(Cats$set_name == "Prussian"), 28:nrow(Cats))
prussian_M = intersect(which(Cats$set_name == "Prussian"), 1:27)
Cats$set_name = as.character(Cats$set_name)
Cats$set_name[prussian_M] = rep("Prussian (M)", length(prussian_M))
Cats$set_name[prussian_H] = rep("Prussian (H)", length(prussian_H))

# Order Feature2 by date of composition
Feature2 = order_data(Feature2)$data

#quartet = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$quartet_name[i], Cats$num_mvmts[i]))) # if you want to use actual names of quartets (categorical)
quartet = unlist(lapply(1:nrow(Cats), function(i) rep(i, Cats$num_mvmts[i]))) # if you want to use numbers for quartets (quantitative)
set = unlist(lapply(1:nrow(Cats), function(i) rep(Cats$set_name[i], Cats$num_mvmts[i])))
set = factor(set, levels = unique(Cats$set_name))

# __________________________________________________________________________________________________________________________________________
#__________________________________________________ Make heatmaps of data matrix X _________________________________________________________
# __________________________________________________________________________________________________________________________________________

# Scale and center features, since they have very different units 
Feature2 = data.frame(apply(Feature2, 2, scale))
row.names(Feature2) = 1:285
names(Feature2) = names(Feature)[-c(1:2)]

# Set up composer, quartet, and set annotations
Anno = data.frame(Composer = c(rep("Mozart", Mozart_num), rep("Haydn", Haydn_num)),
                  Quartet = quartet, Set = set)
row.names(Anno) = 1:285

# Set up column annotations (will label features based on their categories)
recap = grep("Recap", names(Feature2))
dev = grep("Dev", names(Feature2))
expo = grep("Expo", names(Feature2))
basic = unlist(lapply(c("SD_time", "SD_pitch", "mean_time", "mean_pitch", "count_pitch", "simult"), function(x) grep(x, names(Feature2))))
int = c(1:ncol(Feature2))[-c(recap, dev, expo, basic)]
feature_cats = rep(NA, ncol(Feature2))
feature_cats[recap] = rep("Recapitulation", length(recap))
feature_cats[dev] = rep("Development", length(dev))
feature_cats[expo] = rep("Exposition", length(expo))
feature_cats[basic] = rep("Basic", length(basic))
feature_cats[int] = rep("Interval", length(int))
Category = data.frame(Category = feature_cats)
rownames(Category) = colnames(Feature2)


# Set up colors for full heatmaps
composer_col = c("cyan", "magenta")
names(composer_col) = levels(Anno$Composer)
quartet_col = magma(n = length(unique(Anno$Quartet)))
names(quartet_col) = unique(Anno$Quartet)
set_col = viridis(n = length(levels(Anno$Set)))
names(set_col) = unique(Anno$Set)

category_col = cividis(n = 5)
names(category_col) = unique(feature_cats)

qnt = quantile(as.numeric(data.matrix(Feature2)), c(0.01, .99))
brks = seq(qnt[1],qnt[2],length=20)
head(brks)
viridis_col = plasma(n = length(brks - 1))


# full heatmap
make_heatmap() 

# separate heatmaps for each feature category
make_heatmap_feature("recap")
make_heatmap_feature("expo")
make_heatmap_feature("dev")
make_heatmap_feature("int")
make_heatmap_feature("basic")

# separate heatmaps for each composer
make_heatmap_composer("Haydn")
make_heatmap_composer("Mozart")

# separate heatmaps for each pair of composer and feature category 
# (Haydn)
make_heatmap_feature_composer("recap", "Haydn")
make_heatmap_feature_composer("expo", "Haydn")
make_heatmap_feature_composer("dev", "Haydn")
make_heatmap_feature_composer("int", "Haydn")
make_heatmap_feature_composer("basic", "Haydn")
# (Mozart)
make_heatmap_feature_composer("recap", "Mozart")
make_heatmap_feature_composer("expo", "Mozart")
make_heatmap_feature_composer("dev", "Mozart")
make_heatmap_feature_composer("int", "Mozart")
make_heatmap_feature_composer("basic", "Mozart")

# __________________________________________________________________________________________________________________________________________
#__________________________________________________ Make heatmaps of Gram matrix XX' _________________________________________________________
# __________________________________________________________________________________________________________________________________________
# Make a heatmap of the correlation between rows in the Feature set
Gram = cor(t(Feature2))
row.names(Gram) = 1:nrow(Gram)

# Change up colors a bit
qnt2 = quantile(as.numeric(data.matrix(Gram)), c(0.01, .99))
brks2 = seq(qnt2[1], qnt2[2], length=20)

quartet_col2 = magma(n = length(unique(Anno$Quartet)))
set_col2 = viridis(n = length(levels(Anno$Set)))
set.seed(5)
quartet_col2 = sample(quartet_col2) # Randomize quartet colors (so that it's easier to see)
names(quartet_col2) = unique(Anno$Quartet)
names(set_col2) = unique(Anno$Set) 
set_col3 = sample(set_col2)         # Randomize set colors (so that it's easier to see)
names(set_col3) = unique(Anno$Set)

category_col = cividis(n = 5)
names(category_col) = unique(feature_cats)

# Duplicate row annotation to have column annotation too (makes Gram matrix easier to interpet)
Anno_col = Anno
rownames(Anno_col) = colnames(Gram)

viridis_col = viridis(n = length(brks - 1))

# Makes full Gram matrix 
make_cor_heatmap()

# Make Gram matrix for each pair of feature category and composer
# (Haydn)
make_cor_heatmap_feature_composer("recap", "Haydn")
make_cor_heatmap_feature_composer("expo", "Haydn")
make_cor_heatmap_feature_composer("dev", "Haydn")
make_cor_heatmap_feature_composer("int", "Haydn")
make_cor_heatmap_feature_composer("basic", "Haydn")
# (Mozart)
make_cor_heatmap_feature_composer("recap", "Mozart")
make_cor_heatmap_feature_composer("expo", "Mozart")
make_cor_heatmap_feature_composer("dev", "Mozart")
make_cor_heatmap_feature_composer("int", "Mozart")
make_cor_heatmap_feature_composer("basic", "Mozart")
