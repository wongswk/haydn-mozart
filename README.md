# haydn-mozart
Classification of Haydn and Mozart String Quartets

This repository provides the datasets and code used to classify string quartet movements as being composed by either Haydn or Mozart. 

### Datasets

The data consist of 292 **kern files obtained from the KernScores website http://kern.ccarh.org/ (maintained by the Center for Computer Assisted Research in the Humanities at Stanford University) and 107 additional **kern files kindly provided by Dr. Peter van Kranenberg.  They are organized into two datasets:

- HM107: 54 Haydn movements and 53 Mozart movements, as used in the van Kranenburg and Backer (2005) and Velarde et al. (2018, 2016) studies.
- HM285: 203 Haydn movements and 82 Mozart movements.

### Code

The code to produce the analyses in our paper are provided as scripts written in the statistical programming language R. Please set your R working directory to the location of your local repository before running the scripts.

A brief description of each script follows, and further documentation is provided within the script itself.

#### Main analyses

- 1_create_feature_sets.R:  reads in the **kern files, cleans the data, and creates the feature sets
- 2_classify_HM285.R: classifies the composer of each movement in the HM285 dataset using the leave-one-out (LOO) CV scheme
- 3_classify_HM107.R: classifies the composer of each movement in the HM107 dataset using the LOO CV scheme
- 4_classify_simple_HM285.R: classifies the composer of each movement in the HM285 dataset using the LOO CV scheme with a set of simple features only
- 5_classify_LOQO_HM285.R: classifies the composer of each movement in the HM285 dataset using the leave-one-quartet-out (LOQO) CV scheme
- 6_classify_full_HM285.R: fits one model on all 285 movements in the HM285 dataset for interpretation purposes
- 7_interpret_results.R: produces some plots, tables, and other summaries of results on the HM107 and HM285 datasets

#### Supplementary analyses

- S1-1_create_expanded_feature_sets.R:  creates expanded feature sets that includes all segment lengths for the segment features 
- S1-2_analyze_segment_features.R:  makes heatmaps of correlation matrices for sets of segment features for assessing similarity
- S2_make_feature_heatmaps.R: generates heatmaps of the feature matrix X, the Gram matrix XX', and their variants
- S3_analyze_predictions.R: analyzes the estimated probabilities, classes, and selected features from LOO, LOQO, and LOO on the reduced/simple feature set

### Reference

For a full discussion of our methods, please see our paper "Where Does Haydn End and Mozart Begin? Composer Classification of String Quartets" (https://arxiv.org/abs/1809.05075). 
