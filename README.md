# haydn-mozart
Classification of Haydn and Mozart String Quartets

We provide the dataset and code used to classify string quartet movements as being composed by either Haydn or Mozart. 

The repository contains 292 ** kern files obtained from the KernScores website http://kern.ccarh.org/ (maintained by the Center for Computer Assisted Research in the Humanities at Stanford University). Additionally, we provide 3 code scripts written in the statistical programming language R. Please set your R working directory to the location of your local repository before running the scripts.

The first script Clean_Music_Data.R reads in the ** kern files, cleans the data, and encodes the pitch and duration values in each movement. Our resulting dataset consists of 285 movements total, 82 composed by Mozart and 203 composed by Haydn. The resulting cleaned dataframes (3 for each of the 285 movements) are stored in the R workspace. 

The second script Compute_Music_Features.R requires Clean_Music_Data.R to be run first. Using the cleaned dataframes generated from the previous script, Compute_Music_Features.R calculates features for each movement. The resulting 285 x 1117 dataframe is saved as Feature.csv.

The main script for the classifier is LOO_Variable_Selection_and_Classification.R.  It does not require the other two scripts to be run first, since it simply takes in as input the Feature.csv file, a copy of which is provided inside the "data" folder. In this script, a small subset of features is identified through feature screening and selection (using correlation ranking and iterative conditional minimization, respectively). We fit a Bayesian logistic regression model with the selected subset of features. Leave-one-out (LOO) cross-validation is used to assess the accuracy of the classifier. Finally, several plots summarizing the results are made. 

For a full discussion of our methods, please see our paper "Where Does Haydn End and Mozart Begin? Composer Classification of String Quartets" (https://arxiv.org/abs/1809.05075). 
