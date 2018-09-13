# haydn-mozart
Classification of Haydn and Mozart String Quartets

We provide the dataset and code used to classify string quartet movements as being composed by either Haydn or Mozart. 

The repository contains 292 ** kern files obtained from the KernScores website http://kern.ccarh.org/ (maintained by the Center for Computer Assisted Research in the Humanities at Stanford University). Additionally, we provide 3 code scripts written in the statistical programming language R. Please set your R working directory to the location of your local repository before running the scripts.

Our first script Clean_Music_Data.R reads in the ** kern files, cleans the data, and encodes the pitch and duration values in each movement. Our resulting dataset consists of 285 movements total, 82 composed by Mozart and 203 composed by Haydn. The resulting cleaned dataframes (3 for each of the 285 movements) get saved to a specified R workspace. 

The second script Compute_Music_Features.R relies on Clean_Music_Data.R to be run first. Using the cleaned dataframes generated from the previous script, Compute_Music_Features.R calculates features for each movement. The resulting 285 x 1117 dataframe is saved as Feature.csv.

The last script Music_Feature_Selection_and_Classification.R does not require the other two scripts to be run first, since it simply takes in as input the Feature.csv file. In this script, a small subset of features is identified through feature selection (using the method of random iterative conditional minimization). We fit a Bayesian logistic regression model with the selected subset of features. Three cross-validation schemes are used to obtain accuracy, sensitivity, and specificity rates. Finally, several plots summarizing the results are made. 

For a full discussion of our methods, please see our paper "Where Does Haydn End and Mozart Begin? Composer Classification of String Quartets". 
