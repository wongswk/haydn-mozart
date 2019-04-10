#This script only requires Feature.csv.

#Based on the feature data frame, this script identifies a model for each LOO fold using random iterative conditional minimization.
#Then each left-out movement is classified as Mozart or Haydn from the training fold's model.
#Finally, plots and outputs are made that summarize some results. 

#Load the necessary libraries 
library(arm) #for Bayesian glm
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(lattice) #for stripplot
library(ResourceSelection) #for Hosmer-Lemeshow test

#Define the path where Feature.csv is saved
fpath="data/"

Feature=read.csv(paste0(fpath,"Feature.csv"))[,-1]
dim(Feature)



#_____________________________________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________ Define functions ________________________________________________________________________________#
#______________________________________________________________________________________________________________________________________________________________________________

#This function does random ICM, given the training data frame and a random seed to initialize the algorithm.
#We only use BIC here, but the function enables the usage of AIC or BIC through the criterion argument. 

random_ICM=function(train_data,seed=123456,criterion="BIC"){
  if (criterion=="BIC"){
    penalty_string="2 * (log(n) - 2) * (length(index))"
  } else if (criterion=="AIC"){
    penalty_string="0"
  }
  set.seed(seed)
  varlist=c()
  varnames=names(train_data)
  numF=ncol(train_data)
  n=nrow(train_data)
  minCrit <- Inf
  noChange <- F
  while (!noChange) {
    noChange <- T
    for (i in sample(2:numF)) { 
      
      if (i %in% varlist & length(varlist)>1) {
        index <- c(1, varlist[varlist!=i]) 
        train.set <- train_data[,index]
        
        fit=bayesglm(M0~., data=train.set,family=binomial)
        
        if (fit$converged & fit$aic + eval(parse(text=penalty_string)) < minCrit) {
          minCrit <- fit$aic + eval(parse(text=penalty_string))
          varlist <- varlist[varlist!=i]
          best.model <- fit
          noChange <- F
        }
        
      } else if (!i %in% varlist) {
        index <- c(1, varlist, i) 
        train.set <- train_data[,index]
        fit=bayesglm(M0~., data=train.set,family=binomial)
        
        
        if (fit$converged & fit$aic + eval(parse(text=penalty_string))  < minCrit) {
          minCrit <- fit$aic + eval(parse(text=penalty_string))
          varlist <- c(varlist, i)
          best.model <- fit
          noChange <- F
        }    
        
      }
    }
  }
  index=c(1,varlist)
  #return(list(best.model,index))
  return(list(minCrit,index))
}

#This function calls random ICM 10 times, once for each randomly generated seed. 
run_random_ICM=function(train_data,criterion="BIC"){
  #seeds=round(runif(10)*10^6,0)
  seeds=c(690893, 965097, 659011, 253073, 834997,386972,190566, 257693, 181779, 329323)
  results=lapply(seeds,function(x) random_ICM(train_data,x,criterion))
  return(results)
}


# This function performs our modified correlation ranking to reduce the feature set
filter_vars=function(train){
  keep=c()
  group_cors=cor(train[,-1])
  cors=abs(apply(train[,-1],2,function(x) cor(train[,1],x) ))
  keepers=c(2:ncol(train))[sort_f(corf=group_cors,goodf=cors)]
  keep=c(keep,keepers)
  
  keep=c(1,keep)
  return(keep)
}

#This algorithm is part of the filter_vars function
sort_f=function(corf,goodf,cor_threshold=.5){
  keep <- rep(TRUE, length(goodf))
  for (i in order(goodf,decreasing=T)) {
    if (!keep[i]) {
      next
    }
    
    keep[abs(corf[i,])>cor_threshold] <- FALSE
    keep[i] <- TRUE
    
  }
  return(keep)
}


#This is a general function that converts probabilities to classes for binary classification problems and computes the classification accuracy, sensitivity, and specificity. 
#There are numerous ways to set thresholds that are built-in to this function.

classify_f=function(response,pred,lev=c(-1,1), try=NULL, sep.try=NULL, majrule_inds=NULL, return_class=F){
  if (is.null(try) & (is.null(sep.try))){ #This is for the case that you have predictions (not probabilities) already and only need to calculate the accuracy, sensitivity, and specificity.
    response=factor(response,levels=lev)
    pred=factor(pred,levels=lev)
    tab = table(response,pred)
    acc=(tab[2,2]+tab[1,1])/sum(tab)
    spec=tab[1,1]/sum(tab[1,])
    sens=tab[2,2]/sum(tab[2,])
    return(list(acc,sens,spec))
  } else if (!is.null(try)){ #This is for the case that you have predicted probabilities and a range of cutoff values you want to try
    ACC=c(); SENS=c(); SPEC=c()
    for (j in try){
      log.class = ifelse(pred >= j,lev[2],lev[1])
      
      if (!is.null(majrule_inds)){ #This is for a very specific situation common in longitudinal datasets (if we know a group of observations in training should have the same class label)
        maj_pred=unlist(lapply(unique(majrule_inds), function(x) ifelse(mean(log.class[which(majrule_inds==x)]==1) > .5, 1, 2))) #take majority rule for each training group
        log.class=unlist(lapply(1:length(maj_pred), function(x) rep(maj_pred[x],length(which(majrule_inds==unique(majrule_inds)[x])))))
      }
      
      response=factor(response,levels=c(lev[1],lev[2]))
      log.test=factor(log.class,levels=c(lev[1],lev[2]))
      tab = table(response,log.test)
      acc=(tab[2,2]+tab[1,1])/sum(tab)
      spec=tab[1,1]/sum(tab[1,])
      sens=tab[2,2]/sum(tab[2,])
      ACC=c(ACC,acc)
      SENS=c(SENS,sens)
      SPEC=c(SPEC,spec)
      
    }
    return(list(ACC,SENS,SPEC))
  } else { #This is for the case that you have a separate desired cutoff per predicted probability
    log.class = unlist(lapply(1:length(pred), function(x) ifelse(pred[x] >= sep.try[x],lev[2],lev[1])))
    response=factor(response,levels=c(lev[1],lev[2]))
    log.test=factor(log.class,levels=c(lev[1],lev[2]))
    tab = table(response,log.test)
    acc=(tab[2,2]+tab[1,1])/sum(tab)
    spec=tab[1,1]/sum(tab[1,])
    sens=tab[2,2]/sum(tab[2,])
    if (return_class==T){
      return(list(acc,sens,spec,log.test))
    } else {
      return(list(acc,sens,spec))
    }
  }
}

#If there are multiple "best" cutoffs, this function will return the one that's closest to .5 
one_cutoff=function(cutoff_vec){
  if (length(cutoff_vec)>1){
    dist=abs(cutoff_vec-.5)
    cutoff_ind=which(dist==min(dist))[1]
    cutoff=cutoff_vec[cutoff_ind]
  } else {
    cutoff=cutoff_vec
  }
  return(cutoff)
}

#This function trains and tests bayesglm and obtains the best probability threshold from training
bayesglm_f=function(train.set,test.set){
  train.set$M0=as.factor(train.set$M0)
  test.set$M0=as.factor(test.set$M0)
  log.fit=bayesglm(M0~., data=train.set,family=binomial)
  test.probs=predict(log.fit,type="response",newdata=test.set)
  train.probs=predict(log.fit,type="response",newdata=train.set)
  
  train.results=classify_f(train.set$M0, train.probs, lev=c(0,1),try=c(0:100)*.01)
  
  return(list(train.results,test.probs,log.fit))
}

#This function calls bayesglm for LOO
call_bayesglm_f=function(Data, LOO_ind, index){
  train=Data[-LOO_ind,index]
  test=Data[LOO_ind,index]
  results=bayesglm_f(train,test)
  return(results)
}

#This function runs feature selection on each LOO training fold
LOO_ICM_i=function(Features=Feature, LOO_ind){
  test.ID=LOO_ind
  train.set=Features[-test.ID,]
  index=filter_vars(train.set)
  
  train.set=Features[-test.ID,index]
  train.set$M0=as.factor(train.set$M0)
  
  ICM_results=run_random_ICM(train.set,criterion="BIC")
  
  BICs=lapply(ICM_results,"[[",1)
  vars_index=lapply(ICM_results,"[[",2)
  vars_index=lapply(vars_index,function(x) index[x]) #gets the indices of selected variables (in the original Feature set, not the reduced feature set)
  results=lapply(1:10, function(x) list(BICs[[x]],vars_index[[x]]))
  
  return(results)
}

#This function parses the lists generated from calling LOO_ICM, writes an output of the selected variables from each fold, and returns the testing results 
parse_results=function(Features=Feature,LOO_results,filename){
  cutoff_vec=c(1:100)*.01
  Features_M0=factor(Features$M0,levels=c(0,1))
  
  BICs=c()
  VARs=c()
  for (i in 1:10){
    trial_i=lapply(LOO_results,"[[",i)
    BICs=c(BICs,unlist(lapply(trial_i,"[[",1)))
    VARs=c(VARs,lapply(trial_i,"[[",2))
  }
  BIC_mat=data.frame(matrix(BICs,nrow=285,ncol=10))
  names(BIC_mat)=paste0("Trial",c(1:10))
  BIC_min=apply(BIC_mat,1,min)
  BIC_inds=lapply(BIC_min, function(j) which(BIC_mat == j, arr.ind=T)[1,])
  
  #From 10 trials of random ICM, chooses the variables from the trial that achieved minimum BIC
  get_vars=lapply(1:285, function(i) LOO_results[[i]][[BIC_inds[[i]][2]]][[2]] )
  names_vars=lapply(get_vars, function(x) names(Features)[sort(x)])
  
  results=lapply(1:285, function(i) call_bayesglm_f(Features, i, get_vars[[i]])) #refits "best model" for each training fold and gets corresponding testing fold prediction
  cat(capture.output(print(names_vars), file=paste0(fpath,filename,".txt") ))
  
  test.pred=unlist(lapply(results,"[[",2)) #gets test probabilities
  train.results=lapply(results,"[[",1)
  train.acc=lapply(train.results,"[[",1)
  train.maxacc=lapply(train.acc,max)
  train.cutoff=unlist(lapply(train.results, function(x) one_cutoff(cutoff_vec[which(x[[1]]==max(x[[1]]))]))) #gets best cutoff (one that maximizes training classification accuracy) from each training fold
  
  test.results_sepcutoff=classify_f(Features$M0, test.pred, lev=c(0,1),sep.try=train.cutoff, return_class=T) 
  #list item 1 is LOO accuracy, list item 2 is LOO sensitivity (proportion of Haydn movements corrrectly classified), 
  #list item 3 is LOO specificity (proportion of Mozartn movements correctly classified), and list item 4 are the LOO/testing predicted classes
  return(list(names_vars, test.results_sepcutoff, test.pred))
}

#This function fits one model of composer (on all movements), following the same process as before but without LOO or other CV 
Full_results=function(){
  Feature=read.csv(paste0(fpath,"Feature.csv"))[,-1]
  
  index=filter_vars(Feature)
  p_red=length(index)-1
  
  ICM_results=run_random_ICM(Feature[,index],criterion="BIC")
  
  BICS=unlist(lapply(ICM_results,"[[",1))
  VARS=lapply(ICM_results,"[[",2)
  
  best_ind=which(BICS==min(BICS))[1]
  best_BIC=BICS[best_ind]
  
  best_vars=index[VARS[[best_ind]]]
  
  full_results=bayesglm_f(Feature[,best_vars],Feature[,best_vars])
  full_probs=full_results[[2]]
  full_model=full_results[[3]]
  
  return(list(full_model,best_BIC, full_probs, index, best_vars ))
}

make_jitter_plot=function(pred,misclassified,filepath=fpath){
  Pred.df=data.frame(Probability=pred,Composer=c(rep("Mozart",82),rep("Haydn",203)))
  Pred.df$Composer=factor(Pred.df$Composer,levels=c("Mozart","Haydn"))
  pdf(paste0(filepath,"Composer_Jitter_Plot.pdf"))
  col_vec=rep("black",285)
  col_vec[misclassified]=rep("gray60",length(misclassified))
  print(stripplot(Composer ~ Probability,data=Pred.df,main=list(label="Estimated Probability of Composer",fontsize=14),col=col_vec,
                  xlab=list(label="Probability",fontsize=14),ylab=list(label="Composer",fontsize=14),pch=17,jitter=T,factor=1.5,cex=1, scales=list(cex=1.1),
                  panel = function(...){
                    panel.stripplot(...) 
                    #panel.abline(v=.5,lty=2)
                  }))
  dev.off()
}

make_barplot=function(){
  uniq_vars=unique(unlist(names_vars))
  uniq_vars=uniq_vars[-1]
  
  tab=table(unlist(names_vars))
  print(tab)
  df=data.frame(Variables=names(tab)[-which(names(tab)=="M0")], Count=as.vector(tab[-which(names(tab)=="M0")]))
  
  gray_colors=gray.colors(5)
  Category=c(rep("Basic",3), rep("Interval",14), "Exposition", rep("Development",6), rep("Recapitulation"))
  color_inds=c(rep(gray_colors[1],3), rep(gray_colors[2],14),gray_colors[3], rep(gray_colors[4],6), gray_colors[5])
  
  x_names=paste0(c(1:25))
  df_new=cbind(df[c(24,22,23,8,9,16,17,14,15,13,12,25,10,11,18,20,19,7,6,3,2,5,4,1,21),],Category) #variables are ordered by decreasing frequency within each category
  df_return=df_new
  df_new$Variables=factor(c(1:25),levels=c(1:25))
  df_new$Category=factor(df_new$Category,levels=unique(Category))
  pdf(paste0(fpath,"LOO_barplot.pdf"))
  print(ggplot(df_new, aes(x=Variables,y=Count, fill=Category)) + geom_bar(stat="identity") + scale_fill_manual(values=gray_colors)+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.title=element_text(size=12),axis.text=element_text(size=10), plot.title = element_text(hjust = 0.5, size=16, face="bold"))+
    labs(y="Count",x="Feature", title="Count of Selected Features in LOO") + scale_y_continuous(breaks=c(seq(0.0, 285, 50),285), limits=c(0, 285)))
  dev.off()
  return(df_return)
}

make_relfreq_plot=function(){
  Feature_subset=cbind(Feature[,1],c(rep("Mozart",82),rep("Haydn",203)),Feature[,index[-1]])
  names(Feature_subset)=c("name","Composer",names(Feature_subset)[3:ncol(Feature_subset)])
  Feature_subset$Composer=factor(Feature_subset$Composer,levels=c("Mozart","Haydn"))
  plot_names=c("SD count for pitch & Viola", "SD count for pitch & Cello","Mean prop. of m3 intervals for Viola",
               "Prop. of desc. intervals for Violin 1", "Prop. of intervals with dist. 3 for Cello","Mean prop. of m3 intervals for Cello",
               "SD of duration for Violin 1","Max. fraction for duration & Viola", "SD of pitch for Viola")
  feature_names=c(rep("(Development)",2),rep("(Interval)",4),"(Basic)","(Recapitulation)","(Basic)")
  
  for (i in 1:(length(index)-1)){
    data=Feature_subset[,c(2,2+i)]
    names(data)=c("Composer",names(data)[2])
    varname=names(data)[2]
    plot=ggplot(data, aes_string(varname,fill="Composer")) + geom_histogram(aes(y=2*(..density..)/sum(..density..)),position="dodge")+labs(y="",x=plot_names[i], title=feature_names[i])+
      scale_fill_manual(values=c("gray60", "black"))+theme(axis.title=element_text(size=8),axis.text=element_text(size=6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                                           plot.title=element_text(size=10)) 
    
    plotname=paste("Plot_",i,sep="")
    assign(plotname,plot,.GlobalEnv)
  }
  pdf(paste0(fpath,"features_rel_freq.pdf"))   
  print(ggarrange(Plot_1, Plot_2, Plot_3, Plot_4, Plot_5, Plot_6, Plot_7, Plot_8, Plot_9 + 
              font("x.text", size = 8), ncol=3, nrow=3, common.legend = TRUE, legend="bottom",
            labels=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)"),
            font.label = list(size = 10, color = "black", face = "bold", family = NULL, labels=c("Mozart","Haydn"))))
  dev.off()
}

#______________________________________________________________________________________________________________________________________________________________________________
#______________________________________________ Run LOO feature selection and composer classification    ______________________________________________________________________
#______________________________________________________________________________________________________________________________________________________________________________

################################### #Run LOO
LOO_results=lapply(1:285, function(x) LOO_ICM_i(Feature,x))
LOO=parse_results(Feature, LOO_results, filename="LOO_Random_ICM_Variables")
LOO_accuracy=LOO[[2]][[1]]
LOO_Haydn_accuracy=LOO[[2]][[2]]
LOO_Mozart_accuracy=LOO[[2]][[3]]
test.class=LOO[[2]][[4]] #test classes
test.pred=LOO[[3]] #test probabilities
names_vars=LOO[[1]] #names of variables selected in each fold

###################################
#Run LOO on simple subset of features (interval and basic features only)

adv=c(grep("Recap",colnames(Feature)),grep("Dev",colnames(Feature)), grep("Expo",colnames(Feature)))
Simple_Feature=Feature[,-adv]
LOO_simple_results=lapply(1:285, function(x) LOO_ICM_i(Simple_Feature,x))
LOO_simple=parse_results(Simple_Feature, LOO_simple_results, filename="LOO_Random_ICM_Simple_Variables")
LOO_simple_accuracy=LOO_simple[[2]][[1]]
LOO_simple_Haydn_accuracy=LOO_simple[[2]][[2]]
LOO_simple_Mozart_accuracy=LOO_simple[[2]][[3]]

#################################### Generate plots and other outputs
# Make summary of accuracy, Haydn accuracy, and Mozart accuracy 
result_df=data.frame(Accuracy=c(LOO_accuracy,LOO_simple_accuracy), Haydn_Accuracy=c(LOO_Haydn_accuracy,LOO_simple_Haydn_accuracy), 
                                                                                    Mozart_Accuracy=c(LOO_Mozart_accuracy, LOO_simple_Mozart_accuracy))
row.names(result_df)=c("LOO","LOO_simple_features")
print(result_df)
write.csv(result_df,paste0(fpath,"LOO_Results.csv"))

Feature_M0=factor(Feature$M0,levels=c(0,1))

#Make jitter plot of composer probabilities
misclassified=which(test.class!=Feature_M0)
make_jitter_plot(test.pred,misclassified)

#Get confusion matrix 
table(Feature_M0, test.class)

#Summarize variables chosen across folds
counts=make_barplot() 

#Get counts of commonly selected features in LOO (features selected 200 or more times)..
counts[which(counts$Count >= 200),]


#________________________________________________________________________________________________________________________________________________________________________________________________________________________
#__________________________________________________    Fit one model on full set of data using the same modeling process ______________________________________________________________________
#________________________________________________________________________________________________________________________________________________________________________________________________________________________
Full_model=Full_results()

summary(Full_model[[1]])
Full_model[[2]]
Full_probs=Full_model[[3]]
reduced_f=Full_model[[4]]
reduced_p=length(reduced_f)
index=Full_model[[5]] #subset of variables in final model

#Test goodness of fit using Hosmer-Lemeshow test for a range of group sizes (from 20 to 100)
g_val=c(20:100)
pval=c()
for (j in g_val){
  test=hoslem.test(Feature[,1],Full_probs,g=j)
  pval=c(pval,test$p.value)
  
}
summary(pval)

#Relative frequency plots by composer for each selected feature
make_relfreq_plot()

