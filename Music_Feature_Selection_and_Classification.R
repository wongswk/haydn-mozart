#This script only requires Feature.csv.
#Based on the feature data frame, this script identifies a model from random iterative conditional minimization.
#Then Mozart and Haydn string quartets are classified using that model and three difference cross-validation schemes.
#Finally, plots are made that summarize some results. 

#Load the necessary libraries 
library(arm)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(lattice)

#Define the path where Feature.csv is saved
fpath="C:\\Users\\Katie\\Documents\\RFiles\\String_Quartet_Classification\\"

#______________________________ Define functions ________________________#

#This function does random ICM, given the feature data frame, and a random seed to initialize the algorithm.
#We only use BIC later, but the function enables the usage of AIC or BIC through the criterion argument. 

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

#Given the original feature data frame and the appropriate columns for selected variables 
#(as well as the column of composer), this function uses Bayesian logistic regression plus LOO CV 
#to classify the composer.

LOO_bayesglm=function(Feature,index){
  pred <- c()
  for (k in 1:nrow(Feature))
  {
    test.ID=k
    train.set=Feature[-test.ID,index]
    test.set=Feature[test.ID,index]
    train.set$M0=as.factor(train.set$M0)
    test.set$M0=as.factor(test.set$M0)
    log.fit=bayesglm(M0~., data=train.set,family=binomial)
    log.probs=predict(log.fit,type="response",newdata=test.set)
    pred=c(pred,log.probs) 
  }
  log.class = ifelse(pred > 0.50,1,0)
  Feature_M0=factor(Feature$M0,levels=c(0,1))
  log.test=factor(log.class,levels=c(0,1))
  tab = table(Feature_M0,log.test)
  acc=(tab[2,2]+tab[1,1])/sum(tab)
  spec=tab[1,1]/sum(tab[1,])
  sens=tab[2,2]/sum(tab[2,])
  
  #Make jitter plot
  misclassified=which(log.test!=Feature_M0)
  Pred.df=data.frame(Probability=pred,Composer=c(rep("Mozart",82),rep("Haydn",203)))
  Pred.df$Composer=factor(Pred.df$Composer,levels=c("Mozart","Haydn"))
  pdf(paste0(fpath,"Jitter_Plot.pdf"))
  col_vec=rep("black",285)
  col_vec[misclassified]=rep("gray60",length(misclassified))
  print(stripplot(Composer ~ Probability,data=Pred.df,main=list(label="Estimated Probability of Composer",fontsize=14),col=col_vec,
            xlab=list(label="Probability",fontsize=14),ylab=list(label="Composer",fontsize=14),pch=17,jitter=T,factor=1.5,cex=1, scales=list(cex=1.1),
            panel = function(...){
              panel.stripplot(...) 
              panel.abline(v=.5,lty=2)}))
  dev.off()
  
  return(list(acc,sens,spec,pred))
}

#This function trains on 50 randomly selected Mozart movements and 50 randomly selected Haydn movements,
#then tests on the remaining movements. Since it depends on the random selection of movements, the process
#is done 10 times, once per random seed, and the results are averaged over the 10 runs. 
Even1_bayesglm=function(Feature,index,seeds){
  H=Feature[83:285,index[1]]
  M=Feature[1:82,index[1]]
  ACC=NULL; SENS=NULL; SPEC=NULL; AUC=NULL
  for (i in seeds){
    set.seed(i)
    x=sample((1:length(H)),size=50)
    y=sample((1:length(M)),size=50)
    index_x=x+82
    train.ID=c(y,index_x)
    test_index=c(1:nrow(Feature))[-c(train.ID)]
    train.set=Feature[train.ID,index]
    test.set=Feature[test_index,index]
    train.set$M0=as.factor(train.set$M0)
    test.set$M0=as.factor(test.set$M0)
    log.fit=bayesglm(M0~., data=train.set,family=binomial)
    log.probs=predict(log.fit,type="response",newdata=test.set)
    log.class = ifelse(log.probs > 0.5,1,0)
    Feature_M0=factor(test.set$M0,levels=c(0,1))
    log.test=factor(log.class,levels=c(0,1))
    tab = table(Feature_M0,log.test)
    acc=(tab[2,2]+tab[1,1])/sum(tab)
    spec=tab[1,1]/sum(tab[1,])
    sens=tab[2,2]/sum(tab[2,])
    ACC=c(ACC,acc)
    SENS=c(SENS,sens)
    SPEC=c(SPEC,spec)
  } 
  return(list(mean(ACC),mean(SENS),mean(SPEC)))
} 

#This function forms a subset of movements (54 randomly selected Haydn and 53 randomly selected Mozart),
#then does LOO on the subset. Since this depends on the random selection of movements, the process is repeated
#10 times, once per random seed, then the results are averaged at the end. 
Even2_bayesglm=function(Feature,index,seeds){
  H=Feature[83:285,index[1]]
  M=Feature[1:82,index[1]]
  ACC=NULL; SENS=NULL; SPEC=NULL
  for (i in seeds){
    set.seed(i)
    x=sample((1:length(H)),size=54)
    y=sample((1:length(M)),size=53)
    index_x=x+82
    Feature2=Feature[c(y,index_x),index]
    pred <- c()
    for (k in 1:nrow(Feature2))
    {
      test.ID=k
      train.set=Feature2[-test.ID,]
      test.set=Feature2[test.ID,]
      train.set$M0=as.factor(train.set$M0)
      test.set$M0=as.factor(test.set$M0)
      log.fit=bayesglm(M0~., data=train.set,family=binomial)
      log.probs=predict(log.fit,type="response",newdata=test.set)
      pred=c(pred,log.probs)
    }
    log.class = ifelse(pred > 0.50,1,0)
    Feature_M0=factor(Feature2$M0,levels=c(0,1))
    log.test=factor(log.class,levels=c(0,1))
    tab = table(Feature_M0,log.test)
    acc=(tab[2,2]+tab[1,1])/sum(tab)
    ACC=c(ACC,acc)
    spec=tab[1,1]/sum(tab[1,])
    sens=tab[2,2]/sum(tab[2,])
    SENS=c(SENS,sens)
    SPEC=c(SPEC,spec)
  }
  
  return(list(mean(ACC),mean(SENS),mean(SPEC)))
}


#______________________________ Run feature selection then classification ________________________#
Feature=read.csv(paste0(fpath,"Feature.csv"),head=T)[,-1]
Selection_results=run_random_ICM(Feature,"BIC")
BICs=c()
for (i in 1:10){
  BICs=c(BICs,Selection_results[[i]][[1]])
}
index_BIC=Selection_results[[which(BICs==min(BICs))]][[2]] #variable indices corresponding to best random seed 

#Get classification results
LOO_results=LOO_bayesglm(Feature,index=index_BIC)
#seeds_vec=round(runif(10)*1000000,0)
seeds_vec=c(407051,335557,420293,111795,247188,954375,293883,891403,231840,834537)
Even1_results=Even1_bayesglm(Feature,index=index_BIC,seeds=seeds_vec)
Even2_results=Even2_bayesglm(Feature,index=index_BIC,seeds=seeds_vec)

LOO_df=data.frame("LOO"=c(round(LOO_results[[1]],4),round(LOO_results[[2]],4),round(LOO_results[[3]],4)),row.names=c("Accuracy","Sensitivity","Specificity"))
Even1_df=data.frame("Even Subsets 1"=c(round(Even1_results[[1]],4),round(Even1_results[[2]],4),round(Even1_results[[3]],4)),row.names=c("Accuracy","Sensitivity","Specificity"))
Even2_df=data.frame("Even Subsets 2"=c(round(Even2_results[[1]],4),round(Even2_results[[2]],4),round(Even2_results[[3]],4)),row.names=c("Accuracy","Sensitivity","Specificity"))
print(LOO_df)
print(Even1_df)
print(Even2_df)

#______________________________ Make plots ________________________#
#Stacked barplot summarizing selected features by category and voice
df=data.frame(Category=rep(factor(c("Basic","Interval","Exposition","Development","Recapitulation"),levels=c("Basic","Interval","Exposition","Development","Recapitulation")),each=5),
              Count=c(0,0,0,2,0,
                      1,3,0,2,1,
                      1,0,0,0,0,
                      1,2,0,1,0,
                      1,1,0,0,0),
              Voice=rep(factor(c("Cello","Viola","Violin 2","Violin 1","Multi-voice"),levels=c("Cello","Viola","Violin 2","Violin 1","Multi-voice")),5))
df1=data.frame(category=factor(c("Basic","Interval","Exposition","Development","Recapitulation")),counts=c(1,7,3,6,1))
df2=data.frame(voice=factor(c("Cello","Viola","Violin 1","Violin 2")),counts=c(6,5,3,4))


purple_colors=brewer.pal(n = 7, name = "BuPu")[-c(1,3)]
gray_colors=gray.colors(5, start = 0, end = 0.9, gamma = 2.2, alpha = NULL)
pdf(paste0(fpath,"stacked_barplot.pdf"))
ggplot(df, aes(x = Category, y = Count, fill=Voice)) + scale_fill_manual(values = gray_colors) +
  geom_bar(stat='identity')+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.title=element_text(size=12),axis.text=element_text(size=11))
dev.off()


#Relative frequency plots by composer for each selected feature
Feature_subset=cbind(Feature[,1],c(rep("Mozart",82),rep("Haydn",203)),Feature[,index[-1]])
names(Feature_subset)=c("name","Composer",names(Feature_subset)[3:ncol(Feature_subset)])
Feature_subset$Composer=factor(Feature_subset$Composer,levels=c("Mozart","Haydn"))
plot_names=c("SD count for pitch & Cello","Prop. of desc. intervals in Violin 1","SD count for pitch & Viola",
             "SD of duration for Violin 1","Max. frac. for duration & Cello", "Prop. of m3 intervals for Viola",
             "Mean pitch for Violin 1", "Max. frac. for duration & Viola", "Count of m3 ints. for Viola (n=8)",
             "Diff. in prop. of P5 ints. for Cello & Violin 1","Max. frac. for duration & Cello",
             "Prop. of major intervals in Violin 1", "Count of m3 ints. for Viola (n=16)", "Mean prop. of m3 intervals for Cello",
             "Percentile of max SD for pitch & Viola", "SD count for pitch & Violin 1")

for (i in 1:(length(index)-1)){
  data=Feature_subset[,c(2,2+i)]
  names(data)=c("Composer",names(data)[2])
  varname=names(data)[2]
  plot=ggplot(data, aes_string(varname,fill="Composer")) + geom_histogram(aes(y=2*(..density..)/sum(..density..)),position="dodge")+labs(y="",x=plot_names[i])+
    scale_fill_manual(values=c("gray60", "black"))+theme(axis.title=element_text(size=6),axis.text=element_text(size=6),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  plotname=paste("Plot_",i,sep="")
  assign(plotname,plot,.GlobalEnv)
}
pdf(paste0(fpath,"features_rel_freq.pdf"))   
ggarrange(Plot_1, Plot_2, Plot_3, Plot_4, Plot_5, Plot_6, Plot_7, Plot_8, Plot_9, Plot_10, Plot_11, Plot_12, Plot_13, Plot_14, Plot_15, Plot_16 + 
            font("x.text", size = 6), ncol=4, nrow=4, common.legend = TRUE, legend="bottom",
          labels=c("(1)","(2)","(3)","(4)","(5)","(6)","(7)","(8)","(9)","(10)","(11)","(12)","(13)","(14)","(15)","(16)"),
          font.label = list(size = 5, color = "black", face = "bold", family = NULL, labels=c("Mozart","Haydn")))
dev.off()

