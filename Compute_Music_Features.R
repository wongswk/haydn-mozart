#Only run this after running Clean_Music_Data.R. 
#Based on the 285*3 data files made from Clean_Music_Data.R, this script computes the features and forms 
#one 285 x 1117 data frame, where column 1 is the name of the movement, column 2 is a dummy variable of composer
#(0 for Mozart and 1 for Haydn), and the remaining columns are predictors.


#Load in necessary libraries
library(Hmisc)


#_____________________________________________________________________________________________
#---------------------------                                    ---------------------------
#---------------------------          Define Functions        ---------------------------
#---------------------------                                    ---------------------------
#____________________________________________________________________________________________

#Clean up function takes in a movement and the desired voice and removes NAs and converts the voice to numeric.
clean_up.v=function(mvmt,v,expo=FALSE){
  voice=eval(as.symbol(mvmt))[,v]
  voice[is.na(voice)]=""
  voice=gsub("0","",voice)
  whiterows=NULL
  for (j in 1:length(voice)){
    if (voice[j]==""){
      whiterows=c(whiterows,j)
    }
  }
  voice=voice[-whiterows]
  if (expo==T){
    len=round((.5*length(voice)),1)
    return(voice[1:len])
  } else {
    return(as.numeric(voice)) 
  }
}


#____________________________________________________________________
                      # PITCH 
#____________________________________________________________________

#________________________Transpose Functions ________________________ 
transform=function(vec){
  tonic=vec[1]
  scale=tonic:(tonic+11)
  return(scale)
}
orig_pitch=function(vec){
  tonic=vec[1]
  if (tonic==1){
    scale=c(1:12)
    return(scale)
  } else {
    scale=tonic:(tonic+11)
    transform=scale[(14-tonic):12]-12
    scale[(14-tonic):12]=transform
    return(scale)
  }
}

scale_mat=apply(matrix(rep(1:12,each=12),nrow=12,ncol=12),2,transform) #new encoding
scale_mat2=apply(matrix(rep(1:12,each=12),nrow=12,ncol=12),2,orig_pitch) #old encoding

transpose_func=function(segment){
  tonic=segment[1]
  old_scale=scale_mat2[,tonic]
  new_scale=scale_mat[,tonic]
  map=function(elem){
    position=which(old_scale==elem)
    new_elem=new_scale[position]
    return(new_elem)
  }
  mapping=unlist(lapply(segment,map))-tonic+1
  return(mapping)
}

#transposition for full octave information
transpose_octave=function(vec){
  vec_mod=vec%%12
  vec_mod[which(vec_mod==0)]=rep(12,length(which(vec_mod==0)))
  vec_mod_trans=transpose_func(vec_mod)
  vec_rem=(vec%/%12)*12
  new_vec=vec_mod_trans+vec_rem
  return(new_vec)
}

#________________________Recapitulation and Exposition Functions ________________________ 

#Computes fraction of overlap between opening segment and each subsequent segment
ACC=function(voice,n,FUN=identity){
  voice=as.numeric(voice)
  opening=as.numeric(as.character(voice[1:n]))
  dist=FUN(opening)
  beyond=as.numeric(voice[-c(1:n)])
  acc=NULL
  if (length(beyond) >=n){
    for (j in 2:(length(voice)-n+1)){
      y=voice[j:(j+n-1)]
      dist2=FUN(y)
      acc=c(acc,mean(dist2==dist))
    }
  } else {acc=0}
  return(acc)
}

#Calculates maximum fraction of overlap and its associated percentile
calcs=function(voice,acc){
  max_acc=max(acc)
  recap.pt=which(acc==max_acc)
  last_recap.pt=recap.pt[length(recap.pt)] #select last sequence
  percentile=last_recap.pt/length(acc)
  return(list(max_acc,percentile))
}

#Computes number of fraction of overlap matches for a vector of thresholds t 
matches_found=function(violin1,acc,thresholds,exact=F){
  num_matches=c()
  if (!exact){
    for (i in thresholds){
      num_matches=c(num_matches,length(which(acc >= i)))
    } 
  } else {
    for (i in thresholds){
      num_matches=c(num_matches,length(which(acc == i)))
    } 
  }
  return(num_matches)
}

#1) This function searches for the closest, last match that is the same (up to a key change) 
#as the opening segment. 
key_transpose=function(mvmt,n,FUN=clean_up.v,voice=4,expo=F){
  if (expo){
    get_violin1=FUN(mvmt,voice,T)
  } else {
    get_violin1=FUN(mvmt,voice) 
  }
  get_acc=ACC(get_violin1,n,transpose_func)
  get_calcs=calcs(get_violin1,get_acc)
  return(get_calcs)
}

get_len=function(mvmt,voice=4){
  get_violin1=FUN(mvmt,voice)
  return(length(get_violin1))
}

#2) Similar to function 1, but simply returns the number of matches found. 
key_transpose2=function(mvmt,n,FUN=clean_up.v,voice=4,thresholds=c(.70,.85,1),exact=F){
  get_violin1=FUN(mvmt,voice)
  get_acc=ACC(get_violin1,n,transpose_func)
  get_matchnum=matches_found(get_violin1,get_acc,thresholds,exact)
  return(get_matchnum)
}

#______________________________Development functions ______________________________________
#This function returns the standard deviation for each segment in the voice of a movement
get_SD.thresh=function(violin1,n,FUN=transpose_func,thresholds,summary=T){
  sd=NULL
  if (length(violin1) >=n){
    for (j in 2:(length(violin1)-n+1)){
      y=violin1[j:(j+n-1)]
      violin1_dist=FUN(y)
      sd=c(sd,sqrt(var(violin1_dist)))
    }
  } else {sd=sqrt(var(violin1))}
  if (summary==T){
    return(summary(sd)) 
  } else {
    return(sd) 
  }
}

#Calls other SD threshold function
Dev_max_SD.thresh=function(mvmt,n,FUN=clean_up.v,voice=4,thresholds,summary=T){
  get_violin1=as.numeric(clean_up.v(mvmt,voice))
  get__SD=get_SD.thresh(get_violin1,n,transpose_func,thresholds,summary)
  return(get__SD)
}

#Computes maximum standard deviation in the voice of a movement, count of standard deviations at certain thresholds,
#, and the percentile of maximum standard deviation
get_SD=function(violin1,n,FUN=transpose_func,thresholds){
  n=n_vec[n]
  sd=NULL
  if (length(violin1) >=n){
    for (j in 2:(length(violin1)-n+1)){
      y=violin1[j:(j+n-1)]
      violin1_dist=FUN(y)
      sd=c(sd,sqrt(var(violin1_dist)))
    }
  } else {sd=sqrt(var(violin1))}
  max_sd=max(sd)
  violin1_dev.pt=which(sd==max_sd)
  violin1_dev.pt=which(sd==max_sd)[1] #pick first segment
  percentile=violin1_dev.pt/length(sd)
  matches=c()
  for (i in thresholds){
    matches=c(matches,length(which(sd >=i)))
  }
  return(list(max_sd,percentile,matches))
}

#Calls other max_SD function
Dev_max_SD=function(mvmt,n,FUN=clean_up.v,voice=4,thresholds){
  get_violin1=as.numeric(clean_up.v(mvmt,voice))
  get__SD=get_SD(get_violin1,n,transpose_func,thresholds)
  return(get__SD)
}

#______________________________Basic features functions ______________________________________
#Calculates standard deviation of pitch values
SD_pitch=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  SD.p=sqrt(var(c_mvmt))
  return(SD.p)
}

#Calculates mean of all pitch values
mean_pitch=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  mean.p=mean(c_mvmt)
  return(mean.p)
}

#Counts the number of notes in the voice of a movement
count_notes=function(mvmt,v){
  violin1=eval(as.symbol(mvmt))[,v]
  violin1[is.na(violin1)]=rep("",length(violin1[is.na(violin1)]))
  whiterows=NULL
  for (j in 1:length(violin1)){
    if (violin1[j]==""){
      whiterows=c(whiterows,j)
    }
  }
  violin1=violin1[-whiterows]
  return(length(violin1))
}

#Calculates the proportion of simultaneous notes then the proportion of simultaneous rests between all voices of a movement
simult_note_f=function(name){
  df=eval(as.symbol(name))
  df.num=t(apply(df,1,as.numeric))
  simult_note=0; simult_rest=0;
  for (i in 1:nrow(df.num)){
    if (mean(df.num[i,]%in% (1:12))==1){
      simult_note=simult_note+1
    } else if (!is.na(mean(df.num[i,]))){
      if (mean(df.num[i,]==0)==1){
        simult_rest=simult_rest+1 
      }
    }
  }
  simult_note_perc=simult_note/nrow(df)
  simult_rest_perc=simult_rest/nrow(df)
  return(list(simult_note_perc,simult_rest_perc))
}

#______________________________Interval features functions ______________________________________

#For two notes, this function returns its type, sign, and mode
interval_calc=function(pair_i,just_int=T){
  note1=pair_i[1]
  note2=pair_i[2]
  dist=note1-note2
  #Get ascending vs. descending, where 1 is descending, -1 is ascending, 0 is no change 
  if (dist<0){
    sign=-1
  } else if (dist>0){
    sign=1
  } else {
    sign=0
  }
  
  #Get interval type
  dist=abs(dist)
  if (dist%%12==0){
    interval="P0"
  }
  if (dist%%12==1){
    interval="m2"
  }
  if (dist%%12==2){
    interval="M2"
  }
  if (dist%%12==3){
    interval="m3"
  }
  if (dist%%12==4){
    interval="M3"
  }
  if (dist%%12==5){ 
    interval="P4"
  }
  if (dist%%12==6){ #aka A4
    interval="d5"
  }
  if (dist%%12==7){
    interval="P5"
  }
  if (dist%%12==8){
    interval="m6"
  }
  if (dist%%12==9){
    interval="M6"
  }
  if (dist%%12==10){
    interval="m7"
  }
  if (dist%%12==11){
    interval="M7"
  }
  #Get mode of interval, where minor is -1, major is 1, diminished is -2, perfect is 2
  if (str_detect(interval,"m")){
    mode=-1
  } 
  if (str_detect(interval,"M")){
    mode=1
  } 
  if (str_detect(interval,"d")){
    mode=-2
  } 
  if (str_detect(interval,"P")){
    mode=2
  } 
  if (just_int){
    return(interval)
  } else {
    return(c(sign,dist%%12,mode)) 
  }
}

#This function computes the proportion of intervals in each category (e.g., proportion of intervals that are ascending)
intervals_perc=function(interval_list){
  interval_sign=interval_list[[1]]
  interval_dist=interval_list[[2]]
  interval_mode=interval_list[[3]]
  return(c(interval_sign))
}

#Pairwise Interval Function
#This function gets the proportion of each type of interval (where type includes distance, ascending/descending/constant, and mode)
pair_interval_func=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  trans_mvmt=transpose_octave(c_mvmt)
  pairs=NULL
  
  for (i in 1:(length(trans_mvmt)-1)){
    pair_i=list(c(trans_mvmt[i],trans_mvmt[i+1]))
    pairs=c(pairs,pair_i)
  }
  
  intervals=lapply(pairs,function(x) interval_calc(x,F))
  intervals_mat=matrix(unlist(intervals),byrow=T,nrow=length(intervals),ncol=3)
  ints=as.factor(intervals_mat[,2]); 
  levels(ints)=c("0","1","2","3","4","5","6","7","8","9","10","11")
  signs=as.factor(intervals_mat[,1]); levels(signs)=c("1","-1","0")
  modes=as.factor(intervals_mat[,3]); levels(modes)=c("-2","-1","1","2")
  signs_perc=table(signs)/sum(table(signs))
  ints_perc=table(ints)/sum(table(ints))
  modes_perc=table(modes)/sum(table(modes))
  return(c(signs_perc,ints_perc,modes_perc))
}

#Computes mean and standard deviation of intervals
summary_pair_interval=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  trans_mvmt=transpose_octave(c_mvmt)
  pairs=NULL
  
  for (i in 1:(length(trans_mvmt)-1)){
    pair_i=list(c(trans_mvmt[i],trans_mvmt[i+1]))
    pairs=c(pairs,pair_i)
  }
  
  intervals=lapply(pairs,function(x) interval_calc(x,F))
  dist=NULL
  for (j in 1:length(intervals)){
    dist=c(dist,intervals[[j]][2])
  }
  mean.ints=mean(dist)
  sd.ints=sqrt(var(dist))
  return(list(mean.ints,sd.ints))
}

mode_identifier=function(intervals,n){
  num_m3=length(which(intervals=="m3"))
  prop_m3=num_m3/(n-1)
  return(prop_m3)
}

#Calculates summary statistics for proportion of m3 intervals in each segment
mode_calcs=function(mode_mvmt,n){
  summary_mode=summary(mode_mvmt)
  min_mode=summary_mode[[1]]
  q1_mode=summary_mode[[2]]
  med_mode=summary_mode[[3]]
  mean_mode=summary_mode[[4]]
  q3_mode=summary_mode[[5]]
  max_mode=summary_mode[[6]]
  sd_mode=sqrt(var(mode_mvmt))
  
  num_0=length(which(mode_mvmt==0))
  num_.6=length(which(mode_mvmt>=.6))
  
  return(c(min_mode,q1_mode,med_mode,mean_mode,q3_mode,max_mode,sd_mode,num_0,num_.6))
}

#Finds summary statistics for proportion of m3 intervals in each segment
mode_segment_f=function(mvmt,n=12,FUN=clean_up.v,voice=4){
  c_mvmt=as.numeric(clean_up.v(mvmt,voice))
  mode_mvmt=NULL
  for (j in 1:(length(c_mvmt)-n+1)){
    segment=transpose_octave(c_mvmt[j:(j+n-1)])
    pairs=NULL
    for (k in 2:length(segment)){
      pair_k=list(c(segment[1],segment[k]))
      pairs=c(pairs,pair_k)
    }
    intervals=lapply(pairs,interval_calc)
    mode_j=mode_identifier(unlist(intervals),n)
    mode_mvmt=c(mode_mvmt,mode_j)
  }
  mode_summary=mode_calcs(mode_mvmt,n)
  return(mode_summary)
}


#____________________________________________________________________
#                             DURATION 
#____________________________________________________________________

#These functions are similar to those for pitch but must accommodate duration encoding.

#________________________Recapitulation and Exposition Functions ________________________ 

#Calculates fraction of overlap between opening segment and subsequent segments
ACC.t=function(violin1,n){
  violin1_opening=as.numeric(as.character(violin1[1:n]))
  violin1_beyond=as.numeric(violin1[-c(1:n)])
  acc=NULL
  if (length(violin1_beyond) >=n){
    for (j in 1:(length(violin1_beyond)-n+1)){
      y=violin1_beyond[j:(j+n-1)]
      acc=c(acc,mean(violin1_opening==y))
    }
  } else {acc=0}
  return(acc)
}

#Calculates maximum fraction of overlap and its percentile
calcs.t=function(violin1,acc){
  max_acc=max(acc)
  violin1_recap.pt=which(acc==max_acc)
  violin1_recap.pt=violin1_recap.pt[length(violin1_recap.pt)] #select last sequence
  percentile=violin1_recap.pt/(length(acc))
  return(list(max_acc,percentile))
}

#Calculates fraction of overlap counts at threshold vector t 
matches_found.t=function(violin1,acc,thresholds){
  matches=c()
  for (i in thresholds){
    matches=c(matches,length(which(acc >=i)))
  }
  
  return(matches)
}

#This is used for exposition and recapitulation sections to calculate fractions of overlap and summary stats
recap.t=function(mvmt,n,FUN=clean_up.v,voice=4,expo=T){
  if (expo==T){
    get_violin1=FUN(mvmt,voice,expo=T)
  } else {
    get_violin1=FUN(mvmt,voice) 
  }
  get_acc=ACC.t(get_violin1,n)
  get_calcs=calcs.t(get_violin1,get_acc)
  return(get_calcs)
}

recap_match.t=function(mvmt,n,FUN=clean_up.v,voice=4,thresholds=c(.7,.85,1),expo=T){
  if (expo==T){
    get_violin1=FUN(mvmt,voice,expo=T)
  } else {
    get_violin1=FUN(mvmt,voice) 
  }
  get_acc=ACC.t(get_violin1,n)
  get_matchnum=matches_found.t(get_violin1,get_acc,thresholds)
  return(get_matchnum)
}

#________________________Development Functions ________________________
#Gets standard deviation of each segment of duration values
get_SD.thresh_t=function(violin1,n,thresholds){
  sd=NULL
  if (length(violin1) >=n){
    for (j in 2:(length(violin1)-n+1)){
      y=violin1[j:(j+n-1)]
      sd=c(sd,sqrt(var(y)))
    }
  } else {sd=sqrt(var(violin1))}
  return(sd)
}

#Calls other SD threshold function
Dev_max_SD.thresh_t=function(mvmt,n,FUN=clean_up.v,voice=4,thresholds){
  get_violin1=as.numeric(clean_up.v(mvmt,voice))
  get__SD=get_SD.thresh_t(get_violin1,n,thresholds)
  return(get__SD)
}

#Calculates maximum standard deviation, standard deviation counts at thresholds t, and percentile of maximum SD
get_SD.t=function(violin1,n,thresholds){
  n=n_vec[n]
  sd=NULL
  if (length(violin1) >=n){
    for (j in 2:(length(violin1)-n+1)){
      y=violin1[j:(j+n-1)]
      sd=c(sd,sqrt(var(y)))
    }
  } else {sd=sqrt(var(violin1))}
  max_sd=max(sd)[1]
  violin1_dev.pt=which(sd==max_sd)
  violin1_dev.pt=which(sd==max_sd)[1] #pick first sequence 
  percentile=violin1_dev.pt/length(sd)
  matches=c()
  for (i in thresholds){
    matches=c(matches,length(which(sd >= i)))
  }
  return(list(max_sd,percentile,matches))
}


dev_func.t=function(mvmt,n,FUN=clean.up.v,voice=4,thresholds){
  get_violin1=FUN(mvmt,voice)
  get__SD=get_SD.t(get_violin1,n,thresholds)
  return(get__SD)
}

#________________________Basic Features Functions ________________________
#Standard deviation of duration
SD_time=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  SD.t=sqrt(var(c_mvmt))
  return(SD.t)
}

#Mean of duration
mean_time=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  mean.t=mean(c_mvmt)
  return(mean.t)
}

#________________________Interval Features Functions ________________________
#Gets difference between duration of two notes 
dist_interval=function(pair_i){
  note1=pair_i[[1]]
  note2=pair_i[[2]]
  dist=note1-note2
  return(dist)
}

#Computes mean and standard deviation of pairwise differences between durations of notes
pair_int_func.t=function(mvmt,v){
  c_mvmt=as.numeric(clean_up.v(mvmt,v))
  pairs=NULL
  
  for (i in 1:(length(c_mvmt)-1)){
    pair_i=list(c(c_mvmt[i],c_mvmt[i+1]))
    pairs=c(pairs,pair_i)
  }
  
  dist.t=unlist(lapply(pairs,dist_interval))
  mean.t=mean(dist.t)
  sd.t=sqrt(var(dist.t))
  
  return(c(mean.t,sd.t))
}

#_____________________________________________________________________________________________
#---------------------------                                    ---------------------------
#---------------------------          Calculate Features        ---------------------------
#---------------------------                                    ---------------------------
#_____________________________________________________________________________________________


#_____________________________________________________________________________________________
#                                Recapitulation Features 
#_____________________________________________________________________________________________

#Recapitulation - PITCH 

Recap.1=NULL; Recap_matches.1=NULL; Recap.2=NULL; Recap_matches.2=NULL; Recap.3=NULL; Recap_matches.3=NULL
Recap.4=NULL; Recap_matches.4=NULL
n_vec=c(8,10,12,14,16,18)
thresholds_vec=c(.70,.9,1) #Change duration too 
num=length(n_vec)
num_thresh=length(thresholds_vec)

for (k in name){
  Recap.1=c(Recap.1,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,1)))
  Recap_matches.1=c(Recap_matches.1,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,1,thresholds_vec)))
  Recap.2=c(Recap.2,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,2)))
  Recap_matches.2=c(Recap_matches.2,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,2,thresholds_vec)))
  Recap.3=c(Recap.3,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,3)))
  Recap_matches.3=c(Recap_matches.3,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,3,thresholds_vec)))
  Recap.4=c(Recap.4,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,4)))
  Recap_matches.4=c(Recap_matches.4,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,4,thresholds_vec)))
}

Recap1.1=matrix(unlist(Recap.1),nrow=(num*length(name)),ncol=2,byrow=T)
Recap2.1=cbind(matrix(Recap1.1[,1],ncol=num,nrow=length(name),byrow=T),matrix(Recap1.1[,2],ncol=num,nrow=length(name),byrow=T))
Recap3.1=matrix(unlist(Recap_matches.1),ncol=num*num_thresh,nrow=length(name),byrow=T)
Recap.1.out=data.frame(cbind(Recap2.1,Recap3.1))

Recap1.2=matrix(unlist(Recap.2),nrow=(num*length(name)),ncol=2,byrow=T)
Recap2.2=cbind(matrix(Recap1.2[,1],ncol=num,nrow=length(name),byrow=T),matrix(Recap1.2[,2],ncol=num,nrow=length(name),byrow=T))
Recap3.2=matrix(unlist(Recap_matches.2),ncol=num*num_thresh,nrow=length(name),byrow=T) 
Recap.2.out=data.frame(cbind(Recap2.2,Recap3.2))

Recap1.3=matrix(unlist(Recap.3),nrow=(num*length(name)),ncol=2,byrow=T)
Recap2.3=cbind(matrix(Recap1.3[,1],ncol=num,nrow=length(name),byrow=T),matrix(Recap1.3[,2],ncol=num,nrow=length(name),byrow=T))
Recap3.3=matrix(unlist(Recap_matches.3),ncol=num*num_thresh,nrow=length(name),byrow=T)
Recap.3.out=data.frame(cbind(Recap2.3,Recap3.3))

Recap1.4=matrix(unlist(Recap.4),nrow=(num*length(name)),ncol=2,byrow=T)
Recap2.4=cbind(matrix(Recap1.4[,1],ncol=num,nrow=length(name),byrow=T),matrix(Recap1.4[,2],ncol=num,nrow=length(name),byrow=T))
Recap3.4=matrix(unlist(Recap_matches.4),ncol=num*num_thresh,nrow=length(name),byrow=T)
Recap.4.out=data.frame(cbind(Recap2.4,Recap3.4))


names.recap=NULL
for (j in 1:4){
  names.acc=NULL; names.perc=NULL; names.match=NULL
  for (i in n_vec){
    names.acc=c(names.acc,paste("Recap_acc_",i,".",j,sep=""))
  }
  for (i in n_vec){
    names.perc=c(names.perc,paste("Recap_perc_",i,".",j,sep=""))
  }
  for (i in n_vec){
    for (k in thresholds_vec){
      names.match=c(names.match,paste("Recap_match_",i,"_thresh-",k, ".",j,sep="")) 
    }
  }
  names.recap=c(names.recap,names.acc,names.perc,names.match)
}

Recap.out=cbind(Recap.1.out,Recap.2.out,Recap.3.out,Recap.4.out)
names(Recap.out)=names.recap


#Recapitulation - DURATION

n_vec=c(8,10,12,14,16,18)
num=length(n_vec)
thresholds_vec=c(.70,.9,1)
num_thresh=length(thresholds_vec)

Recap_t.1=NULL; 
Recap_t_matches.1=NULL; 
Recap_t.2=NULL; Recap_t_matches.2=NULL;
Recap_t.3=NULL; Recap_t_matches.3=NULL; Recap_t.4=NULL; Recap_t_matches.4=NULL
for (k in name.t){
  Recap_t.1=c(Recap_t.1,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,1,F)))
  Recap_t_matches.1=c(Recap_t_matches.1,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,1,thresholds_vec,F)))
  Recap_t.2=c(Recap_t.2,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,2,F)))
  Recap_t_matches.2=c(Recap_t_matches.2,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,2,thresholds_vec,F)))
  Recap_t.3=c(Recap_t.3,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,3,F)))
  Recap_t_matches.3=c(Recap_t_matches.3,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,3,thresholds_vec,F)))
  Recap_t.4=c(Recap_t.4,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,4,F)))
  Recap_t_matches.4=c(Recap_t_matches.4,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,4,thresholds_vec,F)))
}


names_f=function(str1,voice,match=F,thresh=NULL){
  if (!match){
    names=NULL
    for (i in n_vec){
      names=c(names,paste("Recap_t_",str1,"_",i,".",voice,sep=""))
    } 
  } else {
    names=NULL
    for (i in n_vec){
      for (j in thresholds_vec){
        names=c(names,paste("Recap_t_",str1,"_",i,"-thresh",j,".",voice,sep="")) 
      }
    }
  }
  return(names)
}


Recap=matrix(unlist(Recap_t.1),nrow=(num*length(name)),ncol=2,byrow=T)
Recap_acc.1=data.frame(matrix(Recap[,1],ncol=num,nrow=length(name),byrow=T)); 
Recap_perc.1=matrix(Recap[,2],ncol=num,nrow=length(name),byrow=T);
Recap_match.1=matrix(unlist(Recap_t_matches.1),ncol=num*num_thresh,nrow=length(name),byrow=T); 
Recap_t.1.out=data.frame(cbind(Recap_acc.1,Recap_perc.1,Recap_match.1))
name_acc=names_f("acc",1); name_perc=names_f("perc",1); name_match=names_f("match",1,T,thresholds_vec)
names(Recap_t.1.out)=c(name_acc,name_perc,name_match)

Recap=matrix(unlist(Recap_t.2),nrow=(num*length(name)),ncol=2,byrow=T)
Recap_acc=data.frame(matrix(Recap[,1],ncol=num,nrow=length(name),byrow=T)); 
Recap_perc=matrix(Recap[,2],ncol=num,nrow=length(name),byrow=T);
Recap_match=matrix(unlist(Recap_t_matches.2),ncol=num*num_thresh,nrow=length(name),byrow=T); 
Recap_t.2.out=data.frame(cbind(Recap_acc,Recap_perc,Recap_match))
name_acc=names_f("acc",2); name_perc=names_f("perc",2); name_match=names_f("match",2,T,thresholds_vec)
names(Recap_t.2.out)=c(name_acc,name_perc,name_match)

Recap=matrix(unlist(Recap_t.3),nrow=(num*length(name)),ncol=2,byrow=T)
Recap_acc=data.frame(matrix(Recap[,1],ncol=num,nrow=length(name),byrow=T)); 
Recap_perc=matrix(Recap[,2],ncol=num,nrow=length(name),byrow=T);
Recap_match=matrix(unlist(Recap_t_matches.2),ncol=num*num_thresh,nrow=length(name),byrow=T); 
Recap_t.3.out=data.frame(cbind(Recap_acc,Recap_perc,Recap_match))
name_acc=names_f("acc",3); name_perc=names_f("perc",3); name_match=names_f("match",3,T,thresholds_vec)
names(Recap_t.3.out)=c(name_acc,name_perc,name_match)

Recap=matrix(unlist(Recap_t.4),nrow=(num*length(name)),ncol=2,byrow=T)
Recap_acc=data.frame(matrix(Recap[,1],ncol=num,nrow=length(name),byrow=T)); 
Recap_perc=matrix(Recap[,2],ncol=num,nrow=length(name),byrow=T);
Recap_match=matrix(unlist(Recap_t_matches.2),ncol=num*num_thresh,nrow=length(name),byrow=T); 
Recap_t.4.out=data.frame(cbind(Recap_acc,Recap_perc,Recap_match))
name_acc=names_f("acc",4); name_perc=names_f("perc",4); name_match=names_f("match",4,T,thresholds_vec)
names(Recap_t.4.out)=c(name_acc,name_perc,name_match)

Recap_t.out=cbind(Recap_t.1.out,Recap_t.2.out,Recap_t.3.out,Recap_t.4.out)


#_____________________________________________________________________________________________
#                                 Development Features 
#_____________________________________________________________________________________________
n_vec=c(8,10,12,14,16,18)
num=length(n_vec)

#Get all standard deviations of segments 
DEV_SD.1=NULL; DEV_SD.2=NULL; DEV_SD.3=NULL;DEV_SD.4=NULL
for (k in n_vec){
  DEV_SD.1=c(DEV_SD.1,lapply(name,function(x) Dev_max_SD.thresh(x,k,clean_up.v,1,thresholds_vec,F)))
  DEV_SD.2=c(DEV_SD.2,lapply(name,function(x) Dev_max_SD.thresh(x,k,clean_up.v,2,thresholds_vec,F)))
  DEV_SD.3=c(DEV_SD.3,lapply(name,function(x) Dev_max_SD.thresh(x,k,clean_up.v,3,thresholds_vec,F)))
  DEV_SD.4=c(DEV_SD.4,lapply(name,function(x) Dev_max_SD.thresh(x,k,clean_up.v,4,thresholds_vec,F)))
}

#Get weighted quantiles of standard deviations
end_ind=285*c(1:num)
begin_ind=end_ind-284
lengths.1=c(); sd_vec.1=c(); lengths.2=c(); sd_vec.2=c(); lengths.3=c(); sd_vec.3=c(); lengths.4=c(); sd_vec.4=c()
thresholds.1=c(); thresholds.2=c(); thresholds.3=c(); thresholds.4=c()
for (j in 1:length(n_vec)){
  indices=begin_ind[j]:end_ind[j]
  for (i in indices){
  len.1=length(DEV_SD.1[[i]])
  lengths.1=c(lengths.1,rep(1/len.1,len.1))
  sd_vec.1=c(sd_vec.1,DEV_SD.1[[i]])
  
  len.2=length(DEV_SD.2[[i]])
  lengths.2=c(lengths.2,rep(1/len.2,len.2))
  sd_vec.2=c(sd_vec.2,DEV_SD.2[[i]])
  
  len.3=length(DEV_SD.3[[i]])
  lengths.3=c(lengths.3,rep(1/len.3,len.3))
  sd_vec.3=c(sd_vec.3,DEV_SD.3[[i]])
  
  len.4=length(DEV_SD.4[[i]])
  lengths.4=c(lengths.4,rep(1/len.4,len.4))
  sd_vec.4=c(sd_vec.4,DEV_SD.4[[i]])
  
  }
  thresholds.1=c(thresholds.1,list(wtd.quantile(sd_vec.1, weights=lengths.1, probs=c(.7,.8,.9,.95))))
  thresholds.2=c(thresholds.2,list(wtd.quantile(sd_vec.2, weights=lengths.2, probs=c(.7,.8,.9,.95))))
  thresholds.3=c(thresholds.3,list(wtd.quantile(sd_vec.3, weights=lengths.3, probs=c(.7,.8,.9,.95))))
  thresholds.4=c(thresholds.4,list(wtd.quantile(sd_vec.4, weights=lengths.4, probs=c(.7,.8,.9,.95))))
}

num_thresh=length(thresholds.1)

DEV_SD.1=NULL; DEV_SD.2=NULL; DEV_SD.3=NULL;DEV_SD.4=NULL
for (k in name){
  DEV_SD.1=c(DEV_SD.1,lapply(1:num,function(x) Dev_max_SD(k,x,clean_up.v,1,thresholds.1[[x]])))
  DEV_SD.2=c(DEV_SD.2,lapply(1:num,function(x) Dev_max_SD(k,x,clean_up.v,2,thresholds.2[[x]])))
  DEV_SD.3=c(DEV_SD.3,lapply(1:num,function(x) Dev_max_SD(k,x,clean_up.v,3,thresholds.3[[x]])))
  DEV_SD.4=c(DEV_SD.4,lapply(1:num,function(x) Dev_max_SD(k,x,clean_up.v,4,thresholds.4[[x]])))
}

names_f=function(str1,voice,matches=F,thresh=NULL){
  if (!matches){
    names=NULL
    for (i in n_vec){
      names=c(names,paste("Dev_",str1,"_",i,".",voice,sep=""))
    } 
  } else {
    names=NULL
    for (i in n_vec){
      for (j in thresh){
      names=c(names,paste("Dev_",str1,"_",i, "-thresh",j, ".",voice,sep=""))
      }
    }
  }
  return(names)
}

thresh_names=function(thresholds_list,voice){
  names=c()
  for (i in 1:length(n_vec)){
    names=c(names,paste("Dev_count_",n_vec[i],"_thresh",round(thresholds_list[[i]],3),".",voice,sep="")) 
  }
  return(names)
}

Dev_SD=matrix(unlist(DEV_SD.1),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_matches=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD.1.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_matches))
name_sd=names_f("SD",1); name_perc=names_f("perc",1); name_match=thresh_names(thresholds.1,1)
names(Dev_SD.1.out)=c(name_sd,name_perc,name_match)

Dev_SD=matrix(unlist(DEV_SD.2),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_matches=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD.2.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_matches))
name_sd=names_f("SD",2); name_perc=names_f("perc",2); name_match=thresh_names(thresholds.2,2)
names(Dev_SD.2.out)=c(name_sd,name_perc,name_match)

Dev_SD=matrix(unlist(DEV_SD.3),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_matches=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD.3.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_matches))
name_sd=names_f("SD",3); name_perc=names_f("perc",3); name_match=thresh_names(thresholds.3,3)
names(Dev_SD.3.out)=c(name_sd,name_perc,name_match)

Dev_SD=matrix(unlist(DEV_SD.4),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_matches=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD.4.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_matches))
name_sd=names_f("SD",4); name_perc=names_f("perc",4); name_match=thresh_names(thresholds.4,4)
names(Dev_SD.4.out)=c(name_sd,name_perc,name_match)

Dev_max_SD.out=cbind(Dev_SD.1.out,Dev_SD.2.out,Dev_SD.3.out,Dev_SD.4.out)


#__________________________________________ DURATION _________________________________________
n_vec=c(8,10,12,14,16,18)
num=length(n_vec)

#Get standard deviations to calculate appopriate weighted quantiles
DEV_SD..1=NULL; DEV_SD..2=NULL; DEV_SD..3=NULL;DEV_SD..4=NULL
for (k in n_vec){
  DEV_SD..1=c(DEV_SD..1,lapply(name.t,function(x) Dev_max_SD.thresh_t(x,k,clean_up.v,1,thresholds_vec)))
  DEV_SD..2=c(DEV_SD..2,lapply(name.t,function(x) Dev_max_SD.thresh_t(x,k,clean_up.v,2,thresholds_vec)))
  DEV_SD..3=c(DEV_SD..3,lapply(name.t,function(x) Dev_max_SD.thresh_t(x,k,clean_up.v,3,thresholds_vec)))
  DEV_SD..4=c(DEV_SD..4,lapply(name.t,function(x) Dev_max_SD.thresh_t(x,k,clean_up.v,4,thresholds_vec)))
}

Q3.1=c(); max.1=c(); Q3.2=c(); max.2=c(); Q3.3=c(); max.3=c(); Q3.4=c(); max.4=c()
for (i in 1:285){
  Q3.1=c(Q3.1,DEV_SD..1[[i]][5])
  max.1=c(max.1,DEV_SD..1[[i]][6])
  Q3.2=c(Q3.2,DEV_SD..2[[i]][5])
  max.2=c(max.2,DEV_SD..2[[i]][6])
  Q3.3=c(Q3.3,DEV_SD..3[[i]][5])
  max.3=c(max.3,DEV_SD..3[[i]][6])
  Q3.4=c(Q3.4,DEV_SD..4[[i]][5])
  max.4=c(max.4,DEV_SD..4[[i]][6])
}

end_ind=285*c(1:num)
begin_ind=end_ind-284
lengths..1=c(); sd_vec..1=c(); lengths..2=c(); sd_vec..2=c(); lengths..3=c(); sd_vec..3=c(); lengths..4=c(); sd_vec..4=c()
thresholds..1=c(); thresholds..2=c(); thresholds..3=c(); thresholds..4=c()
for (j in 1:length(n_vec)){
  indices=begin_ind[j]:end_ind[j]
  for (i in indices){
    len..1=length(DEV_SD..1[[i]])
    lengths..1=c(lengths..1,rep(1/len..1,len..1))
    sd_vec..1=c(sd_vec..1,DEV_SD..1[[i]])
    
    len..2=length(DEV_SD..2[[i]])
    lengths..2=c(lengths..2,rep(1/len..2,len..2))
    sd_vec..2=c(sd_vec..2,DEV_SD..2[[i]])
    
    len..3=length(DEV_SD..3[[i]])
    lengths..3=c(lengths..3,rep(1/len..3,len..3))
    sd_vec..3=c(sd_vec..3,DEV_SD..3[[i]])
    
    len..4=length(DEV_SD..4[[i]])
    lengths..4=c(lengths..4,rep(1/len..4,len..4))
    sd_vec..4=c(sd_vec..4,DEV_SD..4[[i]])
    
  }
  thresholds..1=c(thresholds..1,list(wtd.quantile(sd_vec..1, weights=lengths..1, probs=c(.7,.8,.9,.95))))
  thresholds..2=c(thresholds..2,list(wtd.quantile(sd_vec..2, weights=lengths..2, probs=c(.7,.8,.9,.95))))
  thresholds..3=c(thresholds..3,list(wtd.quantile(sd_vec..3, weights=lengths..3, probs=c(.7,.8,.9,.95))))
  thresholds..4=c(thresholds..4,list(wtd.quantile(sd_vec..4, weights=lengths..4, probs=c(.7,.8,.9,.95))))
}

num_thresh=length(thresholds.1)


DEV_SD_t.1=NULL;DEV_SD_t.2=NULL;DEV_SD_t.3=NULL;DEV_SD_t.4=NULL
for (k in name.t){
  DEV_SD_t.1=c(DEV_SD_t.1,lapply(1:num,function(x) dev_func.t(k,x,clean_up.v,1,thresholds..1[[x]])))
  DEV_SD_t.2=c(DEV_SD_t.2,lapply(1:num,function(x) dev_func.t(k,x,clean_up.v,2,thresholds..2[[x]])))
  DEV_SD_t.3=c(DEV_SD_t.3,lapply(1:num,function(x) dev_func.t(k,x,clean_up.v,3,thresholds..3[[x]])))
  DEV_SD_t.4=c(DEV_SD_t.4,lapply(1:num,function(x) dev_func.t(k,x,clean_up.v,4,thresholds..4[[x]])))
}

names_f=function(str1,voice,match=F,thresh=NULL){
  if (!match){
    names=NULL
    for (i in n_vec){
      names=c(names,paste("Dev_",str1,"_t_",i,".",voice,sep=""))
    } 
  } else {
    names=NULL
    for (i in n_vec){
      for (j in thresh){
      names=c(names,paste("Dev_",str1,"_t_",i,"-thresh",j,".",voice,sep=""))
      }
    }
  }
  return(names)
}

thresh_names=function(thresholds_list,voice){
  names=c()
  for (i in 1:length(n_vec)){
    names=c(names,paste("Dev_count_t_",n_vec[i],"_thresh",round(thresholds_list[[i]],3),".",voice,sep="")) 
  }
  return(names)
}

Dev_SD=matrix(unlist(DEV_SD_t.1),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_match=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD_t.1.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_match))
sd_name=names_f("SD",1); perc_name=names_f("perc",1); match_name=thresh_names(thresholds..1,1)
names(Dev_SD_t.1.out)=c(sd_name,perc_name,match_name)

Dev_SD=matrix(unlist(DEV_SD_t.2),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_match=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD_t.2.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_match))
sd_name=names_f("SD",2); perc_name=names_f("perc",2); match_name=thresh_names(thresholds..2,2)
names(Dev_SD_t.2.out)=c(sd_name,perc_name,match_name)

Dev_SD=matrix(unlist(DEV_SD_t.3),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_match=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD_t.3.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_match))
sd_name=names_f("SD",3); perc_name=names_f("perc",3); match_name=thresh_names(thresholds..3,3)
names(Dev_SD_t.3.out)=c(sd_name,perc_name,match_name)

Dev_SD=matrix(unlist(DEV_SD_t.4),nrow=(num*length(name)),ncol=6,byrow=T)
Dev_max_SD=matrix(Dev_SD[,1],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_perc=matrix(Dev_SD[,2],ncol=num,nrow=length(name),byrow=T)
Dev_max_SD_match=cbind(matrix(Dev_SD[,3],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,4],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,5],ncol=num,nrow=length(name),byrow=T),matrix(Dev_SD[,6],ncol=num,nrow=length(name),byrow=T))
Dev_SD_t.4.out=data.frame(cbind(Dev_max_SD,Dev_max_SD_perc,Dev_max_SD_match))
sd_name=names_f("SD",4); perc_name=names_f("perc",4); match_name=thresh_names(thresholds..4,4)
names(Dev_SD_t.4.out)=c(sd_name,perc_name,match_name)

Dev_max_SD_t.out=data.frame(cbind(Dev_SD_t.1.out,Dev_SD_t.2.out,Dev_SD_t.3.out,Dev_SD_t.4.out))


#_____________________________________________________________________________________________
#                           Basic Features
#_____________________________________________________________________________________________

# Standard deviation of pitch
SD_pitch.1=lapply(name,function(x) SD_pitch(x,1))
SD_pitch.2=lapply(name,function(x) SD_pitch(x,2))
SD_pitch.3=lapply(name,function(x) SD_pitch(x,3))
SD_pitch.4=lapply(name,function(x) SD_pitch(x,4))

SD_pitch=cbind(SD_pitch.1,SD_pitch.2,SD_pitch.3,SD_pitch.4)
names.SD_pitch=c("SD_pitch_1","SD_pitch_2","SD_pitch_3","SD_pitch_4")
names(SD_pitch)=names.SD_pitch

#Mean pitch
mean_pitch.1=lapply(name,function(x) mean_pitch(x,1))
mean_pitch.2=lapply(name,function(x) mean_pitch(x,2))
mean_pitch.3=lapply(name,function(x) mean_pitch(x,3))
mean_pitch.4=lapply(name,function(x) mean_pitch(x,4))

mean_pitch=data.frame(cbind(unlist(mean_pitch.1),unlist(mean_pitch.2),unlist(mean_pitch.3),unlist(mean_pitch.4)))
names.mean_pitch=c("mean_pitch_1","mean_pitch_2","mean_pitch_3","mean_pitch_4")
names(mean_pitch)=names.mean_pitch

#Number of notes
count_pitch.1=lapply(name,function(x) count_notes(x,1))
count_pitch.2=lapply(name,function(x) count_notes(x,2))
count_pitch.3=lapply(name,function(x) count_notes(x,3))
count_pitch.4=lapply(name,function(x) count_notes(x,4))

count_pitch=data.frame(cbind(unlist(count_pitch.1),unlist(count_pitch.2),unlist(count_pitch.3),unlist(count_pitch.4)))
names.SD_pitch=c("count_pitch_1","count_pitch_2","count_pitch_3","count_pitch_4")
names(count_pitch)=names.SD_pitch

#Proportion of simultaneous notes and rests
simult_note=lapply(name,simult_note_f)

simult_notes=NULL
simult_rests=NULL
for (i in 1:length(simult_note)){
  simult_notes=c(simult_notes,simult_note[[i]][[1]])
  simult_rests=c(simult_rests,simult_note[[i]][[2]])
}

simult.out=data.frame(cbind(simult_notes,simult_rests))
colnames(simult.out)=c("simult_note_perc","simult_rest_perc")

#Standard deviation of duration
SD_time.1=lapply(name.t,function(x) SD_time(x,1))
SD_time.2=lapply(name.t,function(x) SD_time(x,2))
SD_time.3=lapply(name.t,function(x) SD_time(x,3))
SD_time.4=lapply(name.t,function(x) SD_time(x,4))

SD_time=data.frame(cbind(unlist(SD_time.1),unlist(SD_time.2),unlist(SD_time.3),unlist(SD_time.4)))
names.sd_time=c("SD_time_1","SD_time_2","SD_time_3","SD_time_4")
names(SD_time)=names.sd_time

#Mean duration
mean_time.1=lapply(name.t,function(x) mean_time(x,1))
mean_time.2=lapply(name.t,function(x) mean_time(x,2))
mean_time.3=lapply(name.t,function(x) mean_time(x,3))
mean_time.4=lapply(name.t,function(x) mean_time(x,4))

mean_time=data.frame(cbind(unlist(mean_time.1),unlist(mean_time.2),unlist(mean_time.3),unlist(mean_time.4)))
names.mean_time=c("mean_time_1","mean_time_2","mean_time_3","mean_time_4")

names(mean_time)=names.mean_time


#_____________________________________________________________________________________________
#                                 Interval Information 
#_____________________________________________________________________________________________

#___________________ Pairwise Proportions of Each Interval Type (PITCH) ______________________________

pair_int.1=lapply(name.o,function(x) pair_interval_func(x,1))
pair_int.out.1=matrix(unlist(pair_int.1),nrow=length(name),ncol=19,byrow=T)
pair_int.2=lapply(name.o,function(x) pair_interval_func(x,2))
pair_int.out.2=matrix(unlist(pair_int.2),nrow=length(name),ncol=19,byrow=T)
pair_int.3=lapply(name.o,function(x) pair_interval_func(x,3))
pair_int.out.3=matrix(unlist(pair_int.3),nrow=length(name),ncol=19,byrow=T)
pair_int.4=lapply(name.o,function(x) pair_interval_func(x,4))
pair_int.out.4=matrix(unlist(pair_int.4),nrow=length(name),ncol=19,byrow=T)

pair_int.out=cbind(pair_int.out.1,pair_int.out.2,pair_int.out.3,pair_int.out.4)

names.sign=c("Pair_int_desc_perc","Pair_int_asc_perc","Pair_int_rep_perc")
names.mode=c("Pair_int_dim_perc","Pair_int_minor_perc","Pair_int_major_perc","Pair_int_perf_perc")
names.int=NULL
for (i in 0:11){
  names.int=c(names.int,paste("Pair_int_dist_perc_",i,sep=""))
}
names.pair_int=c(names.sign,names.mode,names.int)
names..sign=NULL; names..mode=NULL; names..int=NULL
for (i in 1:4){
  for (j in 1:length(names.sign)){
    names..sign=c(names..sign,paste(names.sign[j],".",i,sep=""))
  }
  for (j in 1:length(names.mode)){
    names..mode=c(names..mode,paste(names.mode[j],".",i,sep=""))
  }
  for (j in 1:length(names.int)){
    names..int=c(names..int,paste(names.int[j],".",i,sep=""))
  }
}

names.pair_int=c(names..sign[1:3],names..mode[1:4],names..int[1:12],
                 names..sign[4:6],names..mode[5:8],names..int[13:24],
                 names..sign[7:9],names..mode[9:12],names..int[25:36],
                 names..sign[10:12],names..mode[13:16],names..int[37:48])
pair_int.out=data.frame(pair_int.out)
names(pair_int.out)=names.pair_int

#___________________ Mean and Standard Deviation of Pairwise Differences (in DURATION) ______________________________

pair_int_t.1=lapply(name.t,function(x) pair_int_func.t(x,1))
pair_int_t.2=lapply(name.t,function(x) pair_int_func.t(x,2))
pair_int_t.3=lapply(name.t,function(x) pair_int_func.t(x,3))
pair_int_t.4=lapply(name.t,function(x) pair_int_func.t(x,4))

pair_int_t.out=data.frame(cbind(matrix(unlist(pair_int_t.1),byrow=T,ncol=2,nrow=length(name)),
                                matrix(unlist(pair_int_t.2),byrow=T,ncol=2,nrow=length(name)),
                                matrix(unlist(pair_int_t.3),byrow=T,ncol=2,nrow=length(name)),
                                matrix(unlist(pair_int_t.4),byrow=T,ncol=2,nrow=length(name))))

names.pair_int_t=NULL
for (i in 1:4){
  names.pair_int_t=c(names.pair_int_t,c(paste("pair_int_t_m.",i,sep=""),paste("pair_int_t_sd.",i,sep="")))
}

names(pair_int_t.out)=names.pair_int_t

#_____________________________ Mean and SD of interval distance within each voice (for PITCH) ________________________

summary_pair_int.1=lapply(name.o,function(x) summary_pair_interval(x,1))
summary_pair_int.2=lapply(name.o,function(x) summary_pair_interval(x,2))
summary_pair_int.3=lapply(name.o,function(x) summary_pair_interval(x,3))
summary_pair_int.4=lapply(name.o,function(x) summary_pair_interval(x,4))

summary_pair_int.out=data.frame(cbind(matrix(unlist(summary_pair_int.1),byrow=T,nrow=length(name),ncol=2),
                                      matrix(unlist(summary_pair_int.2),byrow=T,nrow=length(name),ncol=2),
                                      matrix(unlist(summary_pair_int.3),byrow=T,nrow=length(name),ncol=2),
                                      matrix(unlist(summary_pair_int.4),byrow=T,nrow=length(name),ncol=2)))

names.mean=NULL
for (i in 1:4){
  names.mean=c(names.mean,c(paste("pair_int_mean.",i,sep=""),paste("pair_int_sd.",i,sep="")))
  
}
names.summary_pair_int=names.mean

names(summary_pair_int.out)=names.summary_pair_int

#______________________________ Voicepair Difference of Proportion for Each Interval Type (PITCH) ________________

v1=8:19
v2=27:38
v3=46:57
v4=65:76
voices=list(v1,v2,v3,v4)
colnames(pair_int.out)=names.pair_int
pair=combn(1:4,2)
dist=NULL
for (i in 1:ncol(pair)){
  voice_1=voices[[pair[1,i]]]
  voice_2=voices[[pair[2,i]]]
  for (j in 1:length(voice_1)){
    index_1=voice_1[j]
    index_2=voice_2[j]
    dist_j=as.numeric(pair_int.out[,index_1])-as.numeric(pair_int.out[,index_2])
    dist=c(dist,dist_j)
  }
}
voicepair_int_dist.out=data.frame(matrix(dist,byrow=T,nrow=length(name),ncol=ncol(pair)*12))
voicepair_int_dist.out2=data.frame(matrix(dist,byrow=F,nrow=length(name),ncol=ncol(pair)))


names.voicepair_int_dist=NULL
for (i in 1:ncol(pair)){
  for (j in 0:11){
    index_1=pair[1,i]
    index_2=pair[2,i]
    names.voicepair_int_dist=c(names.voicepair_int_dist,paste("voicepair_int_dist_",j,"_",index_1,"-",index_2,sep=""))
  }
}  


names(voicepair_int_dist.out)=names.voicepair_int_dist

#_________________________ Voicepair Differences of Interval Distance Means and SDs (for PITCH) __________________________

mean_index=c(1,3,5,7)
sd_index=c(2,4,6,8)

pair=combn(1:4,2)
dist.m=NULL; dist.sd=NULL
for (i in 1:ncol(pair)){
  voice_1.m=mean_index[[pair[1,i]]]
  voice_2.m=mean_index[[pair[2,i]]]
  voice_1.sd=sd_index[[pair[1,i]]]
  voice_2.sd=sd_index[[pair[2,i]]]
  dist.m=c(dist.m,(summary_pair_int.out[,voice_1.m]-summary_pair_int.out[,voice_2.m]))
  dist.sd=c(dist.sd,(summary_pair_int.out[,voice_1.sd]-summary_pair_int.out[,voice_2.sd]))
}
dist.m.mat=matrix(dist.m,nrow=length(name),ncol=ncol(pair))
dist.sd.mat=matrix(dist.sd,nrow=length(name),ncol=ncol(pair))

pairwise_voice_int.out=data.frame(cbind(dist.m.mat,dist.sd.mat))

names.m=NULL; names.sd=NULL
for (i in 1:ncol(pair)){
  index_1=pair[1,i]
  index_2=pair[2,i]
  names.m=c(names.m,paste("Pairwise_voice_int_mean.",index_1,"-",index_2,sep=""))
  names.sd=c(names.sd,paste("Pairwise_voice_int_sd.",index_1,"-",index_2,sep=""))
  
}

names.pairwise_voice_int=c(names.m,names.sd)

names(pairwise_voice_int.out)=names.pairwise_voice_int



#___________________ Summary stats of proportion of minor third intervals (PITCH) ______________________________

n_vec=c(8,10,12,14,16,18)
num=length(n_vec)

Prop_m3.1=NULL; Prop_m3.2=NULL; Prop_m3.3=NULL; Prop_m3.4=NULL
for (k in name.o){
  Prop_m3.1=c(Prop_m3.1,lapply(n_vec,function(x) mode_segment_f(k,x,clean_up.v,1)))
  Prop_m3.2=c(Prop_m3.2,lapply(n_vec,function(x) mode_segment_f(k,x,clean_up.v,2)))
  Prop_m3.3=c(Prop_m3.3,lapply(n_vec,function(x) mode_segment_f(k,x,clean_up.v,3)))
  Prop_m3.4=c(Prop_m3.4,lapply(n_vec,function(x) mode_segment_f(k,x,clean_up.v,4)))
}

Prop_m3.mat.1=matrix(unlist(Prop_m3.1),nrow=(num*length(name)),ncol=9,byrow=T)
Prop_m3.mat.2=matrix(unlist(Prop_m3.2),nrow=(num*length(name)),ncol=9,byrow=T)
Prop_m3.mat.3=matrix(unlist(Prop_m3.3),nrow=(num*length(name)),ncol=9,byrow=T)
Prop_m3.mat.4=matrix(unlist(Prop_m3.4),nrow=(num*length(name)),ncol=9,byrow=T)

#Omit min and max proportion of m3 intervals, since the variability is too low.
string=c("med","mean","q3","max","sd","num_0","num_.6")
Prop_m3.1.out=NULL; Prop_m3.2.out=NULL; Prop_m3.3.out=NULL; Prop_m3.4.out=NULL; 
name.1=NULL;name.2=NULL;name.3=NULL;name.4=NULL
for (j in 3:9){
  if (j!=3){
    v1=matrix(Prop_m3.mat.1[,j],ncol=num,nrow=length(name),byrow=T)
    Prop_m3.1.out=cbind(Prop_m3.1.out,v1)
    
    v2=matrix(Prop_m3.mat.2[,j],ncol=num,nrow=length(name),byrow=T)
    Prop_m3.2.out=cbind(Prop_m3.2.out,v2)
    
    v3=matrix(Prop_m3.mat.3[,j],ncol=num,nrow=length(name),byrow=T)
    Prop_m3.3.out=cbind(Prop_m3.3.out,v3)
    
    v4=matrix(Prop_m3.mat.4[,j],ncol=num,nrow=length(name),byrow=T)
    Prop_m3.4.out=cbind(Prop_m3.4.out,v4)
    
    for (i in n_vec){
      name.1=c(name.1,paste("Prop_m3_",string[j-2],"_",i,".",1,sep=""))
      name.2=c(name.2,paste("Prop_m3_",string[j-2],"_",i,".",2,sep=""))
      name.3=c(name.3,paste("Prop_m3_",string[j-2],"_",i,".",3,sep=""))
      name.4=c(name.4,paste("Prop_m3_",string[j-2],"_",i,".",4,sep=""))
    }
  } else {
    v3=matrix(Prop_m3.mat.3[-c(1:1140),j],ncol=2,nrow=length(name),byrow=T) #only want segment lengths 16, 18
    Prop_m3.3.out=cbind(Prop_m3.3.out,v3)
    
    
    v4=matrix(Prop_m3.mat.4[-c(1:855),j],ncol=3,nrow=length(name),byrow=T)
    Prop_m3.4.out=cbind(Prop_m3.4.out,v4) #only want segment length 14, 16, 18
    
    for (i in c(16,18)){
      name.3=c(name.3,paste("Prop_m3_",string[1],"_",i,".",3,sep=""))
    }
    
    for (i in c(14,16,18)){
      name.4=c(name.4,paste("Prop_m3_",string[1],"_",i,".",4,sep=""))
    }
    
  }
  
}

Prop_m3.1.out=data.frame(Prop_m3.1.out); names(Prop_m3.1.out)=name.1
Prop_m3.2.out=data.frame(Prop_m3.2.out); names(Prop_m3.2.out)=name.2
Prop_m3.3.out=data.frame(Prop_m3.3.out); names(Prop_m3.3.out)=name.3
Prop_m3.4.out=data.frame(Prop_m3.4.out); names(Prop_m3.4.out)=name.4

Prop_m3.out=cbind(Prop_m3.1.out,Prop_m3.2.out,Prop_m3.3.out,Prop_m3.4.out)


#________________________________________________________________________________________________________________________
#                            Exposition Features 
#________________________________________________________________________________________________________________________
n_vec=c(8,10,12,14,16,18) 
num=length(n_vec)


thresholds_vec=c(.70,.9,1)
num_thresh=length(thresholds_vec)

Expo.1=NULL; Expo_matches.1=NULL;Expo.2=NULL; Expo_matches.2=NULL;Expo.3=NULL; Expo_matches.3=NULL;Expo.4=NULL; Expo_matches.4=NULL

for (k in name){
  Expo.1=c(Expo.1,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,1,T)))
  Expo_matches.1=c(Expo_matches.1,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,1,thresholds=thresholds_vec,exact=F)))
  Expo.2=c(Expo.2,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,2,T)))
  Expo_matches.2=c(Expo_matches.2,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,2,thresholds=thresholds_vec,exact=F)))
  Expo.3=c(Expo.3,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,3,T)))
  Expo_matches.3=c(Expo_matches.3,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,3,thresholds=thresholds_vec,exact=F)))
  Expo.4=c(Expo.4,lapply(n_vec,function(x) key_transpose(k,x,clean_up.v,4,T)))
  Expo_matches.4=c(Expo_matches.4,lapply(n_vec,function(x) key_transpose2(k,x,clean_up.v,4,thresholds=thresholds_vec,exact=F)))
}


names_f=function(str1,voice,match=F,thresh=NULL){
  names=NULL
  if (!match){
    for (i in n_vec){
      names=c(names,paste("Expo_",str1,"_",i,".",voice,sep=""))
    }
  } else {
    for (i in n_vec){
      for (j in thresholds_vec){
        names=c(names,paste("Expo_",str1,"_",i,"-thresh",j,".",voice,sep="")) 
      }
    }
  }
  return(names)
}

Expo=matrix(unlist(Expo.1),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches.1),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo.1.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",1); name_perc=names_f("perc",1); name_match=names_f("count",1,T,thresholds_vec)
names(Expo.1.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo.2),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches.2),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo.2.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",2); name_perc=names_f("perc",2); name_match=names_f("count",2,T,thresholds_vec)
names(Expo.2.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo.3),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches.3),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo.3.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",3); name_perc=names_f("perc",3); name_match=names_f("count",3,T,thresholds_vec)
names(Expo.3.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo.4),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches.4),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo.4.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",4); name_perc=names_f("perc",4); name_match=names_f("count",4,T,thresholds_vec)
names(Expo.4.out)=c(name_acc,name_perc,name_match)

Expo.out=cbind(Expo.1.out,Expo.2.out,Expo.3.out,Expo.4.out)


# DURATION
n_vec=c(8,10,12,14,16,18) 
num=length(n_vec)

thresholds_vec=c(.7,.9,1)
num_thresh=length(thresholds_vec)

Expo_t.1=NULL; Expo_matches_t.1=NULL; Expo_t.2=NULL; Expo_matches_t.2=NULL; Expo_t.3=NULL; Expo_matches_t.3=NULL; Expo_t.4=NULL; Expo_matches_t.4=NULL

for (k in name.t){
  Expo_t.1=c(Expo_t.1,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,1,T)))
  Expo_matches_t.1=c(Expo_matches_t.1,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,1,thresholds=thresholds_vec,T)))
  Expo_t.2=c(Expo_t.2,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,2,T)))
  Expo_matches_t.2=c(Expo_matches_t.2,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,2,thresholds=thresholds_vec,T)))
  Expo_t.3=c(Expo_t.3,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,3,T)))
  Expo_matches_t.3=c(Expo_matches_t.3,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,3,thresholds=thresholds_vec,T)))
  Expo_t.4=c(Expo_t.4,lapply(n_vec,function(x) recap.t(k,x,clean_up.v,4,T)))
  Expo_matches_t.4=c(Expo_matches_t.4,lapply(n_vec,function(x) recap_match.t(k,x,clean_up.v,4,thresholds=thresholds_vec,T)))
}


names_f=function(str1,voice,match=F,thresh=NULL){
  names=NULL
  if (!match){
    for (i in n_vec){
      names=c(names,paste("Expo_t_",str1,"_",i,".",voice,sep=""))
    }
  } else {
    for (i in n_vec){
      for (j in thresholds_vec){
        names=c(names,paste("Expo_t_",str1,"_",i,"-thresh",j,".",voice,sep="")) 
      }
    }
  }
  return(names)
}

Expo=matrix(unlist(Expo_t.1),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches_t.1),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo_t.1.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",1); name_perc=names_f("perc",1); name_match=names_f("count",1,T,thresholds_vec)
names(Expo_t.1.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo_t.2),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches_t.2),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo_t.2.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",2); name_perc=names_f("perc",2); name_match=names_f("count",2,T,thresholds_vec)
names(Expo_t.2.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo_t.3),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches_t.3),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo_t.3.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",3); name_perc=names_f("perc",3); name_match=names_f("count",3,T,thresholds_vec)
names(Expo_t.3.out)=c(name_acc,name_perc,name_match)

Expo=matrix(unlist(Expo_t.4),nrow=(num*length(name)),ncol=2,byrow=T)
Expo_acc=matrix(Expo[,1],ncol=num,nrow=length(name),byrow=T)
Expo_perc=matrix(Expo[,2],ncol=num,nrow=length(name),byrow=T)
Expo_match=matrix(unlist(Expo_matches_t.4),ncol=num*num_thresh,nrow=length(name),byrow=T)
Expo_t.4.out=data.frame(cbind(Expo_acc,Expo_perc,Expo_match))
name_acc=names_f("acc",4); name_perc=names_f("perc",4); name_match=names_f("count",4,T,thresholds_vec)
names(Expo_t.4.out)=c(name_acc,name_perc,name_match)

Expo_t.out=cbind(Expo_t.1.out,Expo_t.2.out,Expo_t.3.out,Expo_t.4.out)



#_____________________________________________________________________________________________
#---------------------------                                    ---------------------------
#---------------------------          Create Feature Dataframe       ---------------------------
#---------------------------                                    ---------------------------
#_____________________________________________________________________________________________

Base.df=data.frame(cbind(name,c(rep(0,82),rep(1,203))))
names.df=c("name","M0")
colnames(Base.df)=names.df

Feature.df=data.frame(cbind(Base.df,Recap.out,Recap_t.out,Dev_max_SD.out,Dev_max_SD_t.out,SD_pitch,
                            Prop_m3.out,count_pitch,mean_time,mean_pitch,SD_time,voicepair_int_dist.out,summary_pair_int.out,pairwise_voice_int.out,
                            pair_int_t.out,pair_int.out,simult.out,Expo.out,Expo_t.out))

#Unlist any features that are lists
for (j in 1:ncol(Feature.df)){
  if (class(Feature.df[,j])=="list"){
    Feature.df[,j]=unlist(Feature.df[,j])
  }
  
}

filename=paste0(fpath,"Feature.csv")
write.csv(Feature.df,file=filename,row.names=F)
