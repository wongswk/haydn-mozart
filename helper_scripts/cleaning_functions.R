# This script consists of functions that will be called by the compute_music_features.R script. 

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
#                               PITCH 
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

#Calculates maximum fraction of overlap and its associated percentile/location
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
#, and the percentile/location of maximum standard deviation
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

#Calculates maximum fraction of overlap and its percentile/location
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

#Calculates maximum standard deviation, standard deviation counts at thresholds t, and percentile/location of maximum SD
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