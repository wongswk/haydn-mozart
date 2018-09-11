#Set the path where your kern files are saved
fpath="C:\\Users\\Katie\\Documents\\RFiles\\String_Quartet_Classification\\"

#Load necessary libraries 
library(stringr)
#library(data.table)

#Load in necessary data
files=list.files(fpath,pattern=".krn")

#Remove scores with encoding errors
bad_scores=c(119,284,85,114,155,170,271)
filenum=c(1:length(files))[-bad_scores]

#-------------------------------------------------------------------------------------------#
#Define functions to be used later

#Brute force translation of kern pitches to numeric values on 1-12 scale (and rests=0)
translate = function(vec){
  #First change notes with accidentals 
  vec=gsub(pattern="d\\-",replacement="2",vec); vec=gsub(pattern="c#",replacement="2",vec);
  vec=gsub(pattern="e\\-",replacement="4",vec); vec=gsub(pattern="d#",replacement="4",vec); 
  vec=gsub(pattern="e#",replacement="6",vec)
  vec=gsub(pattern="f\\-",replacement="5",vec);
  vec=gsub(pattern="g\\-",replacement="7",vec); vec=gsub(pattern="f#",replacement="7",vec); 
  vec=gsub(pattern="a\\-",replacement="9",vec); vec=gsub(pattern="g#",replacement="9",vec);
  vec=gsub(pattern="b\\-",replacement="11",vec); vec=gsub(pattern="a#",replacement="11",vec); 
  vec=gsub(pattern="b#",replacement="1",vec)
  vec=gsub(pattern="c\\-",replacement="12",vec)
  
  #Now change remaining notes (those without accidentals)
  vec=gsub(pattern="c",replacement="1",vec)
  vec=gsub(pattern="d",replacement="3",vec)
  vec=gsub(pattern="e",replacement="5",vec)
  vec=gsub(pattern="f",replacement="6",vec); 
  vec=gsub(pattern="g",replacement="8",vec);
  vec=gsub(pattern="a",replacement="10",vec)
  vec=gsub(pattern="b",replacement="12",vec)
  vec=gsub(pattern="r",replacement="0",vec)
  vec=gsub(pattern="n",replacement="",vec)
  
  return(vec)
}

#Brute force translation of kern pitches to numeric values on full octave scale (and rests=0)
translate_octave=function(vec){
  for (i in 6:1){
    
    Dflat=paste(paste(rep("D",i),collapse=""),"-",sep="",collapse=""); Dflat_r=as.character((6-i)*12+2)
    Csharp=paste(paste(rep("C",i),collapse=""),"#",sep="",collapse=""); Csharp_r=as.character((6-i)*12+2)
    Eflat=paste(paste(rep("E",i),collapse=""),"\\-",sep="",collapse=""); Eflat_r=as.character((6-i)*12+4)
    Dsharp=paste(paste(rep("D",i),collapse=""),"#",sep="",collapse=""); Dsharp_r=as.character((6-i)*12+4)
    Fflat=paste(paste(rep("F",i),collapse=""),"\\-",sep="",collapse=""); Fflat_r=as.character((6-i)*12+5)
    Esharp=paste(paste(rep("E",i),collapse=""),"#",sep="",collapse=""); Esharp_r=as.character((6-i)*12+6)
    Gflat=paste(paste(rep("G",i),collapse=""),"\\-",sep="",collapse=""); Gflat_r=as.character((6-i)*12+7)
    Fsharp=paste(paste(rep("F",i),collapse=""),"#",sep="",collapse=""); Fsharp_r=as.character((6-i)*12+7)
    Aflat=paste(paste(rep("A",i),collapse=""),"\\-",sep="",collapse=""); Aflat_r=as.character((6-i)*12+9)
    Gsharp=paste(paste(rep("G",i),collapse=""),"#",sep="",collapse=""); Gsharp_r=as.character((6-i)*12+9)
    Bflat=paste(paste(rep("B",i),collapse=""),"\\-",sep="",collapse=""); Bflat_r=as.character((6-i)*12+11)
    Asharp=paste(paste(rep("A",i),collapse=""),"#",sep="",collapse=""); Asharp_r=as.character((6-i)*12+11)
    Cflat=paste(paste(rep("C",i),collapse=""),"\\-",sep="",collapse=""); Cflat_r=as.character((6-i)*12+1)
    Bsharp=paste(paste(rep("B",i),collapse=""),"#",sep="",collapse=""); Bsharp_r=as.character((6-i)*12+12) 
   
    C=paste(rep("C",i),sep="",collapse=""); C_r=as.character((6-i)*12+1)
    D=paste(rep("D",i),sep="",collapse=""); D_r=as.character((6-i)*12+3)
    E=paste(rep("E",i),sep="",collapse=""); E_r=as.character((6-i)*12+5)
    F_=paste(rep("F",i),sep="",collapse=""); F_r=as.character((6-i)*12+6)
    G=paste(rep("G",i),sep="",collapse=""); G_r=as.character((6-i)*12+8)
    A=paste(rep("A",i),sep="",collapse=""); A_r=as.character((6-i)*12+10)
    B=paste(rep("B",i),sep="",collapse=""); B_r=as.character((6-i)*12+12)
    
    vec=gsub(pattern=Dflat,replacement=Dflat_r,vec); vec=gsub(pattern=Csharp,replacement=Csharp_r,vec);
    vec=gsub(pattern=Eflat,replacement=Eflat_r,vec); vec=gsub(pattern=Dsharp,replacement=Dsharp_r,vec); 
    vec=gsub(pattern=Esharp,replacement=Esharp_r,vec)
    vec=gsub(pattern=Fflat,replacement=Fflat_r,vec);
    vec=gsub(pattern=Gflat,replacement=Gflat_r,vec); vec=gsub(pattern=Fsharp,replacement=Fsharp_r,vec); 
    vec=gsub(pattern=Aflat,replacement=Aflat_r,vec); vec=gsub(pattern=Gsharp,replacement=Gsharp_r,vec);
    vec=gsub(pattern=Bflat,replacement=Bflat_r,vec); vec=gsub(pattern=Asharp,replacement=Asharp_r,vec); 
    vec=gsub(pattern=Bsharp,replacement=Bsharp_r,vec)
    vec=gsub(pattern=Cflat,replacement=Cflat_r,vec)
    
    vec=gsub(pattern=C,replacement=C_r,vec)
    vec=gsub(pattern=D,replacement=D_r,vec)
    vec=gsub(pattern=E,replacement=E_r,vec)
    vec=gsub(pattern=F_,replacement=F_r,vec); 
    vec=gsub(pattern=G,replacement=G_r,vec);
    vec=gsub(pattern=A,replacement=A_r,vec)
    vec=gsub(pattern=B,replacement=B_r,vec)
  }
  vec=gsub(pattern="r",replacement="0",vec)
  vec=gsub(pattern="n",replacement="",vec)
  
  for (i in 6:1){
    Dflat=paste(paste(rep("d",i),collapse=""),"-",sep="",collapse=""); Dflat_r=as.character((i+5)*12+2)
    Csharp=paste(paste(rep("c",i),collapse=""),"#",sep="",collapse=""); Csharp_r=as.character((i+5)*12+2)
    Eflat=paste(paste(rep("e",i),collapse=""),"\\-",sep="",collapse=""); Eflat_r=as.character((i+5)*12+4)
    Dsharp=paste(paste(rep("d",i),collapse=""),"#",sep="",collapse=""); Dsharp_r=as.character((i+5)*12+4)
    Fflat=paste(paste(rep("f",i),collapse=""),"\\-",sep="",collapse=""); Fflat_r=as.character((i+5)*12+5)
    Esharp=paste(paste(rep("e",i),collapse=""),"#",sep="",collapse=""); Esharp_r=as.character((i+5)*12+6)
    Gflat=paste(paste(rep("g",i),collapse=""),"\\-",sep="",collapse=""); Gflat_r=as.character((i+5)*12+7)
    Fsharp=paste(paste(rep("f",i),collapse=""),"#",sep="",collapse=""); Fsharp_r=as.character((i+5)*12+7)
    Aflat=paste(paste(rep("a",i),collapse=""),"\\-",sep="",collapse=""); Aflat_r=as.character((i+5)*12+9)
    Gsharp=paste(paste(rep("g",i),collapse=""),"#",sep="",collapse=""); Gsharp_r=as.character((i+5)*12+9)
    Bflat=paste(paste(rep("b",i),collapse=""),"\\-",sep="",collapse=""); Bflat_r=as.character((i+5)*12+11)
    Asharp=paste(paste(rep("a",i),collapse=""),"#",sep="",collapse=""); Asharp_r=as.character((i+5)*12+11)
    Cflat=paste(paste(rep("c",i),collapse=""),"\\-",sep="",collapse=""); Cflat_r=as.character((i+5)*12+1)
    Bsharp=paste(paste(rep("b",i),collapse=""),"#",sep="",collapse=""); Bsharp_r=as.character((i+5)*12+12) 

    C=paste(rep("c",i),sep="",collapse=""); C_r=as.character((i+5)*12+1)
    D=paste(rep("d",i),sep="",collapse=""); D_r=as.character((i+5)*12+3)
    E=paste(rep("e",i),sep="",collapse=""); E_r=as.character((i+5)*12+5)
    F_=paste(rep("f",i),sep="",collapse=""); F_r=as.character((i+5)*12+6)
    G=paste(rep("g",i),sep="",collapse=""); G_r=as.character((i+5)*12+8)
    A=paste(rep("a",i),sep="",collapse=""); A_r=as.character((i+5)*12+10)
    B=paste(rep("b",i),sep="",collapse=""); B_r=as.character((i+5)*12+12)
  
    vec=gsub(pattern=Dflat,replacement=Dflat_r,vec); vec=gsub(pattern=Csharp,replacement=Csharp_r,vec);
    vec=gsub(pattern=Eflat,replacement=Eflat_r,vec); vec=gsub(pattern=Dsharp,replacement=Dsharp_r,vec); 
    vec=gsub(pattern=Esharp,replacement=Esharp_r,vec)
    vec=gsub(pattern=Fflat,replacement=Fflat_r,vec);
    vec=gsub(pattern=Gflat,replacement=Gflat_r,vec); vec=gsub(pattern=Fsharp,replacement=Fsharp_r,vec); 
    vec=gsub(pattern=Aflat,replacement=Aflat_r,vec); vec=gsub(pattern=Gsharp,replacement=Gsharp_r,vec);
    vec=gsub(pattern=Bflat,replacement=Bflat_r,vec); vec=gsub(pattern=Asharp,replacement=Asharp_r,vec); 
    vec=gsub(pattern=Bsharp,replacement=Bsharp_r,vec)
    vec=gsub(pattern=Cflat,replacement=Cflat_r,vec)
  
    vec=gsub(pattern=C,replacement=C_r,vec)
    vec=gsub(pattern=D,replacement=D_r,vec)
    vec=gsub(pattern=E,replacement=E_r,vec)
    vec=gsub(pattern=F_,replacement=F_r,vec); 
    vec=gsub(pattern=G,replacement=G_r,vec);
    vec=gsub(pattern=A,replacement=A_r,vec)
    vec=gsub(pattern=B,replacement=B_r,vec)
  }
  return(vec)
}

mod12=function(vec){
  actual_0=which(vec==0)
  mod_vec=vec%%12
  mod_vec[which(mod_vec==0)]=12
  mod_vec[actual_0]=0
  return(mod_vec)
}


#This function returns the duration encoding of a note (including dotted notes)
dot_time=function(orig_field){
  num=NULL; squish=NULL; dot=0; total=NULL; dotcount=0
  field=unlist(strsplit(orig_field,split=""))
  for (i in 1:length(field)){
    if (str_detect(field[i],"[0123456789]")){
      if (i!=1){
        if (dot!=0){
          total=c(total,sum(as.numeric(squish)))
          squish=NULL; num=NULL; dot=0; dotcount=0
        }
      }
      num=c(num,field[i])
      if (i==length(field)){
        pastenum=as.character(1/as.numeric(paste(num,sep="",collapse="")))
        squish=c(squish,pastenum)
        total=c(total,sum(as.numeric(squish)))
      }
      
    } else if (str_detect(i,".")){
      dotcount=dotcount+1
      if (dot==0){
        squish=c(squish,1/as.numeric(paste(num,sep="",collapse="")))
      }
      dot=as.numeric(squish[dotcount])/2
      squish=c(squish,dot)
      if (i==length(field)){
        total=c(total,sum(as.numeric(squish)))
      }
    }
  }  
  new_field=paste(total,sep="",collapse=" ")
  return(new_field)                                           
}


get_pitch_position=function(field){
 field[str_detect(".",field)]=99999
 dot_location=which(field==99999)
  to_num=suppressWarnings(as.numeric(field))
  sep_notes=c(); note_seq=c()
  for (j in 1:length(to_num)){
    if (is.na(to_num[j])){
      note_seq=c(note_seq,field[j])
      if (j==length(to_num)){
        sep_notes=c(sep_notes,paste(note_seq,collapse=""))
      }
    } else {
      sep_notes=c(sep_notes,paste(note_seq,collapse=""))
      note_seq=c()
    }
  }
  rows_remove=which(sep_notes=="")
  if (length(rows_remove)>0){
    sep_notes=sep_notes[-rows_remove]
  }
  return(sep_notes)
}

get_rhythm_position=function(field){
  field[str_detect(".",field)]=99999
  dot_location=which(field==99999)
  to_num=suppressWarnings(as.numeric(field))
  sep_notes=c(); note_seq=c()
  for (j in 1:length(to_num)){
    if (is.na(to_num[j])){
      sep_notes=c(sep_notes,paste(note_seq,collapse=""))
      note_seq=c()
    } else {
      note_seq=c(note_seq,field[j])
      if (j==length(to_num)){
        sep_notes=c(sep_notes,paste(note_seq,collapse=""))
      }
    }
  }
  rows_remove=which(sep_notes=="")
  if (length(rows_remove)>0){
    sep_notes=sep_notes[-rows_remove]
  }
  return(sep_notes)
}

#--------------------------------------------------------------------------------------------------------------#

name.o=NULL; name=NULL; name.t=NULL

#Iterates through all **kern files and returns cleaned duration file, pitch file (on 1-12 scale), and pitch file (on full pitch scale)
for (i in filenum){
  #Get max number of columns in the table so that the exact column number can be specified when creating the data frame
  ncol=max(count.fields(paste0(fpath,files[i]), sep = "\t", quote = "", skip = 0,
                        blank.lines.skip = TRUE, comment.char = ""))
  #Create data frame
  y=read.table(paste0(fpath,files[i]), header = FALSE, sep="\t", fill=TRUE, quote="\"", comment.char = "",
               strip.white=FALSE, col.names=1:ncol) 
  
  #Identify rows containing **kern comments  
  comments_1=which(str_detect(y[,1],"^\\*\\*kern$")) #Row where beginning comments end
  comments_2=which(str_detect(y[,1],"^\\*\\-$")) #Row where end comments begin
  
  #Identify columns containing dynamics markings 
  dynamics_col=NULL
  for (j in 1:ncol(y)){
    if (y[[comments_1[1],j]]!="**kern")
    {
      dynamics_col=c(dynamics_col,j)
    }
  }
  
  #Remove dynamics columns and comment rows 
  if (!is.null(dynamics_col)){
    y=y[-c(1:comments_1[1],comments_2[1]:nrow(y)),-dynamics_col]
  } else {
    y=y[-c(1:comments_1[1],comments_2[1]:nrow(y)),]
  }
  
  
  #Make sure all columns are in a consistent order
  clef_row=which(str_detect(y[,1],"clef"))
  
  #Violin clef is written as "*clefF" for some Hadyn files, so fix its name (should be "*clefG4")
  clef_names=NULL
  for (j in 1:ncol(y)){
    clef_names=c(clef_names,as.character(y[[clef_row[1],j]])) 
  }
  if (grepl("o",files[i],fixed=TRUE)){
    if (any(str_detect(clef_names,"^\\*clefF$"))){
      clef_names=gsub(pattern="^\\*clefF$",replacement="\\*clefG4",clef_names)
    }
  } 
  logic=c(str_detect(clef_names[1],"C"),str_detect(clef_names[2],"F"),str_detect(clef_names[3],"G"),str_detect(clef_names[4],"G"))
  if (any(logic==FALSE)){
    y=y[-clef_row,]
    clefC=which(str_detect(clef_names,"C"))
    clefF=which(str_detect(clef_names,"F"))
    clefG=which(str_detect(clef_names,"G"))
    y=cbind(y[,clefC],y[,clefF],y[,clefG])
    clef_names=c(clef_names[clefC],clef_names[clefF],clef_names[clefG])
    y[,3]=factor(y[,3], levels = c(sort(unique(c(levels(y[,3]),clef_names[4])))))
    y[,4]=factor(y[,4], levels = c(sort(unique(c(levels(y[,4]),clef_names[4])))))
    y=rbind(clef_names,y)
  }
  
  #Identify any rows containing comments
  star_rows=NULL
  for (j in 1:nrow(y)){
    if (grepl("*", y[j,1],fixed=TRUE)){
      star_rows=c(star_rows,j)
    }
  }
 
  #Remove bar lines, "*" lines, and comment lines
  bar_row=which(str_detect(y[,1],"="))
  comment_row=which(str_detect(y[,1],"!"))
  rows_to_go=c(bar_row,star_rows,comment_row)
  y=y[-rows_to_go,]

  #Parse data for letters, numbers, dot (for dotted notes), and accidentals
  str_pitch=NULL; str_dur=NULL
  for (j in 1:ncol(y)){
    strlist=str_extract_all(y[,j],"[.123456789ABCDEFGnrabcdefg#\\-]")
    strlist[is.na(strlist)]="" #Blank spaces were coerced to NAs, so change them back to ""
    for (k in 1:length(strlist)){
      unlisted=unlist(strsplit(strlist[[k]],""))
      if (is.null(unlisted) || unlisted=="."){
        str_pitch=c(str_pitch,"")
        str_dur=c(str_dur,"")
      } else {
        sep_notes=get_pitch_position(unlisted)
        oct=as.numeric(unlist(lapply(sep_notes,translate_octave)))
        max_pitch=max(oct)[1]
        sep_dur_notes=get_rhythm_position(unlisted)
        good_dur=sep_dur_notes[which(oct==max_pitch)] #Gets duration associated with highest pitch
        str_pitch=c(str_pitch,max_pitch) 
        str_dur=c(str_dur,good_dur)
      }
    }
  }
  
  dot_replace=unlist(lapply(str_dur,function(x) gsub(pattern="99999",replacement=".",x)))
  for (j in 1:length(dot_replace)){
    if (is.na(dot_replace[j])){
      dot_replace[j]=""
    } else if (!str_detect(dot_replace[j],pattern=".")){ 
      dot_replace[j]=as.character(1/as.numeric(dot_replace[j]))
    } else {
      dot_replace[j]=dot_time(dot_replace[j])
    }
  }
  
  #Make pitch and duration data frames
  duration.df=data.frame(matrix(as.numeric(dot_replace),ncol=4))
  names(duration.df)=clef_names
  
  pitch_all.df=data.frame(matrix(as.numeric(as.character(str_pitch)),ncol=4)) #Pitch on full octave scale
  
  pitch_12.df=apply(pitch_all.df,2,mod12) #Pitch restricted to only 12 pitches (relative pitch)

  
  #Name files and assign to workspace
  #Hadyn
  if (grepl("o",files[i],fixed=TRUE)){
    nums=unlist(str_extract_all(files[i], "[0-9]"))
    name.tdf=paste("tdf_","H_","op",nums[1],nums[2],"_no",nums[3],"_mvmt",nums[4],nums[5],sep="")
    name.pdf=paste("pdf_","H_","op",nums[1],nums[2],"_no",nums[3],"_mvmt",nums[4],nums[5],sep="")
    name.podf=paste("podf_","H_","op",nums[1],nums[2],"_no",nums[3],"_mvmt",nums[4],nums[5],sep="")
    comp="H"
    #Mozart
  } else {
    nums=unlist(str_extract_all(files[i], "[0-9]"))
    name.tdf=paste("tdf_","M_","k",nums[1],nums[2],nums[3],"_mvmt",nums[4],nums[5],sep="")
    name.pdf=paste("pdf_","M_","k",nums[1],nums[2],nums[3],"_mvmt",nums[4],nums[5],sep="")
    name.podf=paste("podf_","M_","k",nums[1],nums[2],nums[3],"_mvmt",nums[4],nums[5],sep="")
    comp="M"
  }
    assign(name.tdf,duration.df)
    assign(name.pdf,pitch_12.df)
    assign(name.podf,pitch_all.df)
    
    #To save the files
    #fwrite(duration.df,paste0(fpath,name.tdf,".csv"))
    #fwrite(pitch_12.df,paste0(fpath,name.pdf,".csv"))
    #fwrite(pitch_all.df,paste0(fpath,name.podf,".csv"))
  
  name=c(name,name.pdf)
  name.t=c(name.t,name.tdf)
  name.o=c(name.o,name.podf)
 
}
