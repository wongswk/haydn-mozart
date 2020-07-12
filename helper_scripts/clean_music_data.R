# This script parses and cleans **kern files for the HM285 or HM107 datasets.

#Load libraries 
library(stringr) # (for easier string manipulation)

Mozart_path = paste0(home_path, "/data/", dataset, "/Mozart/") # path where Mozart kern files are saved
Haydn_path = paste0(home_path, "/data/", dataset, "/Haydn/") # path where Mozart kern files are saved

# List all kern files in this dataset, to be loaded in
Mozart_files = list.files(Mozart_path, pattern=".krn")
Haydn_files =list.files(Haydn_path, pattern=".krn")

if (dataset == "HM285") {
  omitted_scores = c("op103-03.krn", "op76n6-01.krn", "op01n0-03.krn", "op09n3-01.krn", "op20n6-01.krn", "op33n3-04.krn", "op76n2-01.krn")
  Haydn_files = Haydn_files[-which(Haydn_files %in% omitted_scores)]
}

Haydn_num = length(Haydn_files)
Mozart_num = length(Mozart_files)

files = c(Mozart_files, Haydn_files) 
file_num = length(files)



#-------------------------------------------------------------------------------------------#
# Define functions to be used later

# Translation of kern pitches to numeric values on full octave scale (and rests = 0)
translate_octave=function(vec){
  for (i in 6:1){
    
    Dflat = paste(paste(rep("D", i), collapse = ""), "-", sep="", collapse =""); Dflat_r = as.character((6 - i) * 12 + 2)
    Csharp = paste(paste(rep("C", i), collapse = ""), "#", sep = "", collapse = ""); Csharp_r = as.character((6 - i) * 12 + 2)
    Eflat = paste(paste(rep("E", i), collapse = ""), "\\-", sep = "", collapse = ""); Eflat_r = as.character((6 - i) * 12 + 4)
    Dsharp = paste(paste(rep("D", i), collapse = ""), "#", sep = "", collapse = ""); Dsharp_r = as.character((6 - i) * 12 + 4)
    Fflat = paste(paste(rep("F", i), collapse = ""), "\\-", sep = "", collapse = ""); Fflat_r = as.character((6 - i) * 12 + 5)
    Esharp = paste(paste(rep("E", i), collapse = ""), "#", sep = "", collapse = ""); Esharp_r = as.character((6-i)*12+6)
    Gflat = paste(paste(rep("G",i),collapse=""),"\\-",sep="",collapse=""); Gflat_r=as.character((6-i)*12+7)
    Fsharp = paste(paste(rep("F",i),collapse=""),"#",sep="",collapse=""); Fsharp_r=as.character((6-i)*12+7)
    Aflat = paste(paste(rep("A",i),collapse=""),"\\-",sep="",collapse=""); Aflat_r=as.character((6-i)*12+9)
    Gsharp = paste(paste(rep("G",i),collapse=""),"#",sep="",collapse=""); Gsharp_r=as.character((6-i)*12+9)
    Bflat = paste(paste(rep("B",i),collapse=""),"\\-",sep="",collapse=""); Bflat_r=as.character((6-i)*12+11)
    Asharp = paste(paste(rep("A",i),collapse=""),"#",sep="",collapse=""); Asharp_r=as.character((6-i)*12+11)
    Cflat = paste(paste(rep("C",i),collapse=""),"\\-",sep="",collapse=""); Cflat_r=as.character((6-i)*12+1)
    Bsharp = paste(paste(rep("B",i),collapse=""),"#",sep="",collapse=""); Bsharp_r=as.character((6-i)*12+12) 
    
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

mod12 = function(vec){
  actual_0=which(vec==0)
  mod_vec=vec%%12
  mod_vec[which(mod_vec==0)]=12
  mod_vec[actual_0]=0
  return(mod_vec)
}


# This function returns the duration encoding of a note (including dotted notes)
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

get_rhythm_position = function(field){
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
# Define the main function


clean_data = function(file_i) {
  if (file_i %in% Haydn_files) {
    path = Haydn_path
    Haydn = T
  } else {
    path = Mozart_path
    Haydn = F
  }
  
  # Get max number of columns in the table so that the exact column number can be specified when creating the data frame
  ncol = max(count.fields(paste0(path, file_i), sep = "\t", quote = "", skip = 0,
                          blank.lines.skip = TRUE, comment.char = ""))
  # Create data frame
  y = read.table(paste0(path, file_i), header = FALSE, sep="\t", fill=TRUE, quote="\"", comment.char = "",
                 strip.white=FALSE, col.names=1:ncol) 
  
  #Identify rows containing **kern comments  
  comments_1 = which(str_detect(y[,1], "^\\*\\*kern$")) # Row where beginning comments end
  comments_2 = unique(c(which(str_detect(y[,1], "^\\*\\-$")), which(str_detect(y[,2], "^\\*\\-$")))) # Row where end comments begin
  
  # Identify columns containing dynamics markings 
  dynamics_col=NULL
  for (j in 1:ncol(y)){
    if (y[[comments_1[1], j]] != "**kern")
    {
      dynamics_col = c(dynamics_col, j)
    }
  }
  
  # Remove dynamics columns and comment rows 
  if (!is.null(dynamics_col)){
    y = y[-c(1:comments_1[1], comments_2[1]:nrow(y)), -dynamics_col]
  } else {
    y = y[-c(1:comments_1[1], comments_2[1]:nrow(y)), ]
  }
  
  
  # We will identify the instrument corresponding to each column of the kern file, then reorder the columns to be in a consistent format. 
  clef_row = which(str_detect(y[, 1], "clef")) 
  instrument_row = which(apply(y, 1, function(x) T %in% str_detect(x, "Viola")) == T)
  
  if (str_detect(path, "107") & file_i %in% c("op09n3-01.krn", "op20n6-01.krn", "op33n3-04.krn")) {
    # These movements in HM107 have 5 columns, due to simultaneous notes. 
    # From manual inspection, it appears that the extra column for the first 2 movements corresponds to violin 1, vhile the extra column in the third movement corresponds to violin 2.
    
    # This is a helper function that will remove rests and other extraneous information from the extra column, before it is pasted with the main column. 
    str_helper = function(entry) {
      if (str_detect(entry, "r")) {
        # If the entry contains only a rest, then nothing should be pasted with the main column (since the duration info is already in the main column)
        return("")
      } else {
        # If there is an actual note in the extra column, then that entry should be pasted with the main column
        notes = lapply(c("a", "b", "c", "d", "e", "f", "g", "A", "B", "C", "D", "E", "F", "G"), function(s) str_detect(entry, s))
        if (! any(unlist(notes) == T)) {
          return("")
        } else {
          # If there is no rest or note, then the entry contains only ".", which should be removed (otherwise it will be confused for a dotted note later in the code)
          return(as.character(entry))
        }
      }
    }
    
    if (file_i %in% c("op09n3-01.krn", "op20n6-01.krn")) {
      clef_orig = as.character(y[clef_row, 1])
      extra_col = unlist(lapply(y[, 2], str_helper))
      y[, 2] = factor(extra_col, levels = unique(extra_col))
      
      pasted = apply(y, 1, function(row) paste0(row[1], row[2]))
      pasted[clef_row] = clef_orig
      y[, 1] = factor(pasted)
      y = y[, -2]
    }
    
    if (file_i == "op33n3-04.krn") {
      clef_orig = as.character(y[clef_row, 3])
      extra_col = unlist(lapply(y[, 3], str_helper))
      y[, 3] = factor(extra_col, levels = unique(extra_col))
      
      pasted = apply(y, 1, function(row) paste0(row[2], row[3]))
      pasted[clef_row] = clef_orig
      y[, 2] = factor(pasted)
      y = y[, -3]
    }
  } 
  
  if (length(instrument_row) != 0 & dataset == "HM107") {
    # Some files have explicitly marked the instrument corresponding to each column as "*I:Violino 1", "*I:Violino 2", "*I:Viola", or "*I:Violoncello". 
    # In that case, we will use that information. 
    instrument_names = as.character(unlist(y[instrument_row, ]))
    clef_names = as.character(unlist(y[clef_row, ]))
    y = y[-c(clef_row, instrument_row), ] # remove rows with clef names and instrument names, since we will reform the clef names
    # Identify the order of the clefs
    clefC = which(str_detect(instrument_names, "Viola"))
    clefF = which(str_detect(instrument_names, "cello"))
    clefG1 = c(which(str_detect(instrument_names, "Violino 1")), which(str_detect(instrument_names, "Violino I$")))
    clefG2 = c(which(str_detect(instrument_names, "Violino 2")), which(str_detect(instrument_names, "Violino II")))
    
    y = cbind.data.frame(y[, clefC], y[, clefF], y[, clefG2], y[, clefG1])
    clef_names = c(clef_names[clefC], clef_names[clefF], clef_names[clefG2], clef_names[clefG1])
    y[, 3] = factor(y[, 3], levels = c(sort(unique(c(levels(y[, 3]), clef_names[4])))))
    y[, 4] = factor(y[, 4], levels = c(sort(unique(c(levels(y[, 4]), clef_names[4])))))
    y = rbind(clef_names, y)
  } else {
    # If the instrument information isn't explicitly provided, we will use the clef names to estimate which instrument is which.
    # Get names of the clefs. 
    clef_names = NULL
    for (j in 1:ncol(y)){
      clef_names = c(clef_names, as.character(y[[clef_row[1], j]])) 
    }
    
    # Violin clef is written as "*clefF" for some Hadyn files, so fix its name (should be "*clefG4")
    if (Haydn) {
      if (any(str_detect(clef_names, "^\\*clefF$"))) {
        clef_names = gsub(pattern = "^\\*clefF$", replacement="\\*clefG4", clef_names)
      }
    } 
    
    # The columns will be reordered to be Viola in column 1, Cello in column 2, Violin 2 in column 3, and Violin 1 in Column 4.
    # Movements in HM107 typically have columns in a different order from HM285, so we will break into cases to see if the columns
    # are in their expected orders, given the dataset. 
    if (str_detect(path, "285")) {
      violin_reorder = c(1, 2)  # will be used later
      logic = c(str_detect(clef_names[1], "C"), str_detect(clef_names[2], "F"), str_detect(clef_names[3], "G"), str_detect(clef_names[4],"G")) 
      
    } else if (str_detect(path, "107")) {
      # HM107 often have violin 1 in column 1, violin 2 in column 2, viola in column 3, and cello in column 4.
      violin_reorder = c(2, 1)  # will be used later; represents that violins are in the opposite order of HM285.
      logic = c(str_detect(clef_names[3], "C"), str_detect(clef_names[4], "F"), str_detect(clef_names[2], "G"), str_detect(clef_names[1],"G")) 
      # 
      if (!any(logic == FALSE)) {
        # if the clefs are in the typical order for HM107, then we will reorder the columns accordingly. 
        y = y[, c(3, 4, 2, 1)] 
      }
      
    }
    
    # If any clef names are out of the order we wanted, then we will attempt to detect them and reorder them. 
    if (any(logic == FALSE)) {
      y = y[-clef_row, ]
      # Identiy the order of the clefs
      clefC = which(str_detect(clef_names, "C"))
      clefF = which(str_detect(clef_names, "F"))
      clefG = which(str_detect(clef_names, "G"))
      y = cbind(y[ , clefC], y[ , clefF], y[ , clefG[violin_reorder]])
      clef_names = c(clef_names[clefC], clef_names[clefF],clef_names[clefG])
      y[, 3] = factor(y[, 3], levels = c(sort(unique(c(levels(y[, 3]), clef_names[4]))))) # third column is generally violin 2
      y[, 4] = factor(y[, 4], levels = c(sort(unique(c(levels(y[, 4]), clef_names[4]))))) # fourth column is generally violin 1
      y = rbind(clef_names, y)
    }
  }
  
  
  #Identify any rows containing comments, which would be denoted by *
  star_rows = NULL
  for (j in 1:nrow(y)){
    if (grepl("*", y[j, 1], fixed=TRUE)){
      star_rows = c(star_rows, j)
    }
  }
  
  #Remove bar lines, "*" lines, and comment lines
  bar_row = which(str_detect(y[, 1], "="))
  comment_row = which(str_detect(y[, 1], "!"))
  rows_to_go = c(bar_row, star_rows, comment_row)
  y = y[-rows_to_go, ]
  
  #Parse data for letters, numbers, dot (for dotted notes), and accidentals
  str_pitch = NULL; str_dur=NULL
  for (j in 1:ncol(y)){
    strlist = str_extract_all(y[, j], "[.123456789ABCDEFGnrabcdefg#\\-]")
    strlist[is.na(strlist)] = "" #Blank spaces were coerced to NAs, so change them back to ""
    for (k in 1:length(strlist)) {
      unlisted = unlist(strsplit(strlist[[k]], ""))
      if (is.null(unlisted) || unlisted==".") {
        str_pitch = c(str_pitch,"")
        str_dur = c(str_dur, "")
      } else {
        sep_notes = get_pitch_position(unlisted)
        oct = suppressWarnings(as.numeric(unlist(lapply(sep_notes, translate_octave)))) # unidentified pitches will be converted to NAs
        max_pitch = max(oct)[1]
        sep_dur_notes = get_rhythm_position(unlisted)
        good_dur = sep_dur_notes[which(oct == max_pitch)] # Gets duration associated with highest pitch
        str_pitch = c(str_pitch, max_pitch) 
        str_dur = c(str_dur, good_dur)
      }
    }
  }
  
  dot_replace = unlist(lapply(str_dur, function(x) gsub(pattern="99999", replacement=".", x)))
  for (j in 1:length(dot_replace)) {
    if (is.na(dot_replace[j])) {
      dot_replace[j] = ""
    } else if (!str_detect(dot_replace[j], pattern=".")) { 
      dot_replace[j] = as.character(1/as.numeric(dot_replace[j]))
    } else {
      dot_replace[j] = dot_time(dot_replace[j])
    }
  }
  
  #Make pitch and duration data frames
  duration.df = suppressWarnings(data.frame(matrix(as.numeric(dot_replace), ncol=4))) # durations resulting from unidentified pitches will be trimmed off here. 
  names(duration.df) = clef_names
  
  pitch_all.df = data.frame(matrix(as.numeric(as.character(str_pitch)), ncol=4)) #Pitch on full octave scale
  
  pitch_12.df = apply(pitch_all.df, 2, mod12) #Pitch restricted to only 12 pitches (relative pitch)
  
  
  #Name files and assign to workspace
  mvmt_name = strsplit(file_i, "[.]")[[1]][1]
  #Hadyn
  if (Haydn) {
    label = "H"
  } else {
    label = "M"
  }
  
  name.pdf = paste0("pdf_", label, "_", mvmt_name)
  name.tdf = paste0("tdf_", label, "_", mvmt_name)
  name.podf= paste0("podf_", label, "_", mvmt_name)
  
  assign(name.tdf, duration.df, .GlobalEnv)
  assign(name.pdf, pitch_12.df, .GlobalEnv)
  assign(name.podf, pitch_all.df, .GlobalEnv)
  
  return(list(name.pdf, name.tdf, name.podf))
}

cleaned_data = lapply(files[1:length(files)], clean_data)


name = sapply(cleaned_data, "[[", 1)
name.t = sapply(cleaned_data, "[[", 2)
name.o = sapply(cleaned_data, "[[", 3)
