#read in functions from plate_layout.R

setwd("~/Dropbox/Datafiles/covid/WW_runs/20210809_test")

dir("Original Plate Info Files 07.07.21")

# create a simple function to concatenate all input csv files into one

# read files in a directory and output to a directory
# on a mac this works even if there are spaces in the directory name, but good practice not to use spaces

concat.JBCinput.csv <- function(filedir,fileout,sortrows=TRUE,na=""){
  #filedir is a directory (folder) containing the EA csv files, either absolute or relative path
  #fileout is the name of the file to be created, will be created in the current path
  #remember to add .csv extension 
  #don't send it to the same directory as the input files
  #get into a habit of good directory structure, since files will be written in and out
  
  fileout <- gsub(" ","_",fileout) #gets rid of spaces in file name, removing extra periods is too complicated
  
  csvfiles <- dir(filedir,pattern = "*.csv",full.names = TRUE)
  
  if(length(csvfiles)==1) stop(paste("Only 1 csv file found:",csvfiles))
  
  
  outdf <- read.csv(csvfiles[1])
  for(csvnm in csvfiles[-1]){
    outdf <- rbind(outdf,read.csv(csvnm))
  }
  if(sortrows) outdf <- outdf[order(outdf$rna_plate_number,outdf$rna_sample_plate_position_row,outdf$rna_sample_plate_position_column),]
  write.csv(outdf,file=fileout,na=na,quote=TRUE,row.names=FALSE)
}


concat.SampleSheets <- function(filedir,fileout,sortrows=TRUE,na=""){
  #take 4 col Illumina files found in directory filedir and merge together
  # create a directory just for the individual sample sheets
  
  #to do...
}

#try it out

setwd("~/Dropbox/Datafiles/covid/WW_runs/20210809_test")

concat.JBCinput.csv(filedir="Original Plate Info Files 07.07.21",fileout="test_EA_comb.csv")

#now try writing out

write.plate.layout("test_EA_comb.csv",rownames=FALSE,colnames=FALSE,outdir="tst")

write.out.JBCseq(EA.file="test_EA_comb.csv", samp.sheet.file="Sample Indexes 2.csv",out.file="20210713_outfile_test.csv")

