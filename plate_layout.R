#CODE TO GENERATE A PLATE LAYOUT FROM A CSV FILE IN THE FORMAT GIVEN BY JBC AT 8/6/21
#TYPICAL USE IS TO READ FILES IN AND WRITE FILES IN THE SAME DIRECTORY
#READ THE FUNCTIONS IN AND USE THEM AS PER EXAMPLES AT THE BOTTOM


### READ IN FUNCTIONS

write.plate.layout <- function(file.csv,rownames=TRUE,colnames=TRUE){
  #file must be in csv format and have the columns in the if statement below
  #will stop with an error if multiple wells for the same plate found
  #may be other formatting errors I've not thought of
  
  #output will be a set of csv files with 8 rows and 12 columns and sample IDs
  
  #example: write.plate.layout("test_layout_1.csv",rownames=FALSE,colnames=FALSE)
  
  pl.in <- read.csv(file=file.csv)
  if(sum(names(pl.in) %in% c("rna_plate_number","rna_sample_plate_position","rna_sample_plate_position_row","rna_sample_plate_position_column","sample_id"))!=5) stop("missing fields: one of rna_plate_number,rna_sample_plate_position,rna_sample_plate_position_row,rna_sample_plate_position_column,sample_id")
  
  #step through each plate found
  pl.nos <- unique(pl.in$rna_plate_number)
  for(plate in pl.nos){
    pl.subset <- pl.in[pl.in$rna_plate_number==plate,]
    test.table <- table(pl.subset$rna_sample_plate_position)
    if(any(test.table>1)) stop(paste("plate ",plate,"has multiple samples for the same well"))
    out.mat <- matrix(data="not_filled",nrow=8,ncol=12,dimnames=list(rows=LETTERS[1:8],cols=paste("c",1:12,sep="_")))
    for(m in 1:8){
      for(n in 1:12){ #go through each well, test whether there's a record, if so print sample id
        if(any(pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n)){
          out.mat[m,n] <- paste(plate,
                                pl.subset$rna_sample_plate_position[pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n],
                                pl.subset$sample_id[pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n],sep="_")
        }
      }
    }
    #called the values for rownames/colnames in a confusing way
    write.table(out.mat,file=paste(plate,".csv",sep=""),sep=",",row.names=rownames,col.names=colnames)
    #quote=TRUE is default, stops crap with commas etc in field names
  }
}


print.plate.layout <- function(file.csv,rownames=LETTERS[1:8],colnames=paste("c",1:12,sep="_"),file.out=""){
  #file must be in csv format and have the columns in the if statement below
  #will stop with an error if multiple wells for the same plate found
  #may be other formatting errors I've not thought of
  
  #output will be matrices printed to the console with 8 rows and 12 columns and sample IDs
  #set a 
  
  #example: print.plate.layout(file.csv="test_layout_1.csv",file.out="plate_output.txt")
  
  
  
  pl.in <- read.csv(file=file.csv)
  if(sum(names(pl.in) %in% c("rna_plate_number","rna_sample_plate_position","rna_sample_plate_position_row","rna_sample_plate_position_column","sample_id"))!=5) stop("missing fields: one of rna_plate_number,rna_sample_plate_position,rna_sample_plate_position_row,rna_sample_plate_position_column,sample_id")
  
  #wipe any existing output file, bit of a bodge
  cat("",file=file.out)
  
  #step through each plate found
  pl.nos <- unique(pl.in$rna_plate_number)
  for(plate in pl.nos){
    cat(plate,file=file.out,append=TRUE)
    cat("\n",file=file.out,append=TRUE)
    pl.subset <- pl.in[pl.in$rna_plate_number==plate,]
    test.table <- table(pl.subset$rna_sample_plate_position)
    if(any(test.table>1)) stop(paste("plate ",plate,"has multiple samples for the same well"))
    out.mat <- matrix(data="not_filled",nrow=8,ncol=12,dimnames=list(rows=rownames,cols=colnames))
    for(m in 1:8){
      for(n in 1:12){ #go through each well, test whether there's a record, if so print sample id
        if(any(pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n)){
          out.mat[m,n] <- paste(plate,
                                pl.subset$rna_sample_plate_position[pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n],
                                pl.subset$sample_id[pl.subset$rna_sample_plate_position_row==LETTERS[m]&pl.subset$rna_sample_plate_position_column==n],sep="_")
        }
      }
    }
    #called the values for rownames/colnames in a confusing way
    for(m in 1:8) {
      cat(out.mat[m,],file=file.out,append=TRUE)
      cat("\n",file=file.out,append=TRUE)
    }
    cat("\n",file=file.out,append=TRUE)
  }
}

#TO LINK PLATE LAYOUT TO NIMAGEN INDEXES

write.4col <- function(file.in, idx.list, idx.id, SampleProject, i7_index="samplesheet_seq", i5_index,file.out.csv=NULL){
  #
  #file.in is a 8 x 12 format csv file with sample names, eg produced by write.plate.layout()
  #index.list is a list object containing a set of databases for each of the set of 96 barcodes
  #idx.id is the number of the index plate required by the user, eg 1 - 6 for U01 - U06
  #SampleProject given to all 96 samples
  #i7_index name of the column in the index file used for this column, default "samplesheet_seq"
  #i5_index name, as above, but has to be set by user, check carefully but
  #typically samplesheet_workflow_A for MiSeq and samplesheet_workflow_B for MiniSeq
  #flie.out.csv file to write results to
  #
  #example usage
  # write.4col(plate96.in="211493.csv", idx.list = nimagen.June21.idx.list,
  # idx.id=3, SampleProject = "dummyProject1", i7_index="samplesheet_seq",
  # i7_index="samplesheet_workflow_A", file.out.csv = "dummyProject1_4col.csv")
  
  plate96.in <- as.matrix(read.table(file.in,sep=",",header=FALSE))
  if(is.null(file.out.csv)) file.out.csv <- paste(SampleProject,"4col.csv",sep="_")
  
  if(!all(dim(plate96.in) == c(8,12))) stop("input plate not in 8 x 12 format")
  
  out.4col <- data.frame(SampleID=rep("gone_wrong",96),i7_index=rep("gone_wrong",96),
                         i5_index=rep("gone_wrong",96),SampleProject=rep("gone_wrong",96))
  #shouldn't be any 'gone_wrong' in output
  
  #this is clunky, but safe
  tmp <- rep("gone_wrong",96)
  for(m in 1:12){
    tmp[seq((1+8*(m-1)),8*m)] <- plate96.in[,m]
  }
  tmp <- gsub("-","_",tmp) #Miseq doesn't like dashes, may not like other characters
  tmp <- gsub(" ","",tmp)
  
  out.4col$SampleID <- paste(idx.id,paste("seqloc",idx.list[[idx.id]]$plate_location,sep="_"),
                             tmp,sep="_")
  
  #this next bit follows what is done in the lab for the oligos used
  #if anything changes in the lab, different machine etc, will need changed
  
  out.4col$i7_index <- idx.list[[idx.id]][,i7_index]
  out.4col$i5_index <- idx.list[[idx.id]][,i5_index]
  out.4col$SampleProject <- SampleProject
  
  #write out file
  write.table(out.4col,file=file.out.csv,quote=FALSE,row.names=FALSE,sep=",")
  #note that a comma in a field name will screw this up
}

#function to write sequence ids to EA files

write.out.JBCseq <- function(EA.file,samp.sheet.file,out.file=NULL,seq.lab= "LIVE"){
  # Takes a csv file from EA and merges with a 4 column (Illumina) sample sheet,
  # created with write.4col
  
  if(is.null(out.file)) out.file <- sub(".csv","_withseq.csv",EA.file)
  
  # read in 4col samplesheet
  # takes info from the sample id, eg
  # 3_seqloc_A1_211493_A1_VIR1L_2105829_36_02
  # 211493 is the EA RNA plate id, A1 is the EA RNA position
  
  samp.sheet <- read.csv(samp.sheet.file)
  samp.sheet$rna_plate_number <-  
    sapply(in.4col$SampleID,function(X){strsplit(X,split="_")[[1]][4]})
  samp.sheet$rna_sample_plate_position <-
    sapply(in.4col$SampleID,function(X){strsplit(X,split="_")[[1]][5]})
  
  
  #create a field to merge
  samp.sheet$RNA_plate_pos_id <- paste(samp.sheet$rna_plate_number,
                                       samp.sheet$rna_sample_plate_position,sep="_")
  # gives some odd values for not filled wells, but ok
  
  #read in EA spreadsheet, should be able to combine
  
  
  na <- "" #what to put as missing values
  pl.in <- read.csv(file=EA.file)
  pl.in$RNA_plate_pos_id <- paste(pl.in$rna_plate_number,
                                  pl.in$rna_sample_plate_position,sep="_")
  
  # do the merge
  tmp.merge.df <- merge(pl.in, samp.sheet[,c("RNA_plate_pos_id","SampleProject","SampleID")], 
                        by = "RNA_plate_pos_id", all.x = TRUE)
  
  tmp.merge.df$run_id <- tmp.merge.df$SampleProject
  tmp.merge.df$sample_sequencing_id <- tmp.merge.df$SampleID
  tmp.merge.df$sequencing_lab_code[!is.na(tmp.merge.df$sample_sequencing_id)] <- seq.lab
  
  #order and remove duplicate/unused columns
  pl.out <- tmp.merge.df[order(tmp.merge.df$rna_plate_number,tmp.merge.df$rna_sample_plate_position_row,tmp.merge.df$rna_sample_plate_position_column),!names(tmp.merge.df) %in% c("RNA_plate_pos_id","SampleProject","SampleID")]
  
  write.csv(pl.out, file=out.file,na=na,quote=TRUE,row.names=FALSE)
  # note that need to have quote=TRUE because some bell-end put commas in a field name
  
}


##SET DIRECTORY

setwd("/path/to/your/files")

##GENERATE PLATE LAYOUT
#write.plate.layout will write each plate found to a separate file

write.plate.layout("test_layout_1.csv",rownames=FALSE,colnames=FALSE)

#print.plate.layout will print plate layout to console or to file

print.plate.layout(file.csv="test_layout_1.csv",file.out="plate_output.txt")

#To generate sample sheet

#get the data on nimagen plates, in the form of an r object called nimagen.June21.idx.list
load("path/to/nimagen.idx.list.2021.06.10.rdata")
#this can sit somewhere that's used on successive weeks
# other versions of nimagen indixes or swift will need this rewritten
# input file 211493.csv comes from write.plate.layout
# see the notes in the function above re i5/i7 indices, 

write.4col(file.in="211493.csv", idx.list = nimagen.June21.idx.list,
           idx.id=3, SampleProject = "dummyProject1", i7_index="samplesheet_seq",
           i5_index="samplesheet_workflow_A", file.out.csv = "dummyProject1_4col.csv")

#typical sample name from this might be
# 3_seqloc_A1_211493_A1_VIR1L_2105829_36_02
# underscores separate different elements. 3 is the plate index
# seqloc_A1 is the position on the sequencing plate
# 211493 is the EA plate reference
# A1 is the position on the EA plate
# VIR1L_2105829_36_02 is EA sample ID (dashes changed to underscores,
# previously tried colons but query to whether MiSeq would read)
# the input plate has to be in 8 x 12 format, but values can be anything, 
# eg EA id, positive_control, not_filled
# any spaces will changed to colons

# write file back out to give to Hubert et al, adding seq ids to the EA file from Tracey
# this may make most sense to (1) combine the separate files EA from a Wednesday into a 
# single file and (2) combine multiple sample sheets in a single file
# otherwise EA plates spread across different sequencing plates gets confusing

# samp.sheet.file is created with write.4col() function above
# EA.file comes from Tracey

# THIS IS NOT WELL TESTED, there may be some odd results, please check before sending out

write.out.JBCseq(EA.file="test_layout_1.csv", samp.sheet.file="dummyProject1_4col.csv")
