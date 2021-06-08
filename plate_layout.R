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

##SET DIRECTORY

setwd("/path/to/your/files")

##GENERATE PLATE LAYOUT
#write.plate.layout will write each plate found to a separate file

write.plate.layout("test_layout_1.csv",rownames=FALSE,colnames=FALSE)

#print.plate.layout will print plate layout to console or to file

print.plate.layout(file.csv="test_layout_1.csv",file.out="plate_output.txt")