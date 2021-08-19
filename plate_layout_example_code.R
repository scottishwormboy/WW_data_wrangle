#change to something appropriate
setwd("/Users/stevep11/Dropbox/Datafiles/covid/WW_runs/plate_layout/v3plate")

#reads in an RNA plate coming from EA-ALB lab
#generates a set of 8x12 spreadsheets to help visualise the plates
write.plate.layout("20210817_ALB_RNA_PLATE_LIVERPOOL_UNI.csv",rownames=FALSE,colnames=FALSE)
#typically lab will edit these manually to squash together onto sequencing plates

#this is the nimagen index file for 96-well plates
load("/Users/stevep11/Dropbox/Datafiles/covid/WW_runs/plate_layout/nimagen.idx.list.2021.06.10.rdata")

#write out a sample sheet for each 96-well sequencing plate
#here it takes as input the out of the previous function, but this would be edited if the
#lab team have squashed samples together onto a sequencing plate
#note carefully whether workflow A or B needed for MiSeq/NextSeq/Novaseq
#see Nimagen protocol
write.4col(file.in="tmp_ypcmsvwoxlzv/2122611.csv", idx.list = nimagen.June21.idx.list,
           idx.id=3, SampleProject = "dummyProject1", i7_index="samplesheet_seq",
           i5_index="samplesheet_workflow_A", file.out.csv = "2122611_4col.csv")
#typically concatenate manually all the spreadsheets from a run, not yet coded this part but easy

#takes a sample sheet and the EA file and outputs a combined spreadsheet
write.out.JBCseq(EA.file="20210817_ALB_RNA_PLATE_LIVERPOOL_UNI.csv", samp.sheet.file="2122611_4col.csv")

#if you need to concatenate multiple output files, put them into a directory by themselves and use
#concat.JBCinput.csv()