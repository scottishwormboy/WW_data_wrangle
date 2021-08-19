# WW_data_wrangle
A collection  of code to wrangle data within WW project

plate_layout.R contains functions for manipulating the RNA samplesheet and working in the lab

write.plate.layout() takes spreadsheets from EA labs and converts to 8 x 12 plate layout, useful for the lab teams

write.4col() creates a spreadsheet that can be used with Illumina platforms and that is needed for write.out.JBCseq

write.out.JBCseq() creates a file that can be uploaded to JBC as metadata

combine_csvs.R

concat.JBCinput.csv() concatenates several RNA templates files, with or without sequence id data
