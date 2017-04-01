# load required libraries
library(readr)
library(data.table) 

# Set location of source files as working directory
# read_csv(choose.files()) to open dialogue box to select file. Same for write_csv
setwd("c:/myfiles")

# Get list of available CSV files
filelist <- list.files(pattern="*.csv")

# Import and merge availabe source data into temporary dataframe 
Login <- do.call(rbind, lapply(filelist, fread))

# Write data to output file for later analysis
write_csv(original_MobileLogin.csv, "c:/myfiles")