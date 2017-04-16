# load required libraries
library(readr)
library(data.table)

# Set location of source files as working directory
setwd("c:/myFiles/raw_data")

# Get list of available CSV files
filelist <- list.files(pattern="*.csv")

# Import and merge availabe source login data into temporary dataframe 
Login <- do.call(rbind, lapply(filelist, fread))

# Write data to output file for later analysis
write_csv(Login, "original_MobileLogin.csv")
