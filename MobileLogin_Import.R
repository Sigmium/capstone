# load required libraries
library(readr)
library(data.table)
library(httr)
library(curl)
library(jsonlite)
library(rvest)
library(xml2)

# Set location of source files as working directory
setwd("c:/myFiles")

# Get list of available CSV files
# TODO: 'login_' prefix is to ensure only login data files are pulled into filelist, but is broken and needs fixing 
filelist <- list.files(pattern="login_*.csv")

# Import and merge availabe source login data into temporary dataframe 
Login <- do.call(rbind, lapply(filelist, fread))

# Write data to output file for later analysis
write_csv(Login, "original_MobileLogin.csv")
