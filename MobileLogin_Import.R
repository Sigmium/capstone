# load required libraries
library(readr)
library(data.table)
library(httr)
library(curl)
library(jsonlite)
library(rvest)

# Set location of source files as working directory
# read_csv(choose.files()) to open dialogue box to select file. Same for write_csv
setwd("c:/myfiles")

# Get list of available CSV files
filelist <- list.files(pattern="*.csv")

# Import and merge availabe source data into temporary dataframe 
Login <- do.call(rbind, lapply(filelist, fread))

# Import Android Devices from Google Play Supported Devices ULR/CSV
  #CSV Import via URL:
    # TASK: Troubleshoot why this isnt working
    # GoogleSupportedDevices <- read_delim(url("http://storage.googleapis.com/play_public/supported_devices.csv"))
    # GoogleSupportedDevices <- read_delim(url("http://storage.googleapis.com/play_public/supported_devices.csv"), delim = ",")
  #CSV Import after download & column split: 
    # TASKS: Rows 3784 - 3787, 5212 & 13253 have errors due to comma in first value/column. Need to wrangle.    
    GoogleSupportedDevices <- read_csv("c:/Users/TDP177/OneDrive - Capital One Financial Corporation/Data Science - Springboard/GooglePlaySupportedDevices.csv")

# Import iOS Devices from theiphonewiki.com
  # Wiki API Help URL https://www.wikidata.org/w/api.php
  # The iPhone Wiki URL https://www.theiphonewiki.com/wiki/Models
  # TASKS: Need to study and build HTTP API extract process  

url <- "https://www.theiphonewiki.com/wiki/Models"
GET(url)

# Write data to output file for later analysis
write_csv(Login, "c:/myfiles/original_MobileLogin.csv")
write_csv(GoogleSupportedDevices, "c:/myfiles/original_GoogleSupportedDevices.csv")
