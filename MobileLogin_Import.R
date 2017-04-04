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

# Import Google Play Supported Devices CSV
  #URL Import:
    # GoogleSupportedDevices <- read_delim(url("http://storage.googleapis.com/play_public/supported_devices.csv"))
    # GoogleSupportedDevices <- read_delim(url("http://storage.googleapis.com/play_public/supported_devices.csv"), delim = ",")
 
GoogleSupportedDevices <- read_csv("c:/Users/TDP177/OneDrive - Capital One Financial Corporation/Data Science - Springboard/GooglePlaySupportedDevices.csv")
  # Local File Import: (ISSUE: Seperated columns in Excel. Rows 3784 - 3787, 5212 & 13253 have errors due to comma in first variable)

# Write data to output file for later analysis
write_csv(Login, "c:/myfiles/original_MobileLogin.csv")
write_csv(GoogleSupportedDevices, "c:/myfiles/original_GoogleSupportedDevices.csv")
