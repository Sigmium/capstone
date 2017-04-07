# load required libraries
library(tidyr)
library(dplR)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table) 

setwd("c:/myfiles")

### DEVICE LOOKUPS ###

# Import Device Model Number to Product Name tables
# Load & Wrangle ANDROID device list from local CSV file (from https://support.google.com/googleplay/answer/1727131?hl=en)
AndroidDevices <- read_csv("original_AndroidDevices.csv")
names(AndroidDevices)[names(AndroidDevices)=="Retail Branding"] <- "RETAIL_BRANDING"
names(AndroidDevices)[names(AndroidDevices)=="Marketing Name"] <- "FRIENDLY_PRODUCT_NAME"
names(AndroidDevices)[names(AndroidDevices)=="Device"] <- "DEVICE_MODEL"
names(AndroidDevices)[names(AndroidDevices)=="Model"] <- "DEVICE_MODEL_2"
AndroidDevices$PRODUCT_MODEL_INFO <- c("N/A")
# Excluding 6 of 14.6k rows that split incorrectly due to "," in manufacturer name
AndroidDevices$X5 <- NULL

# Load & Wrangle APPLE device list from local CSV file (from https://support.hockeyapp.net/kb/client-integration-ios-mac-os-x-tvos/ios-device-types)
AppleDevices <- read_csv("original_AppleDevices.csv")
names(AppleDevices)[names(AppleDevices)=="DEVICE TYPE"] <- "DEVICE_MODEL"
names(AppleDevices)[names(AppleDevices)=="PRODUCT NAME"] <- "PRODUCT_NAME"
AppleDevices <- AppleDevices %>% 
  separate_('PRODUCT_NAME', into = c('FRIENDLY_PRODUCT_NAME','PRODUCT_MODEL_INFO'), sep = '\\(')
AppleDevices$PRODUCT_MODEL_INFO <- gsub('\\)','', AppleDevices$PRODUCT_MODEL_INFO)
AppleDevices$RETAIL_BRANDING <- c("Apple")
AppleDevices$DEVICE_MODEL_2 <- c("N/A")

# Merge into master device lookup & save
MasterDeviceList <- rbind(AppleDevices, AndroidDevices)
write_csv(MasterDeviceList, "clean_MasterDeviceList.csv")
write_csv(AndroidDevices, "clean_AndroidDevices.csv")
write_csv(AppleDevices, "clean_AppleDevices.csv")

### LOGIN ANALYSIS ###

# Import merged login data file 
Login <- read_csv("original_MobileLogin.csv")

# Wrangle, cleanup, add & save 
# TODO: Use mutate, adjust data formats, correct for timezones and daylight savings time, and...
names(Login)[names(Login)=="_time"] <- "Timestamp"
names(Login)[names(Login)=="count"] <- "Volume"
Login$Date <- as.POSIXct(strptime(Login$"Timestamp", "%Y-%m-%d"))
Login$Hour <- strftime(Login$"Timestamp", "%H:%M")
Login$Weekday <- weekdays(as.Date(Login$Date))
Login$WeekNumber <- strftime(Login$Date, format = "%W")

write_csv(Login, "clean_MobileLogin.csv")

# Filter, assess quality, size, stats (mean, min, max)
  # filter
  # missing or unexpected values
  # mean, min, max
  # regression

# Import master device lookup list
MasterDeviceList <- read_csv("MasterDeviceList.csv")

# Lookup & append to login data file
# TODO: Solve for device model identifiers accross multiple columns, fallouts, excptions, over counting, etc...

# Vizualize, plot, regression

# iOS Failure/Success/Policy volume by Device and Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS" & DEVICE_TYPE!="Android" )

ggplot(data = x, aes(x = Volume, y = DEVICE_MODEL, size = Volume, color = AUTH_METHOD)) + 
  geom_jitter()
  geom_point()

# Success volume over time by Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS") %>%
  group_by(Date, AUTH_METHOD) %>%
  summarise(Total = sum(Volume)) 

ggplot(data = x, aes(x = Date, y = Total, fill = AUTH_METHOD)) + 
    geom_area()

# Save results

# Misc vizuals

wk_by_auth <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>% 
  group_by(WeekNumber, AUTH_METHOD) %>% 
  summarize(Total = sum(Volume))
