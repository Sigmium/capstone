# load required libraries
library(tidyr)
library(dplR)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table) 

setwd("c:/myFiles/")

#### DEVICE LOOKUP IMPORT & WRANGLING ####

# Import Apple Device lookup data from URL
# AppleDevices <- read_csv("https://raw.githubusercontent.com/Sigmium/capstone/master/original_AppleDevices.csv")
AppleDevices <- read_csv("original_AppleDevices.csv")
  
# Wrangle and structure lookup data for later lookups and possible merging into larger device lookup table 
names(AppleDevices)[names(AppleDevices)=="DEVICE TYPE"] <- "DEVICE_MODEL"
names(AppleDevices)[names(AppleDevices)=="PRODUCT NAME"] <- "PRODUCT_NAME"
AppleDevices <- AppleDevices %>% 
  separate_('PRODUCT_NAME', into = c('FRIENDLY_PRODUCT_NAME','PRODUCT_MODEL_INFO'), sep = '\\(')
AppleDevices$PRODUCT_MODEL_INFO <- gsub('\\)','', AppleDevices$PRODUCT_MODEL_INFO)
AppleDevices$RETAIL_BRANDING <- c("Apple")
AppleDevices$DEVICE_MODEL_2 <- c("N/A")

# Save clean file for use in other scripts
# write_csv(AppleDevices, "clean_AppleDevices.csv")




#### LOGIN DATA IMPORT & WRANGLING ####

# Import merged login data file 
Login <- read.csv("original_MobileLogin.csv")

# Filter to Apple Device login data only
Login <- filter(Login, DEVICE_TYPE != "Android")

# Conduct preliminary data wrangling and cleanup
Login <- filter(Login, CHANNEL__TYPE == "MOBILE")
names(Login)[names(Login)=="X_time"] <- "Timestamp"
names(Login)[names(Login)=="count"] <- "Volume"
names(Login)[names(Login)=="DEVICE_TYPE"] <- "APP_TYPE"
Login$Timestamp <- format(Login$Timestamp, tz="America/New_York",usetz=TRUE)

# Save/Load clean login data file from APPLE devices
# write_csv(Login, "clean_APPLE_MobileLogin.csv")
# Login <- read_csv(Login, "clean_APPLE_MobileLogin.csv")

# Conditioning and other prep for analysis
Login$Hour <- strftime(Login$"Timestamp", "%H")
Login$Weekday <- weekdays(as.Date(Login$Timestamp))
Login$WeekNumber <- strftime(Login$Timestamp, format = "%W")

# Lookup and merge device info
Login <- full_join(Login, AppleDevices, by = "DEVICE_MODEL")

# Save clean file, conditioned and lookup device data appended 
write_csv(Login, "clean2_APPLE_MobileLogin.csv")



                    
#### LOGIN DATA QUALITY ANALYSIS & MEASUREMENT ####

# Quality check - lookup dropout, mismatces, NAs
# null_fields_raw <- Login %>% search for any values of "NULL" for null fields from raw data
# null_fields_clean <- Login %>% search for any NAs for null fields from clean data

lookup_quality <- Login %>%
  group_by(FRIENDLY_PRODUCT_NAME, DEVICE_MODEL, APP_TYPE) %>%
  summarise(Total = sum(Volume))

# Quality check - totals by (x)

total_by_AUTH_METHOD <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, AUTH_METHOD) %>% 
  summarise(Total = sum(Volume)) %>%
  filter(!is.na(Total))

total_by_DEVICE_counts <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, DEVICE_MODEL, DEVICE_OPERATING_SYSTEM, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  filter(!is.na(Total)) %>%
  spread(RESULT_DISPOSITION, Total) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT))

# Quality check - values, min, max, mean, unexpected

# Data Quality Summary
odd_rows <- filter(Login, DEVICE_MODEL=="x86_64" | RESULT_DISPOSITION=="UNKNOWN" | is.na(FRIENDLY_PRODUCT_NAME))


#### HYPOTHESIS > INVESTIGATION, REGRESSION, PROJECTION, ETC ####

# Investigation
# OS analysis - iOS vs. iPhone OS by Device, Version(s), etc...
os_summary <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, DEVICE_OPERATING_SYSTEM, DEVICE_OPERATING_SYSTEM_VERSION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(DEVICE_OPERATING_SYSTEM, Total)
# Version analysis - "  "  "
# 

# Check & plot fail rate by device, auth method, etc...
FailRate <- Login %>% group_by(FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION, AUTH_METHOD) %>%
  summarise(Count = sum(Volume)) %>%
  spread(RESULT_DISPOSITION, Count) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(FAIL_RATE)) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))

FailRate <- filter(FailRate, FAIL_RATE > 0.00001) 
ggplot(data = FailRate, aes(x = FAIL_RATE , y = FRIENDLY_PRODUCT_NAME, color = AUTH_METHOD)) +
  geom_point()

# Save results




#### MISC SCRIPTS ####

# iOS Failure/Success/Policy volume by Device and Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS" & DEVICE_TYPE!="Android" )

ggplot(data = x, aes(x = Volume, y = FRIENDLY_PRODUCT_NAME, size = Volume, color = AUTH_METHOD)) + 
  geom_jitter()
  geom_point()

# Success volume over time by Authentication Method

x <- filter(Login, RESULT_DISPOSITION=="SUCCESS") %>%
  group_by(Date, AUTH_METHOD) %>%
  summarise(Total = sum(Volume)) 

ggplot(data = x, aes(x = Date, y = Total, fill = AUTH_METHOD)) + 
    geom_area()

# Misc vizuals

wk_by_auth <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>% 
  group_by(WeekNumber, AUTH_METHOD) %>% 
  summarize(Total = sum(Volume))




#### SAVE FOR LATER ####

# OBSERVATION: Failure Rates are Higher for both iPad and iPhone apps

FailRate <- Login %>% group_by(Timestamp, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION, DEVICE_TYPE) %>%
  summarise(Count = sum(Volume)) %>%
  spread(RESULT_DISPOSITION, Count) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  #  Change filter to iPhone for iPhone app, iPad for iPad app
  filter(DEVICE_TYPE == "iPhone")
  filter(!is.na(FAIL_RATE)) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))

ggplot(data = FailRate, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE)) +
  geom_boxplot()

# OBSERVATION: iPhone 5c outlier populations #

sum_by_device <- Login %>% group_by(FRIENDLY_PRODUCT_NAME) %>% 
  summarise(Count = sum(Volume))
# Create bar chart or plot showing distribution of login by device
ggplot(data = sum_by_device, aes(x = FRIENDLY_PRODUCT_NAME, y = Volume)) +
  geom_bar()
