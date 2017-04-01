# load required libraries
library(tidyr)
library(dplR)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table) 

# Import merged data file 
setwd("c:/myFiles")
Login <- read_csv("c:/myFiles/original_MobileLogin.csv")

# Wrangle, cleanup, add & save [TODO: Use mutate, adjust data formats, correct for timezones and daylight 
# savings time, and...]
names(Login)[names(Login)=="_time"] <- "Timestamp"
names(Login)[names(Login)=="count"] <- "Volume"
Login$Date <- as.POSIXct(strptime(Login$"Timestamp", "%Y-%m-%d"))
Login$Hour <- strftime(Login$"Timestamp", "%H:%M")
Login$Weekday <- weekdays(as.Date(Login$Date))
Login$WeekNumber <- strftime(Login$Date, format = "%W")
write_csv(Login, "c:/myFiles/clean_MobileLogin.csv")

# Filter, assess quality, size, stats (mean, min, max)
  # filter
  # missing or unexpected values
  # mean, min, max
  # regression

# Vizualize, plot, regression

summ <- filter(Login, Date=="2017-03-15")

x <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>%
  group_by(DEVICE_TYPE, AUTH_METHOD) %>%
  summarise(Total = sum(Volume)) %>%
  ggplot(data = x, aes(x = DEVICE_TYPE, y = Total, fill = AUTH_METHOD)) + 
    geom_area() 

# Save results

# Misc vizuals

wk_by_auth <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>% 
  group_by(WeekNumber, AUTH_METHOD) %>% 
  summarize(Total = sum(Volume))