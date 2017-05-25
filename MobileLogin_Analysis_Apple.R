# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(data.table) 
library(vtreat) 
library(scales)

setwd("c:/myFiles/")




#### DATA IMPORT, WRANGLING & LOOKUP ####

# Import Apple Device lookup data from URL
AppleDevices <- read_csv("https://raw.githubusercontent.com/Sigmium/capstone/master/original_AppleDevices.csv")


# Wrangle and structure imported Apple Device lookup data 
names(AppleDevices)[names(AppleDevices)=="DEVICE TYPE"] <- "DEVICE_MODEL"
names(AppleDevices)[names(AppleDevices)=="PRODUCT NAME"] <- "PRODUCT_NAME"
AppleDevices <- AppleDevices %>% 
  separate_('PRODUCT_NAME', into = c('FRIENDLY_PRODUCT_NAME','PRODUCT_MODEL_INFO'), sep = '\\(')
AppleDevices$PRODUCT_MODEL_INFO <- gsub('\\)','', AppleDevices$PRODUCT_MODEL_INFO)
AppleDevices$RETAIL_BRANDING <- c("Apple")
AppleDevices$DEVICE_MODEL_2 <- c("N/A")

# Import raw login data file collected and merged using MobileLogin_Import.R 
Login <- read.csv("original_MobileLogin.csv")

# Conduct preliminary data filtering, wrangling and cleanup
# Filter to iOS devices, remove unnecessary data
Login <- filter(Login, DEVICE_TYPE != "Android") %>%
  filter(CHANNEL__TYPE == "MOBILE") %>%
  filter(DEVICE_MODEL!="x86_64") %>%
  filter(DEVICE_MODEL!="^iPad7.*") 

# Correct column names for accuracy and consistency, format timestamp and create Date, Hour, Weekday and Week Number
names(Login)[names(Login)=="X_time"] <- "Timestamp"
names(Login)[names(Login)=="count"] <- "Volume"
names(Login)[names(Login)=="DEVICE_TYPE"] <- "APP_TYPE"
Login$RESULT_DISPOSITION <- as.character(Login$RESULT_DISPOSITION)
Login <- filter(RESULT_DISPOSITION!="UNKNOWN")
Login$Timestamp <- format(Login$Timestamp, tz="America/New_York",usetz=TRUE)
Login$Date <- as.Date(Login$Timestamp,  tz="America/New_York")
Login$Hour <- strftime(Login$Timestamp, tz="America/New_York", "%H")
Login$Weekday <- weekdays(as.Date(Login$Timestamp))
Login$Weekday <- format(Login$Weekday, tz="America/New_York",usetz=FALSE)
Login$WeekNumber <- strftime(Login$Timestamp, format = "%U")
Login$WeekNumber <- format(Login$WeekNumber, tz="America/New_York",usetz=FALSE)

# Lookup and merge device info then append device super group names
Login <- full_join(Login, AppleDevices, by = "DEVICE_MODEL")
Login$DEVICE_SUPERGROUP_NAME <- Login$FRIENDLY_PRODUCT_NAME
Login <- Login %>% mutate(DEVICE_SUPERGROUP_NAME = sub('^iPhone.*', 'iPhone', DEVICE_SUPERGROUP_NAME)) %>%
  mutate(DEVICE_SUPERGROUP_NAME = sub('^iPod.*', 'iPod Touch', DEVICE_SUPERGROUP_NAME)) %>%
  mutate(DEVICE_SUPERGROUP_NAME = sub('^iPad.*', 'iPad', DEVICE_SUPERGROUP_NAME)) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME)) %>%
  filter(!is.na(Timestamp))
names(Login)[names(Login)=="DEVICE_MODEL_2"] <- "DEVICE_MODEL_NOTES"



                    
#### DATA QUALITY ANALYSIS & MEASUREMENT ####

# Quality control to validate Lookup functionality
lookup_quality <- Login %>%
  group_by(DEVICE_SUPERGROUP_NAME, FRIENDLY_PRODUCT_NAME, DEVICE_MODEL, APP_TYPE) %>%
  summarise(Total = sum(Volume))

# Quality control to identify expected/unexpected values, min, max, mean, null
str(Login)
summary(Login)

# Summary of observations (to be excluded from analysis)
dq_anomaly <- filter(Login, DEVICE_MODEL=="x86_64" | RESULT_DISPOSITION=="UNKNOWN" | is.na(FRIENDLY_PRODUCT_NAME))




#### TOP LEVEL LOGIN VOLUME, POLICY/FAILURE RATE & DISTRIBUTION ####

# Review total volume by Device (Super group)
total_VOLUME_by_super_device <- Login %>% 
  filter(!is.na(RESULT_DISPOSITION)) %>%
  group_by(DEVICE_SUPERGROUP_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_VOLUME_by_super_device$SUCCESS [is.na(total_VOLUME_by_super_device$SUCCESS)] <- 0
total_VOLUME_by_super_device$POLICY [is.na(total_VOLUME_by_super_device$POLICY)] <- 0
total_VOLUME_by_super_device$DEFECT [is.na(total_VOLUME_by_super_device$DEFECT)] <- 0
total_VOLUME_by_super_device <- mutate(total_VOLUME_by_super_device, TOTAL = (SUCCESS + POLICY + DEFECT)) 
ggplot(total_VOLUME_by_super_device, aes(x = DEVICE_SUPERGROUP_NAME, y = TOTAL))+
 geom_col()
print(total_VOLUME_by_super_device)

# Review total volume by Device (Sub group)
total_VOLUME_by_device_subgroup <- Login %>% 
  group_by(DEVICE_SUPERGROUP_NAME, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_VOLUME_by_device_subgroup$SUCCESS [is.na(total_VOLUME_by_device_subgroup$SUCCESS)] <- 0
total_VOLUME_by_device_subgroup$POLICY [is.na(total_VOLUME_by_device_subgroup$POLICY)] <- 0
total_VOLUME_by_device_subgroup$DEFECT [is.na(total_VOLUME_by_device_subgroup$DEFECT)] <- 0
total_VOLUME_by_device_subgroup <- mutate(total_VOLUME_by_device_subgroup, TOTAL = (SUCCESS + POLICY + DEFECT)) 
total_VOLUME_by_device_subgroup <- filter(total_VOLUME_by_device_subgroup, TOTAL > 0)
total_VOLUME_by_device_subgroup$TOTAL <- as.numeric(total_VOLUME_by_device_subgroup$TOTAL)
ggplot(total_VOLUME_by_device_subgroup, aes(x = FRIENDLY_PRODUCT_NAME, y = TOTAL))+
  geom_col()+ 
  scale_y_continuous(labels = comma) +
  facet_grid(DEVICE_SUPERGROUP_NAME ~ ., scales = "free") +
  coord_flip() +
  theme_classic()

# Hourly Totals by Device (Sub group)
total_hourly_VOLUME_by_sub_device <- Login %>% 
  group_by(Timestamp, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>% 
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_hourly_VOLUME_by_sub_device$SUCCESS [is.na(total_hourly_VOLUME_by_sub_device$SUCCESS)] <- 0
total_hourly_VOLUME_by_sub_device$POLICY [is.na(total_hourly_VOLUME_by_sub_device$POLICY)] <- 0
total_hourly_VOLUME_by_sub_device$DEFECT [is.na(totals$DEFECT)] <- 0
total_hourly_VOLUME_by_sub_device <- mutate(total_hourly_VOLUME_by_sub_device, TOTAL = (SUCCESS + POLICY + DEFECT)) 
total_hourly_VOLUME_by_sub_device <- filter(total_hourly_VOLUME_by_sub_device, TOTAL > 0)
total_hourly_VOLUME_by_sub_device$'<NA>' <- total_hourly_VOLUME_by_sub_device$'<NA>' <- NULL

# Hourly Totals by Device (Sub group) line chart
ggplot(total_hourly_VOLUME_by_sub_device, aes(x = Timestamp, y = TOTAL))+
  geom_line()

# Hourly Policy/Fail rate by device super group, chart over time and box plot
rate_hourly_RESULT_supergroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
rate_hourly_RESULT_supergroup_device$SUCCESS [is.na(rate_hourly_RESULT_supergroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_supergroup_device$POLICY [is.na(rate_hourly_RESULT_supergroup_device$POLICY)] <- 0
rate_hourly_RESULT_supergroup_device$DEFECT [is.na(rate_hourly_RESULT_supergroup_device$DEFECT)] <- 0
rate_hourly_RESULT_supergroup_device <- rate_hourly_RESULT_supergroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))
rate_hourly_RESULT_supergroup_device$'<NA>' <- rate_hourly_RESULT_supergroup_device$'<NA>' <- NULL

# Create one data frame for each device super group
rate_hourly_RESULT_supergroup_deviceiPhone <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
rate_hourly_RESULT_supergroup_deviceiPad <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
rate_hourly_RESULT_supergroup_deviceiPod <- rate_hourly_RESULT_supergroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")

# Chart POLICY rate over time by device super group
ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(col = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPad, aes(color = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPod, aes(color = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))

# Box plot hourly POLICY rate counts 
ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = DEVICE_SUPERGROUP_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPod, aes())

# Chart FAILURE rate over time by device super group
ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(color = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPad, aes(color = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPod, aes(color = DEVICE_SUPERGROUP_NAME, group = DEVICE_SUPERGROUP_NAME))

# Box plot hourly FAILURE rate counts 
ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = DEVICE_SUPERGROUP_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPod, aes())



#### TOP LEVEL INVESTIGATION OF OBSERVATIONS ####

# Create Policy/Fail rate data frame by device super and sub group
rate_hourly_RESULT_subgroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME,FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
rate_hourly_RESULT_subgroup_device$SUCCESS [is.na(rate_hourly_RESULT_subgroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_subgroup_device$POLICY [is.na(rate_hourly_RESULT_subgroup_device$POLICY)] <- 0
rate_hourly_RESULT_subgroup_device$DEFECT [is.na(rate_hourly_RESULT_subgroup_device$DEFECT)] <- 0
rate_hourly_RESULT_subgroup_device <- rate_hourly_RESULT_subgroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))

# Filter and split into data frames for each super group
rate_hourly_RESULT_subgroup_deviceiPhone <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
rate_hourly_RESULT_subgroup_deviceiPad <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
rate_hourly_RESULT_subgroup_deviceiPod <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")

# POLICY RATE - Plot policy rate over time for each super group, stacked by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = Timestamp, y = POLICY_RATE))+
  geom_line(aes(colour = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))


# POLICY RATE - Box plot policy rate for each super group, split by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = FRIENDLY_PRODUCT_NAME, y = POLICY_RATE))+
  geom_boxplot(aes())

# FAIL RATE - Plot fail rate over time for each super group, stacked by subgroup

ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(col = FRIENDLY_PRODUCT_NAME, group = FRIENDLY_PRODUCT_NAME))

# FAIL RATE - Box plot fail rate over time for each super group, stacked by subgroup
ggplot(rate_hourly_RESULT_subgroup_deviceiPhone, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPad, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())




#### SECOND LEVEL INVESTIGATION OF OBSERVATIONS ####

# Investigate observation - Elevated iPad Fail Rates

# Put iPad data into a separate data frame, group by RESULT RATE and APP_TYPE
x <- Login %>% 
  filter(DEVICE_SUPERGROUP_NAME =="iPad") %>%
  group_by(Timestamp, APP_TYPE, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad Fail Rate Box Plot by APP_TYPE
ggplot(x, aes(x = APP_TYPE, y = FAIL_RATE))+
  geom_boxplot(aes())

# Put iPad data into a separate data frame, (iPad App) group by RESULT RATE and AuthMethod
x <- Login %>% 
  filter(DEVICE_SUPERGROUP_NAME=="iPad") %>%
  filter(APP_TYPE=="iPad") %>%
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME, AUTH_METHOD, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)

# iPad (iPad App) Fail RATE Box Plot by AUTH_METHOD
ggplot(x, aes(x = AUTH_METHOD, y = FAIL_RATE))+
  geom_boxplot(aes()) + 
  facet_grid(. ~ DEVICE_SUPERGROUP_NAME, scales = "free") +
  theme_classic()

# iPad (iPad App) Fail RATE Box Plot by AUTH_METHOD for each sub group
x <- Login %>%
  filter(DEVICE_SUPERGROUP_NAME=="iPad") %>%
  filter(APP_TYPE=="iPad") %>%
  group_by(Timestamp, AUTH_METHOD, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
x$SUCCESS [is.na(x$SUCCESS)] <- 0
x$POLICY [is.na(x$POLICY)] <- 0
x$DEFECT [is.na(x$DEFECT)] <- 0
x <- x %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0)
x_pwd <- x %>% filter(AUTH_METHOD=="Password")
x_pat <- x %>% filter(AUTH_METHOD=="Pattern")
x_fin <- x %>% filter(AUTH_METHOD=="FingerPrint")
  
ggplot(x_pwd, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes()) +
  facet_grid(. ~ AUTH_METHOD, scales = "free") +
  theme_classic()

ggplot(x_pat, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes()) +
  facet_grid(. ~ AUTH_METHOD, scales = "free") +
  theme_classic()

ggplot(x_fin, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())+
  facet_grid(. ~ AUTH_METHOD, scales = "free") +
  theme_classic()



#### LOGISTIC REGRESSION, PREDICTION, ETC... ####

# Load required libraries for Logistic Regression 
library(caTools)
library(ROCR)
library(effects)

# Convert dependent variable to binomial: 
# Prep dependent variables for logistic regression and prediction of DEFECT (Login Failure)

Login$RESULT_FAIL <- Login$RESULT_DISPOSITION
Login <- Login %>% 
  mutate(RESULT_FAIL = sub('SUCCESS', 0, RESULT_FAIL)) %>%
  mutate(RESULT_FAIL = sub('POLICY', 0, RESULT_FAIL)) %>%
  mutate(RESULT_FAIL = sub('DEFECT', 1, RESULT_FAIL)) 
Login$RESULT_FAIL <- as.factor(Login$RESULT_FAIL)
  
# Prep dependent variables for logistic regression and prediction of POLICY (Login Policy Failure)
# This is out of scope for current project goals, but it was easy to check. 
# ROC results were significantly under 0.5.
Login$RESULT_POLICY <- Login$RESULT_DISPOSITION
Login <- Login %>% 
  mutate(RESULT_POLICY = sub('SUCCESS', 0, RESULT_POLICY)) %>%
  mutate(RESULT_POLICY = sub('POLICY', 1, RESULT_POLICY)) %>%
  mutate(RESULT_POLICY = sub('DEFECT', 0, RESULT_POLICY)) 
Login$RESULT_POLICY <- as.factor(Login$RESULT_POLICY)

# Convert statistically significant independent variables to binomial for possible model improvement
# This was determined to offer little improvement. There was no improvement 
# seen in AIC of overall GLM. 
# Convert statistically significant OS versions to binomial:
Login$OS_VER_FAIL <- factor(Login$DEVICE_OPERATING_SYSTEM_VERSION, levels=c("10.0.2", "10.0.3", "10.2.1", "10.3", "10.3.1", "10.3.2", "9.0.2", "9.3", "9.3.1", "9.3.4", "9.3.5"))
Login <- Login %>%
  mutate(OS_VER_FAIL = sub("^10.*", 1, OS_VER_FAIL)) %>%
  mutate(OS_VER_FAIL = sub("^9.*", 1, OS_VER_FAIL))
Login$OS_VER_FAIL[Login$OS_VER_FAIL != 1] <- 0
Login$OS_VER_FAIL [is.na(Login$OS_VER_FAIL)] <- 0

# Convert statistically significant Device Models to binomial: 
Login$DEV_MOD_FAIL <- factor(Login$DEVICE_MODEL, levels=c("iPad2,4", "iPad2,7","iPad3,2","iPad4,1", "iPad4,2", "iPad4,4","iPad4,5","iPad4,7","iPad4,8","iPad5,1","iPad5,2","iPad5,3","iPad5,4","iPad6,11","iPad6,12","iPad6,3","iPad6,4","iPad6,7","iPad6,8","iPhone4,1","iPhone5,1","iPhone5,2","iPhone5,3","iPhone5,4","iPhone6,1","iPhone6,2","iPhone7,1","iPhone7,2","iPhone8,1","iPhone8,2","iPhone8,4","iPhone9,1","iPhone9,2","iPhone9,3","iPhone9,4","iPod5,1","iPod7,1"))
Login <- Login %>%
  mutate(DEV_MOD_FAIL = sub("^iP.*", 1, DEV_MOD_FAIL)) 
Login$DEV_MOD_FAIL[Login$DEV_MOD_FAIL != 1] <- 0
Login$DEV_MOD_FAIL [is.na(Login$DEV_MOD_FAIL)] <- 0

# Summarize and table totals by binomial value 
# Required approach because each row represents aggregated and grouped counts

totals_fail <- Login %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(totals_fail)

totals_policy <- Login %>% 
  group_by(RESULT_POLICY) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_POLICY, Total) 
print(totals_policy)


# Converted to factors for various iterations of model 
# testing (ex: stat sig independent variables above).

Login$DEVICE_MODEL <- as.factor(Login$DEVICE_MODEL)
Login$FRIENDLY_PRODUCT_NAME <- as.factor(Login$FRIENDLY_PRODUCT_NAME)
Login$DEVICE_SUPERGROUP_NAME <- as.factor(Login$DEVICE_SUPERGROUP_NAME)
Login$PRODUCT_MODEL_INFO <- as.factor(Login$PRODUCT_MODEL_INFO)
Login$Date <- as.factor(Login$Date)
Login$APP_VERSION <- as.factor(Login$APP_VERSION)
Login$OS_VER_FAIL <- as.factor(Login$OS_VER_FAIL)
Login$DEV_MOD_FAIL <- as.factor(Login$DEV_MOD_FAIL)
Login$RESULT_FAIL <- as.factor(Login$RESULT_FAIL)
Login$Weekday <- as.factor(Login$Weekday)
Login$Hour <- as.factor(Login$Hour)

# Created logistic regression model on superset through multiple iterations and to 
# identify the best independent variables to use. 

LogisticModel <- glm(RESULT_FAIL ~ 
                      AUTH_METHOD
                      + APP_VERSION
                      + DEVICE_MODEL
                      + DEVICE_OPERATING_SYSTEM_VERSION
                      + Hour,
                      weights=Volume,
                      data=Login,
                      family="binomial")
summary(LogisticModel)

# Split data for training, testing and prediction
set.seed(1000)
split = sample.split(Login$RESULT_FAIL, SplitRatio = 0.65)
trainingData = subset(Login, split == TRUE)
testingData = subset(Login, split == FALSE)

# Collected true columes for each subst 
trainTotals <- trainingData %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(trainTotals)

testingTotals <- testingData %>% 
  group_by(RESULT_FAIL) %>% 
  summarise(Total = sum(Volume)) %>%
  spread(RESULT_FAIL, Total) 
print(testingTotals)

# Created logistic regression model on training subset, using final set of independent variables.
LogisticModel1 <- glm(RESULT_FAIL ~ AUTH_METHOD 
                      + APP_VERSION 
                      + DEVICE_MODEL
                      + DEVICE_OPERATING_SYSTEM_VERSION
                      + Hour,
                      weights=Volume,
                      data=trainingData,
                      family="binomial")
summary(LogisticModel1)

# Run model on TRAINING data, Plot and review ROC 
# Measure effectivness: ROCR for out of sample AUC
predictTrain = predict(LogisticModel1, type = "response", newdata = trainingData)
table(trainingData$RESULT_FAIL, predictTrain > 0.01)

ROCRpred = prediction(predictTrain, trainingData$RESULT_FAIL)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.010), text.adj=c(-0.5, 1.0))

# Run model on TESTING data, Plot and review ROC
predictTest = predict(LogisticModel1, type = "response", newdata = testingData)
table(testingData$RESULT_FAIL, predictTest > 0.01)

ROCRpred = prediction(predictTest, testingData$RESULT_FAIL)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.010), text.adj=c(-0.5, 1.0))




#### OBSERVATIONS ####

# OBSERVATION: Failure Rates are Higher for both iPad and iPhone apps

FailRate <- Login
FailRate$TYPE <- FailRate$FRIENDLY_PRODUCT_NAME
FailRate$TYPE <- gsub('\\iPhone','', FailRate$TYPE)
FailRate$TYPE <- gsub('\\iPad','', FailRate$TYPE)
FailRate$TYPE <- gsub('\\iPod touch','', FailRate$TYPE)
FailRate <- FailRate %>% group_by(Timestamp, DEVICE_SUPERGROUP_NAME, FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION, TYPE) %>%
  summarise(Count = sum(Volume)) %>%
  spread(RESULT_DISPOSITION, Count) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  #  Change filter to iPhone for iPhone app, iPad for iPad app
  filter(DEVICE_SUPERGROUP_NAME != "iPad") %>%
  filter(!is.na(FAIL_RATE)) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))

ggplot(data = FailRate, aes(x = TYPE, y = FAIL_RATE)) +
    geom_boxplot() +
    facet_grid(. ~ DEVICE_SUPERGROUP_NAME, scales = "free") +
    theme_classic()

# OBSERVATION: iPhone 5c outlier populations

ggplot(Login, aes(x = FRIENDLY_PRODUCT_NAME, fill = AUTH_METHOD))+
  geom_bar(aes(weights = Volume)) +
  scale_y_continuous(labels = comma) +
  facet_grid(DEVICE_SUPERGROUP_NAME ~ ., scales = "free") +
  coord_flip() +
  theme_classic()

sum_by_device <- Login %>% group_by(FRIENDLY_PRODUCT_NAME) %>% 
  summarise(Count = sum(Volume))


x <- Login %>% 
  filter(FRIENDLY_PRODUCT_NAME == "iPhone 5c") %>%
  group_by(FRIENDLY_PRODUCT_NAME, DEVICE_OPERATING_SYSTEM_VERSION, AUTH_METHOD) %>%
  summarise(Total = sum(Volume))


x$OS_VER_GROUP <- x$DEVICE_OPERATING_SYSTEM_VERSION
x <- x %>% 
  mutate(OS_VER_GROUP = sub('^10.*', 'v10.x', OS_VER_GROUP)) %>%
  mutate(OS_VER_GROUP = sub('^9.*', 'v9.x', OS_VER_GROUP)) %>%
  mutate(OS_VER_GROUP = sub('^8.*', 'v8.x', OS_VER_GROUP)) %>%
  group_by(OS_VER_GROUP,FRIENDLY_PRODUCT_NAME, DEVICE_OPERATING_SYSTEM_VERSION, AUTH_METHOD) %>%
  summarise(Total = sum(Total))


ggplot(x, aes(x = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_bar(aes(weights = Total)) + 
  scale_y_continuous(labels = comma) +
  facet_grid(OS_VER_GROUP ~ ., scales = "free") +
  coord_flip() +
  theme_classic()

x <- x %>% 
  group_by(OS_VER_GROUP,DEVICE_OPERATING_SYSTEM_VERSION) %>%
  summarise(Total = sum(Total))

# OBSERVATION: Elevated iPad Touch failure rate: 

# Create Policy/Fail rate data frame by device super and sub group
rate_hourly_RESULT_subgroup_device <- Login %>% 
  group_by(Timestamp, DEVICE_SUPERGROUP_NAME,FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total) 
rate_hourly_RESULT_subgroup_device$SUCCESS [is.na(rate_hourly_RESULT_subgroup_device$SUCCESS)] <- 0
rate_hourly_RESULT_subgroup_device$POLICY [is.na(rate_hourly_RESULT_subgroup_device$POLICY)] <- 0
rate_hourly_RESULT_subgroup_device$DEFECT [is.na(rate_hourly_RESULT_subgroup_device$DEFECT)] <- 0
rate_hourly_RESULT_subgroup_device <- rate_hourly_RESULT_subgroup_device %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(POLICY_RATE = POLICY / (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))

# Filter and split into dataframes for each super group
rate_hourly_RESULT_subgroup_deviceiPhone <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone")
rate_hourly_RESULT_subgroup_deviceiPad <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPad")
rate_hourly_RESULT_subgroup_deviceiPod <- rate_hourly_RESULT_subgroup_device %>% filter(DEVICE_SUPERGROUP_NAME == "iPod Touch")


ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = Timestamp, y = FAIL_RATE))+
  geom_line(aes(color = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPad, aes(color = DEVICE_SUPERGROUP_NAME))+
  geom_line(data = rate_hourly_RESULT_supergroup_deviceiPod, aes(color = DEVICE_SUPERGROUP_NAME))

ggplot(rate_hourly_RESULT_supergroup_deviceiPhone, aes(x = DEVICE_SUPERGROUP_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())+
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPad, aes()) +
  geom_boxplot(data = rate_hourly_RESULT_supergroup_deviceiPod, aes())

ggplot(rate_hourly_RESULT_subgroup_deviceiPod, aes(x = FRIENDLY_PRODUCT_NAME, y = FAIL_RATE))+
  geom_boxplot(aes())


#### MISC CODE - SAVE FOR LATER ####

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


# totals by Device, Result & Failure Rate

total_by_DEVICE_RESULT_FAIL_RATE <- Login %>% 
  group_by(DEVICE_SUPERGROUP_NAME, APP_TYPE, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN <- total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN
total_by_DEVICE_RESULT_FAIL_RATE$SUCCESS [is.na(total_by_DEVICE_RESULT_FAIL_RATE$SUCCESS)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$POLICY [is.na(total_by_DEVICE_RESULT_FAIL_RATE$POLICY)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$DEFECT [is.na(total_by_DEVICE_RESULT_FAIL_RATE$DEFECT)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN [is.na(total_by_DEVICE_RESULT_FAIL_RATE$UNKNOWN)] <- 0
total_by_DEVICE_RESULT_FAIL_RATE <- total_by_DEVICE_RESULT_FAIL_RATE %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT + UNKNOWN)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))
total_by_DEVICE_RESULT_FAIL_RATE$'<NA>' <- total_by_DEVICE_RESULT_FAIL_RATE$'<NA>' <- NULL


# totals by Device Type, Result & Failure Rate

total_by_DEVICE_TYPE_RESULT_FAIL_RATE <- Login %>% 
  group_by(FRIENDLY_PRODUCT_NAME, RESULT_DISPOSITION) %>%
  summarise(Total = sum(Volume)) %>% 
  spread(RESULT_DISPOSITION, Total)
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$SUCCESS [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$SUCCESS)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$POLICY [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$POLICY)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$DEFECT [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$DEFECT)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN [is.na(total_by_DEVICE_TYPE_RESULT_FAIL_RATE$UNKNOWN)] <- 0
total_by_DEVICE_TYPE_RESULT_FAIL_RATE <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE %>%
  mutate(TOTAL = (SUCCESS + POLICY + DEFECT)) %>%
  mutate(FAIL_RATE = DEFECT / (SUCCESS + POLICY + DEFECT + UNKNOWN)) %>%
  filter(!is.na(TOTAL)) %>%
  filter(TOTAL != 0) %>%
  filter(!is.na(FRIENDLY_PRODUCT_NAME))
total_by_DEVICE_TYPE_RESULT_FAIL_RATE$'<NA>' <- total_by_DEVICE_TYPE_RESULT_FAIL_RATE$'<NA>' <- NULL


# iOS version distribution by device over time

totals <- Login %>%
  group_by(Timestamp, DEVICE_OPERATING_SYSTEM_VERSION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) 

totals_iPad <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPad") 
ggplot(totals_iPad, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

totals_iPhone <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPhone, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

totals_iPod <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPod, aes(x = Timestamp, y = Total, fill = DEVICE_OPERATING_SYSTEM_VERSION))+
  geom_area(position = "fill") 

# App version distribution by device over time

totals <- Login %>%
  group_by(Timestamp, APP_VERSION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) 

totals_iPad <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPad") 
ggplot(totals_iPad, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 

totals_iPhone <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPhone, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 

totals_iPod <- totals %>% filter(DEVICE_SUPERGROUP_NAME == "iPhone") 
ggplot(totals_iPod, aes(x = Timestamp, y = Total, fill = APP_VERSION))+
  geom_area(position = "fill") 


# Result distribution by device over time

totals <- Login %>%
  group_by(Timestamp, RESULT_DISPOSITION, DEVICE_SUPERGROUP_NAME) %>%
  summarise(Total = sum(Volume)) %>%
  filter(!is.na(DEVICE_SUPERGROUP_NAME))

totals_POLICY <- totals %>% filter(RESULT_DISPOSITION == "POLICY") 
ggplot(totals_POLICY, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

totals_DEFECT <- totals %>% filter(RESULT_DISPOSITION == "DEFECT") 
ggplot(totals_DEFECT, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

totals_SUCCESS <- totals %>% filter(RESULT_DISPOSITION == "SUCCESS") 
ggplot(totals_SUCCESS, aes(x = Timestamp, y = Total, fill = DEVICE_SUPERGROUP_NAME))+
  geom_area(position = "fill") 

# Misc Visuals

wk_by_auth <- summ %>%
  filter(Login, RESULT_DISPOSITION=="SUCCESS") %>% 
  group_by(WeekNumber, AUTH_METHOD) %>% 
  summarize(Total = sum(Volume))
