# install necessary library
install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("Hmisc")

library(tidyverse)
library(janitor)
library(skimr)
library(Hmisc)

# add data to the R studio for clean up

dailyActivity_df <- read_csv("dailyActivity_merged.csv")
head(dailyActivity_df)

dailyCalories_merged_df <- read_csv("dailyCalories_merged.csv")
head(dailyCalories_merged_df)
View(dailyCalories_merged_df)

daily_sleep <- read.csv("sleepDay_merged.csv")
head(daily_sleep)

#dailySteps_merged_df <- read_csv("dailySteps_merged.csv")
#head(dailySteps_merged_df)


heartrate_seconds_merged_df <- read_csv("heartrate_seconds_merged.csv")
head(heartrate_seconds_merged_df)

dailyIntensities_merged_df <- read_csv("dailyIntensities_merged.csv")
head(dailyIntensities_merged_df)

weightLogInfo_merged_df <- read_csv("weightLogInfo_merged.csv")
head(weightLogInfo_merged_df)

hourlyCalories_df <- read_csv("hourlyCalories_merged.csv")
head(hourlyCalories_df)

# remove duplicate or check if there is any duplicate
dailyActivity_df %>%
  duplicated() %>%
  sum()

dailyCalories_merged_df %>%
  duplicated() %>%
  sum()

daily_sleep %>%
  duplicated() %>%
  sum()

#dailySteps_merged_df %>%
#  duplicated() %>%
#  sum()

heartrate_seconds_merged_df %>%
  duplicated() %>%
  sum()

dailyIntensities_merged_df%>%
  duplicated() %>%
  sum()

weightLogInfo_merged_df%>%
  duplicated() %>%
  sum()

hourlyCalories_df%>%
  duplicated() %>%
  sum()

# we found that there is duplcate in daily sleep column so lets found that so we can remove it.
daily_sleep[duplicated(daily_sleep),]

# remove duplicate row 
daily_sleep <- daily_sleep %>% distinct()

# checking if duplicate removed or not
daily_sleep[duplicated(daily_sleep),]

daily_sleep %>%
  duplicated() %>%
  sum()

# change the columns name if it needs to be changed so first checked if all the names are right or not.

daily_sleep <- daily_sleep %>% 
  rename(SleepDate = SleepDay)

head(daily_sleep)

dailyCalories_merged_df <- dailyCalories_merged_df %>%
  rename(ActivityDate = ActivityDay)

head(dailyCalories_merged_df)

dailyIntensities_merged_df <- dailyIntensities_merged_df %>%
  rename(ActivityDate = ActivityDay)

head(dailyIntensities_merged_df)

hourlyCalories_df <- hourlyCalories_df %>%
  rename(ActivityTime = ActivityHour)

head(hourlyCalories_df)




# add day of the week columns so we can have idea of the day.



# convert the date formate to POSIXxt and it will convert it to the object of class "character" and it will be use in the 

dailyActivity_df$ActivityDate <- as.POSIXct(dailyActivity_df$ActivityDate, format="%m/%d/%Y", tz="UTC")
head(dailyActivity_df)

dailyCalories_merged_df$ActivityDate <- as.POSIXct(dailyCalories_merged_df$ActivityDate,format="%m/%d/%Y", tz="UTC")

daily_sleep$SleepDate <- as.POSIXct(daily_sleep$SleepDate, format="%m/%d/%Y", tz="UTC")

dailyIntensities_merged_df$ActivityDate <-as.POSIXct(dailyIntensities_merged_df$ActivityDate, format="%m/%d/%Y", tz="UTC")

weightLogInfo_merged_df$Date <- as.POSIXct(weightLogInfo_merged_df$Date,format="%m/%d/%Y", tz="UTC")

hourlyCalories_df$ActivityTime <- as.POSIXct(hourlyCalories_df$ActivityTime, format="%m/%d/%Y", tz="UTC")

class(dailyActivity_df$ActivityDate)







dailyActivity_df$ActivityDay <- weekdays(dailyActivity_df$ActivityDate)
head(dailyActivity_df)

dailyCalories_merged_df$ActivityDay <- weekdays(dailyCalories_merged_df$ActivityDate)
head(dailyCalories_merged_df)

daily_sleep$SleepDay <- weekdays(daily_sleep$SleepDate)
head(daily_sleep)

dailyIntensities_merged_df$ActivityDay <- weekdays(dailyIntensities_merged_df$ActivityDate)
head(dailyIntensities_merged_df)

weightLogInfo_merged_df$Date <- weekdays(weightLogInfo_merged_df$Date)
head(weightLogInfo_merged_df)

hourlyCalories_df$ActivityTime <- weekdays(hourlyCalories_df$ActivityTime)
head(hourlyCalories_df)

# convert days into ordered factors so the days can appear in the correct order in the visuals later

dailyActivity_df$ActivityDay <- ordered(dailyActivity_df$ActivityDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                               "Friday", "Saturday", "Sunday"))
head(dailyActivity_df)

dailyCalories_merged_df$ActivityDay <- ordered(dailyCalories_merged_df$ActivityDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                                             "Friday", "Saturday", "Sunday"))
head(dailyCalories_merged_df)

daily_sleep$SleepDay <- ordered(daily_sleep$SleepDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                               "Friday", "Saturday", "Sunday"))
head(daily_sleep)

dailyIntensities_merged_df$ActivityDay <- ordered(daily_sleep$SleepDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                                 "Friday", "Saturday", "Sunday"))

weightLogInfo_merged_df$Date <- ordered(daily_sleep$SleepDay, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                       
                                                                       "Friday", "Saturday", "Sunday"))
hourlyCalories_df$ActivityTime <- ordered(hourlyCalories_df$ActivityTime, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                                                                   
                                                                                   "Friday", "Saturday", "Sunday"))

# dropping NA (if any) 

dailyActivity_df <- dailyActivity_df %>%
  drop_na()
dailyCalories_merged_df <- dailyCalories_merged_df %>%
  drop_na()
daily_sleep <- daily_sleep %>%
  drop_na()# %>%
#unique()
dailyIntensities_merged_df <- dailyIntensities_merged_df %>%
  drop_na()
weightLogInfo_merged_df <- weightLogInfo_merged_df %>%
  drop_na()
hourlyCalories_df <- hourlyCalories_df %>%
  drop_na()



# finding rows with zero values

nrow(filter(dailyActivity_df, TotalSteps == 0))
nrow(filter(daily_sleep, TotalSleepRecords == 0))
nrow(filter(weightLogInfo_merged_df, WeightKg == 0))

daily_activity_filtered = dailyActivity_df %>%  
  select(TotalSteps,
         TotalDistance,
         VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>% summary()
View(daily_activity_filtered)

install.packages("rio")
library(rio)

# export cleaned csv for analysis purpose

export(dailyActivity_df, "dailyActivity_df.csv")
export(dailyCalories_merged_df, "dailyCalories_merged_df.csv")
export(daily_sleep, "daily_sleep.csv")
export(heartrate_seconds_merged_df, "heartrate_seconds_merged_df.csv")
export(dailyIntensities_merged_df, "dailyIntensities_merged_df.csv")
export(weightLogInfo_merged_df, "weightLogInfo_merged_df.csv")
export(hourlyCalories_df, "hourlyCalories_df.csv")
