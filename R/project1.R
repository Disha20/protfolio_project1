install.packages("tidyverse")
install.packages("janitor")
install.packages("skimr")
install.packages("Hmisc")

library(tidyverse)
library(janitor)
library(skimr)
library(Hmisc)

dailyActivity_df <- read_csv("dailyActivity_merged.csv")
head(dailyActivity_df)

dailyCalories_merged_df <- read_csv("dailyCalories_merged.csv")
head(dailyCalories_merged_df)
View(dailyCalories_merged_df)

daily_sleep <- read.csv("sleepDay_merged.csv")
head(daily_sleep)

dailySteps_merged_df <- read_csv("dailySteps_merged.csv")
head(dailySteps_merged_df)


heartrate_seconds_merged_df <- read_csv("heartrate_seconds_merged.csv")
head(heartrate_seconds_merged_df)

dailyIntensities_merged_df <- read_csv("dailyIntensities_merged.csv")
head(dailyIntensities_merged_df)

weightLogInfo_merged_df <- read_csv("weightLogInfo_merged.csv")
head(weightLogInfo_merged_df)

hourlyCalories_df <- read_csv("hourlyCalories_merged.csv")
head(hourlyCalories_df)

merge_1 <- merge(dailyActivity_df, dailyCalories_merged_df, by = c("Id", "Calories"))
head(merge_1)

# to merge daily activity, daily arories and remove duplicate activityDay & ActivtyDate column
merge_2 <- merge(merge_1, dailyIntensities_merged_df, by = c("Id", "ActivityDay")) %>%
  select(-ActivityDay) %>%
  rename(Date = ActivityDate)

head(merge_2)

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

dailySteps_merged_df %>%
  duplicated() %>%
  sum()



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
  duplicated()
sum()

# we found that there is duplcate in daily sleep and sleep day column so lets found that so we can remove it.
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
dailyCalories_merged_df <- dailyCalories_merged_df %>%
  rename(ActivityDate = ActivityDay)
dailyIntensities_merged_df <- dailyIntensities_merged_df %>%
  rename(ActivityDate = ActivityDay)
hourlyCalories_df <- hourlyCalories_df %>%
  rename(ActivityTime = ActivityHour)

# change the data type if it needs to be chanaged.

#daily_sleep$SleepDate <- format(as.Date(daily_sleep$SleepDate), "%m/%d/%Y")

# add day of the week columns so we can have idea of the day.



# convert the date formate to POSIXxt and it will convert it to the object of class "character" and it will be use in the 
dailyActivity_df$ActivityDate <- as.POSIXct(dailyActivity_df$ActivityDate, format="%m/%d/%Y", tz="UTC")
dailyCalories_merged_df$ActivityDate <- as.POSIXct(dailyCalories_merged_df$ActivityDate,format="%m/%d/%Y", tz="UTC")
daily_sleep$SleepDate <- as.POSIXct(daily_sleep$SleepDate, format="%m/%d/%Y", tz="UTC")
dailyIntensities_merged_df$ActivityDate <-as.POSIXct(dailyIntensities_merged_df$ActivityDate, format="%m/%d/%Y", tz="UTC")
weightLogInfo_merged_df$Date <- as.POSIXct(weightLogInfo_merged_df$Date,format="%m/%d/%Y", tz="UTC")
hourlyCalories_df$ActivityTime <- as.POSIXct(hourlyCalories_df$ActivityTime, format="%m/%d/%Y", tz="UTC")

class(dailyActivity_df$ActivityDate)





dailyActivity_df$ActivityDay <- weekdays(dailyActivity_df$ActivityDate)
dailyCalories_merged_df$ActivityDay <- weekdays(dailyCalories_merged_df$ActivityDate)
daily_sleep$SleepDay <- weekdays(daily_sleep$SleepDate)
dailyIntensities_merged_df$ActivityDay <- weekdays(dailyIntensities_merged_df$ActivityDate)
weightLogInfo_merged_df$Date <- weekdays(weightLogInfo_merged_df$Date)
hourlyCalories_df$ActivityTime <- weekdays(hourlyCalories_df$ActivityTime)

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

# Now calculate the time not asleep in bed based on the TotalMinutesAsleep and TotalTimeInBed in the daily_sleep table. 
# It would be interesting to explore the trends of the time people stay in bed when they are not asleep.

#calculate the time not asleep in bed
daily_sleep <- daily_sleep %>%
  mutate(MinutesNotAsleepInBed = TotalTimeInBed - TotalMinutesAsleep)

# count unique records (ID) in each table
n_distinct(dailyActivity_df$Id)
n_distinct(dailyCalories_merged_df)
n_distinct(daily_sleep$Id)
n_distinct(dailyIntensities_merged_df$Id)
n_distinct(weightLogInfo_merged_df$Id)
n_distinct(hourlyCalories_df$Id)

# Removing duplicates and dropping NA (if any) 

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

#sum(duplicated(sleep_day))


# dailyActivity_df_filter = dailyActivity_df %>%
#                           summary()
#View(dailyActivity_df_filter)

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


# Look at the distribution of activity level data
hist.data.frame(daily_activity_filtered)


daily_sleep_selected <- daily_sleep %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         MinutesNotAsleepInBed) 

daily_sleep_selected %>% summary()

hist.data.frame(daily_sleep_selected)

# is there any trend of activity level and sleep throughout the week?
# Group activity level by days of week

daily_Activity_week <- dailyActivity_df %>% 
  group_by(ActivityDay) %>%
  summarise(mean_total_steps = mean(TotalSteps),
            mean_total_distance = mean(TotalDistance),
            median_very_active_minutes = median(VeryActiveMinutes),
            meadian_fairly_Active_minutes = median(FairlyActiveMinutes),
            meadian_lightly_Active_minutes = median(LightlyActiveMinutes),
            mean_sedentary_minutes = mean(SedentaryMinutes))

View(daily_Activity_week)

# now let's see relationship between activity data and days of week by graph
ggplot(daily_Activity_week) + 
  geom_col(mapping=aes(y=mean_total_steps, x=ActivityDay)) + 
  labs(title="Mean Total Steps Over the Week", 
       x="Day of the Week",
       y="Number of Steps")

ggplot(daily_Activity_week) + 
  geom_col(mapping=aes(y=mean_sedentary_minutes, x=ActivityDay), fill='darkgreen') + 
  labs(title="Mean Sedentary Minutes Over the Week", 
       x="Day of the Week",
       y="Minutes")

ggplot(daily_Activity_week) + 
  geom_col(mapping=aes(y=median_very_active_minutes, x=ActivityDay), fill='blue') + 
  labs(title="Median 'Very Active Minutes' Over the Week", 
       x="Day of the Week",
       y="Minutes")+
  annotate("text", x=6, y=4, label="Less time spent on vigorous-intensity \n activity during the weekend", 
           color="red", fontface="bold", size=4)


ggplot(daily_Activity_week) + 
  geom_col(mapping=aes(y=meadian_fairly_Active_minutes, x=ActivityDay), fill='purple') + 
  labs(title="Median 'Fairly Active Minutes' Over the Week", 
       x="Day of the Week",
       y="Minutes")+
  annotate("text", x=6.3, y=5.2, label="Less time spent on moderate- \n intensity activity during \n the weekend", 
           color="red", fontface="bold", size=4)

# now, lets see if we can find any trend in sleep

daily_sleep_week <- daily_sleep %>%
  group_by(SleepDay) %>%
  summarise(mean_total_sleep_hours = mean(TotalMinutesAsleep) / 60,
            mean_total_minutes_not_asleep_in_bed = mean(MinutesNotAsleepInBed))

ggplot(daily_sleep_week)+
  geom_col(mapping=aes(y=mean_total_sleep_hours, x=SleepDay), color='orange')+
  labs(title="Mean Hours of Sleep Over the Week", 
       x="Day of the Week",
       y="Hours")

ggplot(daily_sleep_week)+
  geom_col(mapping=aes(y=mean_total_minutes_not_asleep_in_bed, x=SleepDay), color='yellow')+
  labs(title="Mean Number of Minutes Not Asleep in Bed Over the Week", 
       x="Day of the Week",
       y="Minutes")

# now lets check what is the relationship between active time and calories burn
# noramlly the assumption is the more active you are, the higher the calories you burn. Lets see if this is true or not

# turn wide data into long data by creating a variable for the internisty of activity
# using 'daily_activity' dataframe, choosing variables for analysis, converting dataframe from wide to long

calorie_and_active_minutes <- dailyActivity_df %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, Calories) %>%
  rename(very_active = VeryActiveMinutes, fairly_active = FairlyActiveMinutes, lightly_active = LightlyActiveMinutes) %>%
  pivot_longer(!Calories, names_to = "Intensity", values_to = "Minutes")

head(calorie_and_active_minutes)

daily_activity_filtered %>%  
  select(VeryActiveMinutes,FairlyActiveMinutes,LightlyActiveMinutes) %>%
  summary()

# resize the ggplot to make it bigger
options(repr.plot.width = 15, repr.plot.height = 8)

#plot the facet graph
calorie_and_active_minutes %>%
  ggplot() +
  geom_point(mapping=aes(y=Calories, x=log(Minutes), color=Intensity))+ # apply log transformation on minutes
  geom_smooth(mapping=aes(y=Calories, x=log(Minutes), color=Intensity))+
  facet_wrap(~Intensity)+
  labs(title="Relationship between active minutes and calories burn")




# lets check what is the relationship between activity level and sleep?
# create date column
dailyActivity_df <- dailyActivity_df %>%
  mutate(Date = as.Date(ActivityDate))

daily_sleep <- daily_sleep %>%
  mutate(Date = as.Date(SleepDate))

# Outer join of two tables based on ID and date
# Outer join 'daily_activity' with 'sleep_day'

combined_data <- full_join(dailyActivity_df, daily_sleep, by=c("Id", "Date"))
head(combined_data)

View(combined_data)

n_distinct(combined_data$Id)


# id is 33 so we suceessfully join 2 tables with outer join
ggplot(combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=TotalSteps), color='dark green')+
  labs(title="Total Steps vs. Total Sleep Per Day", 
       x="Total minutes asleep",
       y="Total steps")

# this graph does not show clear trend so check the relationship between sleep and sendentary minutes
ggplot(combined_data) + 
  geom_point(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
  geom_smooth(mapping=aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
  geom_vline(xintercept = 200, linetype="dotted", color = "red", size=1)+
  geom_vline(xintercept = 600, linetype="dotted", color = "red", size=1)+
  labs(title="Sedentary Minutes vs. Total Sleep Per Day", 
       x="Total minutes asleep",
       y="Sedentary minutes")

# calories burned vs hours




hourlyCalories_df2 <- hourlyCalories_df %>% 
  group_by(ActivityTime) %>%
  summarise(mean_calories = mean(Calories))
head(hourlyCalories_df2)

# creating a bar chart
ggplot(hourlyCalories_df2, aes(x = ActivityTime, y = mean_calories)) +
  geom_bar(start = "identity", fill = "plum3") +
  geom_hline(yintercept = mean(hourlyCalories_df2$mean_calories),
             linetype = "dashed", color = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Calories Burned vs. Hours", x = "Time", y = "Average calories burned")


# calories Burned vs days

# using daily_activity dataframe, grouping by Day, finding mean of calories

Calories_days <- dailyCalories_merged_df %>%
  group_by(ActivityDate) %>%
  summarise(avg_calories = mean(Calories))

head(Calories_days)

# creating a bar chart
ggplot(Calories_days, aes(x = ActivityDate, y = avg_calories)) +
  geom_bar(start = "identity", aes(fill = avg_calories)) +
  scale_fill_continuous(limits = c(0, 2500)) +
  geom_hline(yintercept = mean(Calories_days$avg_calories),
             linetype = "dashed", color = "red") +
  theme_bw() +
  labs(title = "Calories Burned vs. Days", x = "Day", y = "Average calories burned", fill = "Calories")





######################################################################
# Calculate correlation coefficient (omit NA data)
cor.test(combined_data$SedentaryMinutes, combined_data$TotalMinutesAsleep, na.action=na.omit)

combined_data %>% 
  filter(TotalMinutesAsleep >= 200 & TotalMinutesAsleep <= 600) %>%
  select(TotalMinutesAsleep, SedentaryMinutes) %>%
  na.omit() %>% # omit NA rows
  cor()

combined_data <- combined_data %>% 
  mutate(ModToVigMinutes = VeryActiveMinutes + FairlyActiveMinutes)

head(combined_data)

ggplot(combined_data) 
geom_jitter(mapping=aes(x=TotalMinutesAsleep, y=ModToVigMinutes), color="dark green")+
  labs(title="Relationship between Moderate to Vigorous Intensity Activity with Sleep",
       x="Total minutes asleep per day",
       y="Moderate-to-vigorous-intensity activity minutes per day")

combined_data %>% 
  select(TotalMinutesAsleep, ModToVigMinutes) %>%
  na.omit() %>% # omit NA rows
  cor()