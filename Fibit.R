library(tidyverse)

daily_activity <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\dailyActivity_merged.csv")
daily_calories <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\dailyCalories_merged.csv")
daily_intensities <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\dailyIntensities_merged.csv")
daily_steps <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\dailySteps_merged.csv")
heartrate_seconds <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\heartrate_seconds_merged.csv")
hourly_intensities <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\hourlyIntensities_merged.csv")
hourly_steps <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteCaloriesNarrow_merged.csv")
minute_calories_wide <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteIntensitiesWide_merged.csv")
minute_METs_narrow <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteStepsNarrow_merged.csv")
minute_steps_wide <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\minuteStepsWide_merged.csv")
sleep_day <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\sleepDay_merged.csv")
weightlog_info <- read_csv("C:\\Users\\Prachi\\Documents\\MyProject\\Fitabase_Data\\weightLogInfo_merged.csv")


view(daily_activity)
view(daily_calories)

view(daily_steps)
colnames(daily_steps)

view(heartrate_seconds)
heartrate_seconds_new <- separate(data=heartrate_seconds,"Time",into=c("date","time"),sep = ' ')
view(head(heartrate_seconds_new))

str(sleep_day)
view(sleep_day)
sleep_day_new <- sleep_day %>% 
  separate("SleepDay",into=c("date","time"),sep=' ')
view(sleep_day_new)


head(weightlog_info)
glimpse(weightlog_info)



# FIlter sleep_day_new for TotalTimeInBed is greater than 900
Filtered_sleep_day <- sleep_day_new %>% 
  filter(TotalTimeInBed>900) 
view(Filtered_sleep_day)


daily_steps %>% 
 summarise(max_steps = max(StepTotal))

daily_steps %>% 
 summarise(min_steps = min(StepTotal))


n_distinct(daily_activity$Id)
n_distinct(daily_steps$Id)


daily_activity %>% 
  select(Id, ActivityDate, Calories) %>% 

Merged_data <-bind_rows(daily_calories, daily_steps)
Merged_data <-merge(daily_calories, daily_steps, by='Id')

View(Merged_data)

# relationship between steps taken in a day and sedentary minutes
ggplot(data = daily_activity)+
  geom_point(mapping = aes(x=TotalSteps, y=SedentaryMinutes,color=Calories))+
  labs(title="TotalSteps VS SedentaryMinutes",subtitle = "Relationsip between between steps taken in a day and sedentary minutes")+
  annotate("text",25000,500,label="Less calories", angle=25)



# relationship between amount of time slept and the total time someone spends in bed.
ggplot(data = sleep_day)+
  geom_point(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed))
  
## There are some data points that spent a lot of time in bed, but are not sleeping
