---
title: "Reproducible Research: Peer Assessment 1"
output:
  html_document:
    keep_md: true
    toc: true
    toc_float: true
    number_sections: true
    theme: darkly
---
# Session Settings


```r
knitr::opts_chunk$set(echo = TRUE)

# Packages
library(tidyverse) # message = FALSE hides the loading messages of a package
```

# Loading and preprocessing the data


```r
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
downl <- download.file(URL,destfile = "df")
df <- unzip("df")

ActivityData_df <- read.csv(df, header = TRUE)

ActivityData_df <- ActivityData_df %>%
  mutate(date = as.Date(date))

rm(df, downl, URL)
```

# What is mean total number of steps taken per day?  


```r
# Calculate the total steps taken per day
TotalSteps_df <- ActivityData_df %>%
  group_by(date) %>%
  summarize(steps_sum = sum(steps, na.rm = TRUE)) %>%
  ungroup()

qplot(TotalSteps_df$steps_sum,
      geom = "histogram",
      binwidth = 2000,
      main = "Histogram: Total No of Steps Taken / Day",
      xlab = "Total Steps",
      fill = I("seagreen"))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->



```r
# Calculate and report the mean and median of total steps taken per day
MeanSteps <- TotalSteps_df %>%
  summarize(mean(steps_sum, na.ram = TRUE))

MedianSteps <- TotalSteps_df %>%
  summarize(median(steps_sum, na.rm = TRUE))
```

The mean of total steps taken per day is 9354.2295082 and the median 10395.  

# What is the average daily activity pattern?


```r
# Make a time series plot of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)

MeanStepsByInterval_df <- ActivityData_df %>%
  group_by(interval) %>%
  summarise(MeanSteps = mean(steps, na.rm = TRUE)) %>%
  ungroup()

ggplot(MeanStepsByInterval_df, aes(interval, MeanSteps)) +
  geom_line(col = "seagreen") +
  labs(title = "Avg Number of Stepy by Interval") +
  labs(x = "Interval",
       y = "Avg No of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Imputing missing values


```r
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA.
NAVal <- sum(is.na(ActivityData_df))
```


There are 2304 observations with missing values in the dataset, making up about 13 percent of the dataset.


```r
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be
# sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# The strategy will use the mean as calculated before, as no sophisticated strategy is required :-)
missingSteps <- ActivityData_df %>%
  filter(is.na(steps))

missingSteps <- merge(missingSteps, MeanStepsByInterval_df, by = "interval")  %>%
  within(rm(steps)) %>%
  rename(steps = MeanSteps) # replacing NA w/ the calculated mean

# Create a new dataset that is equal to the original dataset but with the missing data filled in
ActivityData_df_NA_cleaned <- ActivityData_df %>%
  filter(!is.na(steps)) %>%
  rbind(missingSteps) # binding the impuded rows to a new df

# Make a histogram of the total number of steps taken each day 

TotalSteps_df_NA_cleaned <- ActivityData_df_NA_cleaned %>%
  group_by(date) %>%
  summarize(steps_sum = sum(steps, na.rm = TRUE)) %>%
  ungroup()

qplot(TotalSteps_df_NA_cleaned$steps_sum,
      geom = "histogram",
      binwidth = 2000,
      main = "Histogram: Total No of Steps Taken / Day (w/ imputed NA values)",
      xlab = "Total Steps",
      fill = I("seagreen"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
# and Calculate and report the mean and median total number of steps taken per day
# Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

MeanSteps_NA_cleaned <- TotalSteps_df_NA_cleaned %>%
  summarize(mean(steps_sum, na.ram = TRUE))

MedianSteps_NA_cleaned <- TotalSteps_df_NA_cleaned %>%
  summarize(median(steps_sum, na.rm = TRUE))
```

The average of steps taken on a given day is now at 1.0766189\times 10^{4} and the median at 1.0766189\times 10^{4}. Beforehand the mean was 9354.2295082 and the median at 10395. The mean was lower before, as I have removed NA values. Using the mean by interval when not including empty values, a relevant share has been attributed to high average total steps and therefore we see an increase in both, mean and median, values.

# Are there differences in activity patterns between weekdays and weekends?

I will continue working with the imputed data frame.


```r
library(lubridate)

MeanStepsByInterval_df <- ActivityData_df_NA_cleaned %>%
  mutate(WDAY = wday(date, label = TRUE, abbr = TRUE), # using lubridate to add day of the week in 3-letter code
         WDAY = as.factor(ifelse(str_detect(WDAY, "S"), "weekend", "weekday"))) %>% # creating a factor; if the 3-letter code contains S (SAT, SUN) for weekend
  group_by(interval, WDAY) %>%
  summarise(MeanSteps = mean(steps, na.rm = TRUE)) %>%
  ungroup() 

#ploting
ggplot(MeanStepsByInterval_df, aes(interval, MeanSteps)) +
  facet_grid(WDAY~.) +
  geom_line(col = "seagreen") +
  labs(title = "Average Steps per Day and Interval, Faceted by Type of Day") +
  labs(x = "Interval",
       y = "Averade Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

It appears, that, on weekdays, people tend to walk more in the earlier parts of the day whereas, throughout the day, people might accumulate more steps on weekends.

