library(knitr)
 opts_chunk$set(echo = TRUE)
library(dplyr)
library(lubridate)
library(ggplot2)
 
## Read data
 setwd("C:/Users/ADMIN/Desktop/Data_Analytics/Reproducible Research")
 data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character",
                                                                           +                                                                           "integer"))
 
 ## Cleaning the data
 data$date <- ymd(data$date)
 
 ## For average steps without NA
 steps <- data %>%
   filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarize(steps = sum(steps)) %>%
   print
 ggplot(steps, aes(x = steps)) +
   geom_histogram(fill = "firebrick", binwidth = 1000) +
   labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")
 
 mean_steps <- mean(steps$steps, na.rm = TRUE)
 mean_steps
 median_steps <- median(steps$steps, na.rm = TRUE)
 median_steps
 
 ## For average daily activity:
 interval <- data %>%
   filter(!is.na(steps)) %>%
   group_by(interval) %>%
   summarize(steps = mean(steps))
 ggplot(interval, aes(x=interval, y=steps)) +
   geom_line(color = "firebrick")
 interval[which.max(interval$steps),]
 
 ## With missing values
 data_full <- data
 nas <- is.na(data_full$steps)
 avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
 data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]
 
 steps_full <- data_full %>%
   filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarize(steps = sum(steps)) %>%
   print
 
 ggplot(steps_full, aes(x = steps)) +
   geom_histogram(fill = "firebrick", binwidth = 1000) +
   labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")
 
 mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
 mean_steps_full
 median_steps_full <- median(steps_full$steps, na.rm = TRUE)
 median_steps_full
 
 ## Activity Patterns between week and weekends:
 data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
 data_full$weektype <- as.factor(data_full$weektype)
 head(data_full)
 interval_full <- data_full %>%
   group_by(interval, weektype) %>%
   summarise(steps = mean(steps))
 s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
   geom_line() +
   facet_wrap(~weektype, ncol = 1, nrow=2)
 print(s)
 