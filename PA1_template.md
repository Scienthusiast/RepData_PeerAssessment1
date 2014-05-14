Reproducible research - Peer assessment number 1
========================================================
**Author : Timothée Behra**


```r
opts_chunk$set(echo = TRUE)
```


## Loading and preprocessing the data


```r
setwd("/Users/timothee/Documents/Serendipity/dataAnalysis/coursera reproducible research/assessment 1")  # Must be replaced with an adequate path -  That cannot be reproducible... what do ?
activity <- read.csv("activity.csv")
```


## What is the mean total number of steps taken per day?


```r
stepsByDay <- NULL
for (day in split(activity, activity$date)) {
    stepsByDay <- c(stepsByDay, sum(day$steps))
}
hist(stepsByDay, breaks = 8, main = "Histogram of number of steps by day", xlab = "Number of steps in a day")
```

![plot of chunk dailyStepNumber](figure/dailyStepNumber.png) 

```r
meanStepsByDay <- mean(stepsByDay, na.rm = T)
medianStepsByDay <- median(stepsByDay, na.rm = T)
```


The median number of steps in a day is 10765 steps, the mean number of steps in a day is 1.0766 &times; 10<sup>4</sup> steps.

## What is the average daily activity pattern?

The following graph presents the number of steps taken in each time interval of the day.



```r
stepsByInt <- NULL
for (interval in split(activity, activity$interval)) {
    stepsByInt <- c(stepsByInt, mean(interval$steps, na.rm = T))
}
intervals <- unique(activity$interval)
plot(intervals, stepsByInt, type = "l", main = "Number of steps by interval", 
    xlab = "Intervals", ylab = "Number of steps")
```

![plot of chunk dailyPattern](figure/dailyPattern.png) 

```r
maxStepsInt <- intervals[which(stepsByInt == max(stepsByInt))]
```


The time interval with the maximum mean number of steps in a day is the interval labeled 835.

## Imputing mssing values

This section deals with the missing values in the data.  

First, we compute the number of missing values :

```r
nbMissingIntervals <- sum(is.na(activity$steps))
nbMissingDays <- sum(is.na(stepsByDay))  ## Number of days with at least one missing value
```

There are 2304 missing values of the number of steps for a time interval, and there are 8 days with incomplete or no data.  

A copy of the dataset called activityNoNA is created, in which the NAs for the steps are replaced by the mean number of steps for that time interval in the whole dataset.


```r
activityNoNA <- activity
missingIntervals <- which(is.na(activity$steps))  # Index of all missing values
for (i in missingIntervals) {
    # We replace the NA's by the mean step number for that interval
    activityNoNA$steps[i] <- stepsByInt[which(intervals == activity$interval[i])]
}
stepsByDayNoNA <- NULL
for (day in split(activityNoNA, activityNoNA$date)) {
    stepsByDayNoNA <- c(stepsByDayNoNA, sum(day$steps))
}
hist(stepsByDayNoNA, breaks = 8, main = "Histogram of number of steps by day", 
    xlab = "Number of steps in a day")
```

![plot of chunk NAFilledIn](figure/NAFilledIn.png) 

```r
meanStepsByDayNoNA <- mean(stepsByDayNoNA, na.rm = T)
medianStepsByDayNoNA <- median(stepsByDayNoNA, na.rm = T)
```

Now, with the NAs filled in, the median number of steps in a day is 1.0766 &times; 10<sup>4</sup> steps, the mean number of steps in a day is 1.0766 &times; 10<sup>4</sup> steps.  

With the strategy used for imputing missing data, the mean number of steps doesn't change. The median, however, changes from 10765to 1.0766 &times; 10<sup>4</sup>, and could change quite dramatically as we input many times the exact same values for days with no data.

## Are there differences in activity patterns between weekdays and weekends ?

Here we first add a weekday factor in the data set, which tells whether a given day in the datawet is a day of the week or a day of the weekend. We then we plot the mean number of steps taken at each time interval for the two levels in the factor weekday. 


```r
Sys.setlocale("LC_TIME", "C")  # Necessary in order to get English names for weekdays
```

```
## [1] "C"
```

```r
weekday <- factor(weekdays(as.Date(activity$date)) %in% c("Saturday", "Sunday"), 
    labels = c("weekday", "weekend"))
activityNoNA <- cbind(activityNoNA, weekday)

weekendSteps <- NULL
weekdaySteps <- NULL
for (interval in split(activityNoNA, activityNoNA$interval)) {
    weekendSteps <- c(weekendSteps, mean(interval$steps[interval$weekday == 
        "weekend"]))
    weekdaySteps <- c(weekdaySteps, mean(interval$steps[interval$weekday == 
        "weekday"]))
}
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot(intervals, weekendSteps, type = "l", main = "Weekend", ylab = "Number of steps")
plot(intervals, weekdaySteps, type = "l", main = "Weekday", ylab = "Number of steps")
```

![plot of chunk weekDays](figure/weekDays.png) 


We can see that the daily patterns are different, and that the subject takes more steps during the week end.
