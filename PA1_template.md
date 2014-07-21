---
output: html_document
---
# Reproducible Research: Peer Assessment 1
============================================================================================
Setting the environment

```r
knitr::opts_chunk$set(echo=TRUE)
setwd("D:/Trainings/ReproducibleResearch/RepData_PeerAssessment1")
```
## Loading and preprocessing the data


```r
unzip("activity.zip")
activity<-read.csv("activity.csv", stringsAsFactors = FALSE)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
#change the date which is a string variable now, to a date type
activity$date<-as.Date(activity$date, format = '%Y-%m-%d')
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

```r
# aggrigating by date
total_daily_steps <- aggregate(steps~date, activity,sum, na.rm=TRUE)
# 1. Make a histogram of the total number of steps taken each day
histogram<-barplot(total_daily_steps$steps, names.arg =total_daily_steps$date, xlab = "Date",ylab="Total Daily Steps", main="Number of Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# 2. Calculate and report the mean and median total number of steps taken per day
daily_mean_steps<- mean(total_daily_steps$steps,na.rm=TRUE)
daily_median_steps<-median(total_daily_steps$steps,na.rm=TRUE)
```
The daily **mean** steps is 10766 and the **median** is 10765.

## What is the average daily activity pattern?


```r
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_interval <- aggregate(steps ~ interval, activity, mean)

plot(average_interval, type = "l", xlab="Intervals", ylab="Average Steps per interval", main="Average steps per interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_interval$interval[which.max(average_interval$steps)]
```

```
## [1] 835
```

## Imputing missing values


```r
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(!complete.cases(activity))
```

```
## [1] 2304
```

```r
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    #I prefer to use mean to fill the missing value

#3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity <- merge(activity,average_interval, by = "interval", suffixes = c("", 
    ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]

#4. Make a histogram of the total number of steps taken each day.
total_steps_per_day <- aggregate(steps ~ date,activity,sum)
barplot(total_steps_per_day$steps, names.arg = total_steps_per_day$date, xlab = "Date", ylab = "Total number of Steps",main="Total Steps per Day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
#Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#Mean
mean(total_steps_per_day$steps)
```

```
## [1] 10766
```

```r
#Median
median(total_steps_per_day$steps)
```

```
## [1] 10766
```
As one can clearly see from the above figures the **mean** and **median** are equal.

## Are there differences in activity patterns between weekdays and weekends?
1.Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
activity$dayType <- ifelse(weekdays(activity$date) %in%  c("Saturday", "Sunday"),'weekend','weekday')

head(activity)
```

```
##   interval steps       date dayType
## 1        0 1.717 2012-10-01 weekday
## 2        0 0.000 2012-11-23 weekday
## 3        0 0.000 2012-10-28 weekend
## 4        0 0.000 2012-11-06 weekday
## 5        0 0.000 2012-11-24 weekend
## 6        0 0.000 2012-11-15 weekday
```

```r
table(activity$dayType)
```

```
## 
## weekday weekend 
##   12960    4608
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
qplot(x=interval, y=steps,data=subset(activity, complete.cases(activity)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~dayType,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

