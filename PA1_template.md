---
title: "PA1_template"
author: "Robinsson Deantonio"
date: "15/5/2020"
output: html_document
keep_md: true
---

# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
  unzip(zipfile="./repdata_data_activity.zip")
activityData <- read.csv("./activity.csv")
```

## What is mean total number of steps taken per day?

```r
library(ggplot2)
activityData$date <- as.Date(activityData$date,"%Y-%m-%d")
Totsteps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(Totsteps, binwidth=1000, xlab="Number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](fig/plot1.png)
```r
mean(Totsteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(Totsteps, na.rm=TRUE)
```

```
## [1] 10395
```





## What is the average daily activity pattern?

```r
avg <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=avg, aes(x=interval, y=steps)) + geom_line(color="red") +
  xlab("Interval (5 min)") + ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-1](fig/plot2.png)

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?

```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
missing <- is.na(activityData$steps)
# How many missing
table(missing)
```

```
## missing
## FALSE  TRUE 
## 15264  2304
```

All of the missing values are filled in with mean value for that 5-minute
interval.


```r
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (avg[avg$interval==interval, "steps"])
    return(filled)
}
filled.data <- activityData
filled.data$steps <- mapply(fill.value, filled.data$steps,
                            filled.data$interval)
```
Now, using the filled data set, let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](fig/plot3.png)

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

The original data, there are some days with `steps` values `NA` for  any `interval`. The total number of steps taken in such days are set to 0s by
default.  
Mean and median values are higher after imputing missing data.  However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value.


## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
week.dayORend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))
        return("weekday")
    else if (day %in% c("sábado", "domingo"))
        return("weekend")
    else
        return("invalidDate")
}
filled.data$date <- as.Date(filled.data$date,"%Y-%m-%d")
filled.data$day <- lapply(filled.data$date, FUN = week.dayORend)
filled.data$day <- as.character(filled.data$day)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
avgs <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(avgs, aes(interval, steps, color = day)) + geom_line()  +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-1](fig/plot4.png)
