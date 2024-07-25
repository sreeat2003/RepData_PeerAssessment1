---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
fullData <- read.csv("C:\\Users\\HP\\Downloads\\repdata_data_activity\\activity.csv")
fullData$date <- as.Date(fullData$date, "%Y-%m-%d")

## What is mean total number of steps taken per day?
stepsPerDay <- aggregate(steps ~ date, fullData, FUN = sum)
g <- ggplot (stepsPerDay, aes (x = steps))
g + geom_histogram(fill = "yellow", binwidth = 1000) +
  labs(title = " Histogram of Steps Taken Each Day ", x = "Steps", y = "Frequency")
stepsMean <- mean(stepsPerDay$steps, na.rm=TRUE)
stepsMean
stepsMedian <- median(stepsPerDay$steps, na.rm=TRUE)
stepsMedian

## What is the average daily activity pattern?
stepsPerInterval <- aggregate(steps ~ interval, fullData, mean)
h <- ggplot (stepsPerInterval, aes(x=interval, y=steps))
h + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval", x = "Interval", y = "Average Steps across All Days")
maxInterval <- stepsPerInterval[which.max(stepsPerInterval$steps), ] 
maxInterval

## Imputing missing values
noMissingValue <- nrow(fullData[is.na(fullData$steps),])
noMissingValue
fullData1 <- read.csv("C:\\Users\\HP\\Downloads\\repdata_data_activity\\activity.csv", header=TRUE,sep=",")
fullData1$day <- weekdays(as.Date(fullData1$date))
stepsAvg1 <- aggregate(steps ~ interval + day, fullData1, mean)
nadata <- fullData1 [is.na(fullData1$steps),]
newdata1 <- merge(nadata, stepsAvg1, by=c("interval", "day"))
cleanData <- fullData1 [!is.na(fullData1$steps),]
newdata2 <- newdata1[,c(5,4,1,2)]
colnames(newdata2) <- c("steps", "date", "interval", "day")
mergeData <- rbind (cleanData, newdata2)
stepsPerDayFill <- aggregate(steps ~ date, mergeData, FUN = sum)
g1 <- ggplot (stepsPerDayFill, aes (x = steps))
g1 + geom_histogram(fill = "green", binwidth = 1000) +
  labs(title = " Histogram of Steps Taken Each Day ", x = "Steps", y = "Frequency")
stepsMeanFill <- mean(stepsPerDayFill$steps, na.rm=TRUE)
stepsMeanFill
stepsMedianFill <- median(stepsPerDayFill$steps, na.rm=TRUE)
stepsMedianFill

## Are there differences in activity patterns between weekdays and weekends?
mergeData$DayType <- ifelse(mergeData$day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
stepsPerIntervalDT <- aggregate(steps ~ interval+DayType, mergeData, FUN = mean)
j <- ggplot (stepsPerIntervalDT, aes(x=interval, y=steps))
j + geom_line()+ labs(title = " Time Series Plot of Average Steps per Interval: weekdays vs. weekends", x = "Interval", y = "Average Number of Steps") + facet_grid(DayType ~ .)
