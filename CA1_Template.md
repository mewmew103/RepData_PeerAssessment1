---
title: "A Report on various results observed from the Activity Monitoring Dataset of an individual"
author: "R Vidhya Lakshmi"
date: "24/04/2020"
output: html_document
fig_width: 6
fig_height: 4
---
## Loading and pre-processing the data  
### Code to download and read the Activity Monitoring dataset from Course website using [URL](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip/)  
  
  ```r
  temp.file<-tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", temp.file)
  rep_data<-read.csv(unzip(temp.file, exdir = "rep_data"))
  head(rep_data)
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
  
  ```r
  library(lattice)
  ```

## What is the mean total number of steps taken per day?
### Mean and median of number of steps taken everyday
  
  ```r
  Steps_Cumulative<-aggregate(steps~date,rep_data,sum, na.rm=TRUE)
  hist(Steps_Cumulative$steps,xlab = "No of steps taken everyday", main="Frequency of total number of steps taken everyday", col="blue")
  ```
  
  ![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)
  
  ```r
  mean(Steps_Cumulative$steps, na.rm=TRUE)
  ```
  
  ```
  ## [1] 10766.19
  ```
  
  ```r
  median(Steps_Cumulative$steps, na.rm = TRUE)
  ```
  
  ```
  ## [1] 10765
  ```

## What is the average daily activity pattern?
### Time-series plot for average number of steps taken everyday
  
  ```r
  Steps_timeseries<-tapply(rep_data$steps,rep_data$interval, mean,na.rm = TRUE)
  plot(row.names(Steps_timeseries),Steps_timeseries,type="l", col="blue",main="Time-series plot for average number of steps taken everyday", xlab = "5 min intervals", ylab = "Average no of steps")
  ```
  
  ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
### Interval with the maximum number of steps

  
  ```r
  names(which.max(Steps_timeseries))
  ```
  
  ```
  ## [1] "835"
  ```

## Imputing missing values
### Calculating number of missing values and replacing them with the corresponding mean
  
  ```r
  Total.No.of.Nas<-sum(is.na(rep_data))
  Total.No.of.Nas
  ```
  
  ```
  ## [1] 2304
  ```
### A dataset  containing a new column with the average mean for each entry for variable "Steps" is created 
  
  ```r
  tmp<-merge(rep_data,aggregate(steps~interval, rep_data, mean, na.rm=TRUE), by=c("interval"))
  tmp[is.na(tmp$steps.x),"steps.x"]<-tmp[is.na(tmp$steps.x),"steps.y"]
  names(tmp)[names(tmp)=="steps.x"]<-"steps"
  ```
### New Dataset with missing values filled in and its histogram for total number of steps 
  
  ```r
  rep_data_without_nas<-tmp[,c("steps","date","interval")]
  Steps_Without_Nas_Cumulative<-aggregate(steps~date,rep_data_without_nas,sum)
  hist(Steps_Without_Nas_Cumulative$steps,main="Total no of steps taken everyday", xlab = "Steps")
  ```
  
  ![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
### Mean of the new dataset is same as the previous one for rep_data. There is a slight change in median value.

  
  ```r
  mean(Steps_Without_Nas_Cumulative$steps)
  ```
  
  ```
  ## [1] 10766.19
  ```
  
  ```r
  median(Steps_Without_Nas_Cumulative$steps)
  ```
  
  ```
  ## [1] 10766.19
  ```

## Are there differences in activity patterns between weekdays and weekends?
### Creating a New factor Variable with type weekend and weekday
  
  ```r
  rep_data_without_nas$Day<-weekdays(as.Date(rep_data_without_nas$date, "%Y-%m-%d"))
  rep_data_without_nas$type<-ifelse(rep_data_without_nas$Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  head(rep_data_without_nas)
  ```
  
  ```
  ##      steps       date interval      Day    type
  ## 1 1.716981 2012-10-01        0   Monday Weekday
  ## 2 0.000000 2012-11-23        0   Friday Weekday
  ## 3 0.000000 2012-10-28        0   Sunday Weekend
  ## 4 0.000000 2012-11-06        0  Tuesday Weekday
  ## 5 0.000000 2012-11-24        0 Saturday Weekend
  ## 6 0.000000 2012-11-15        0 Thursday Weekday
  ```
### Observing differences in activity pattern across weekends and weekdays with a time series plot
### From the below plot, it is evident that the no of steps is more on weekends than on weekdays.
  
  ```r
  Steps_Cumulative_by_Type<-aggregate(steps~interval+type, rep_data_without_nas,mean)
  par(mfrow=c(1,2))
  xyplot(Steps_Cumulative_by_Type$steps~Steps_Cumulative_by_Type$interval|Steps_Cumulative_by_Type$type,col="Red", type="l",xlab = "interval",ylab = "No of Steps", layout=c(1,2))
  ```
  
  ![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)
