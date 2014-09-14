---
title: "Reproducible Research Peer Assessment 1"
output: html_document
---
##Loading and preprocessing the data

Show any code that is needed to
Load the data (i.e. read.csv())

```r
data<-read.csv("activity.csv")
data$date<-as.Date(data$date, "%Y-%m-%d")
```

##Process/transform the data (if necessary) into a format suitable for your analysis

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

Calculate and report the mean and median total number of steps taken per day

```r
dailystep<-tapply(data$steps,data$date, sum,na.rm=TRUE)
hist(dailystep)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
median(dailystep,na.rm=TRUE)
```

```
## [1] 10395
```

```r
mean(dailystep,na.rm=TRUE)
```

```
## [1] 9354
```

##What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data$stepsmean<- tapply(data$steps,data$interval,mean,na.rm=TRUE)
with(data[1:288,],plot(interval,stepsmean,type="l"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
datasample<-data[1:288,]
datasample[datasample$stepsmean==max(datasample$stepsmean),3]
```

```
## [1] 835
```
##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(data))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.
Here I will fill with mean of that 5-min interval

```r
data1<-data
## fill with mean of that 5-minutes interval
for (i in 1:17568) {
                if (is.na(data1[i,1])) {data1[i,1]<-data1[i,4]}
        }
```
Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
par(mfrow=c(1,2))
dailystep<-tapply(data$steps,data$date, sum,na.rm=TRUE)
dailystep1<-tapply(data1$steps,data1$date, sum,na.rm=TRUE)

hist(dailystep);hist(dailystep1)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

```r
print ("original median and mean")
```

```
## [1] "original median and mean"
```

```r
median(dailystep,na.rm=TRUE);mean(dailystep,na.rm=TRUE)
```

```
## [1] 10395
```

```
## [1] 9354
```

```r
print ("modified median and mean")
```

```
## [1] "modified median and mean"
```

```r
median(dailystep1,na.rm=TRUE);mean(dailystep1,na.rm=TRUE)
```

```
## [1] 10766
```

```
## [1] 10766
```

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
data1$weekday<-weekdays(data1$date)
data1$weekday1<-data1$weekday
for (i in 1:17568) {
        if (data1[i,5]%in% c("Saturday","Sunday"))
        data1[i,6]<- "weekend"
        else data1[i,6] <- "weekday"
}
data1$weekday1<-as.factor(data1$weekday1)
data1$interval<-sprintf("%04d",data1$interval)
data1$weekdayinterval <- paste(data1$interval,data1$weekday1,sep="")
data1$weekdayinterval<-as.factor(data1$weekdayinterval)
meanstep<- 1:17568
for (i in 1:17568){
        
        data1$meanstep[i]<-mean(data1[data1$weekdayinterval==data1[i,7],1])
        }
library(lattice)
data1$interval<-as.numeric(data1$interval)
data2<-rbind(data1[1:288,],data1[1729:2016,])
xyplot(meanstep~interval|weekday1, data=data2,type="l", layout= c(1,2),
       xlab="Interval",ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

