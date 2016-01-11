---
title: "PA1_template"
output: html_document
---

##Loading and preprocessing the data
```{r}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip","./Step_data.zip", mode = "wb")
unzip("Step_data.zip")
activity <- read.csv("./activity.csv", na.strings = "NA")
activity$date<-as.Date(activity$date)
activity_<-subset(activity, !is.na(steps))
```

##What is mean total number of steps taken per day?
```{r}
library(ggplot2)
steps_pd<-aggregate(steps ~ date,activity_, sum)
ggplot(steps_pd, aes(steps, fill=..count..))+geom_histogram(binwidth = 5000)+ xlim(c(0,25000))+
  labs(title="Histogram for steps")

mean(steps_pd$steps)
median(steps_pd$steps)
```
The mean is 10766.19 and the median is 10765


##What is the average daily activity pattern?
```{r}
steps_int<-aggregate(steps ~ interval,activity_, mean)
ggplot(steps_int, aes(interval, steps))+geom_line(size=1)+labs(title="Average steps for each interval")

steps_int$interval[steps_int$steps==max(steps_int$steps)]

```
 The maximum number of steps is in the interval 835

##Imputing missing values
```{r}
activity_na<-subset(activity, is.na(steps))
nrow(activity_na)
```
There are 2305 missing values

```{r}
activity_inp<-activity
names(steps_int)[2]<-paste("steps_y")

library(plyr)
merge_data<-join(activity_inp[is.na(activity_inp$steps),], steps_int, by="interval")

activity_inp$steps[is.na(activity_inp$steps)]<-merge_data$steps_y

steps_pd_inp<-aggregate(steps ~ date,activity_inp, sum)

ggplot(steps_pd_inp, aes(steps, fill=..count..))+geom_histogram(binwidth = 5000)+ xlim(c(0,25000))+
  labs(title="Histogram for steps input missing values")

mean(steps_pd_inp$steps)
median(steps_pd_inp$steps)

mean(steps_pd_inp$steps)- mean(steps_pd$steps)
median(steps_pd_inp$steps)-median(steps_pd$steps)

```
The mean in the imputing missing data is 10766.19 and the median is 10766.19
The difference between the first data and the imputing missing data are just in the median wich decrease 1.2

##Are there differences in activity patterns between weekdays and weekends?
```{r}
wks <- c("sábado","domingo")
activity_inp$days<-as.factor(ifelse(is.element(weekdays(steps_pd_inp$date), wks),"Weekend", "Weekday"))

steps_int_wd<-aggregate(steps ~ interval + days, activity_inp, mean)

library(lattice)

xyplot(steps ~ interval| factor(days), data=steps_int_wd, layout=c(1,2), type="l")

```

