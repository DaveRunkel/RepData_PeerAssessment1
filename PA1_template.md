# RepResearch Proj 1
Dave!  
Friday, June 05, 2015  

Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data

The data for this assignment can be downloaded from the course web site:
.Dataset: Activity monitoring data [52K]

The variables included in this dataset are:

.steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)


.date: The date on which the measurement was taken in YYYY-MM-DD format


.interval: Identifier for the 5-minute interval in which measurement was taken


The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment

This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

Throughout your report make sure you always include the code that you used to generate the output you present. When writing code chunks in the R markdown document, always use echo = TRUE so that someone else will be able to read the code. This assignment will be evaluated via peer assessment so it is essential that your peer evaluators be able to review the code for your analysis.

For the plotting aspects of this assignment, feel free to use any plotting system in R (i.e., base, lattice, ggplot2)

Fork/clone the GitHub repository created for this assignment. You will submit this assignment by pushing your completed files into your forked repository on GitHub. The assignment submission will consist of the URL to your GitHub repository and the SHA-1 commit ID for your repository state.

NOTE: The GitHub repository also contains the dataset for the assignment so you do not have to download the data separately.

#Loading and preprocessing the data

##Show any code that is needed to

1.Load the data 


2.Process/transform the data (if necessary) into a format suitable for your analysis



```r
#brings in appropriate libraries
library(knitr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library (lattice)
#Loading and Processiong Data

activities<-read.csv("activity.csv")
```

What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.



1.Calculate the total number of steps taken per day



```r
##reads activities table and pulls out steps/day
dailysteps <- aggregate(activities$steps, list(Day=activities$date), FUN=sum,na.rm=TRUE)
names(dailysteps)<- c("date","Total_Steps")
```

2. Make a histogram of the total number of steps taken each day


```r
#creates histogram of daily steps
hist(dailysteps$Total_Steps, col = "purple", breaks=50,xlab="Total Steps per Day", ylab="Daily step Frequency",
     main ="Daily Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day


```r
#Reports mean and Median steps/dday
meanstd <-as.data.frame(rbind(c("MeanSteps",round(sum(mean(dailysteps$Total_Steps)))),
                c("MedianSteps",sum(median(dailysteps$Total_Steps))),
                c("TotalSteps",sum(dailysteps$Total_Steps))))

colnames(meanstd) <- c("Descriptor", "Tally")
print(meanstd)
```

```
##    Descriptor  Tally
## 1   MeanSteps   9354
## 2 MedianSteps  10395
## 3  TotalSteps 570608
```

#What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
#computes the average step by interval
int <- aggregate(activities$steps, list(x=activities$interval)
                 , FUN=mean, na.rm=TRUE)
names(int) <- c("interval", "steps")
#plots the time average steps
plot(int[,1],int$steps, type = "l",col="pink", xlab="Time [5 minutes increments]",
     ylab="Steps")
title("Average Steps in 5 minute increments")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
#calculates the maximum step and time
maxint <- which.max(int$steps)
maxmaxint <- int[maxint,]
print(paste("Max Step @ Time:",maxmaxint[,1]))
```

```
## [1] "Max Step @ Time: 835"
```

#Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
#calculates the total number of NAs for each column
stepna <- c("NAs in Step",sum(is.na(activities$steps)))
intna <- c("NAs in Interval",sum(is.na(activities$interval)))
datena <- c("NAs in Date", sum(is.na(activities$date)))
NAS<- as.data.frame(rbind(stepna,intna,datena))
colnames(NAS)<- c("counter", "NUmber of NAs")
#displays counts
print(NAS)
```

```
##                counter NUmber of NAs
## stepna     NAs in Step          2304
## intna  NAs in Interval             0
## datena     NAs in Date             0
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
method<-("Using the mean steps")
```

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
#merges the original activities table with the average by interval table. 
#X= origible table y= average table
room.for.activities<-merge(activities, int, by="interval")
#runs through removing NAs in original data with average step count
for (i in 1:nrow(room.for.activities)){
  if (is.na(room.for.activities$steps.x[i])){
    room.for.activities$steps.x[i] <-room.for.activities$steps.y[i]
  }
}
#removes average data column
room.for.activities$steps.y <- NULL
#renames columns
names(room.for.activities) <- c("interval", "steps.RFA", "date")
head(room.for.activities)
```

```
##   interval steps.RFA       date
## 1        0  1.716981 2012-10-01
## 2        0  0.000000 2012-11-23
## 3        0  0.000000 2012-10-28
## 4        0  0.000000 2012-11-06
## 5        0  0.000000 2012-11-24
## 6        0  0.000000 2012-11-15
```

```r
colSums(is.na(room.for.activities))
```

```
##  interval steps.RFA      date 
##         0         0         0
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
#calculates total daily steps from imputed data
total.RFA <- aggregate(room.for.activities$steps, list(Day=room.for.activities$date), FUN= sum)
names(total.RFA)<- c("date","Total_Steps")
head(total.RFA)
```

```
##         date Total_Steps
## 1 2012-10-01    10766.19
## 2 2012-10-02      126.00
## 3 2012-10-03    11352.00
## 4 2012-10-04    12116.00
## 5 2012-10-05    13294.00
## 6 2012-10-06    15420.00
```



```r
#creates histogram of imputed total number of steps
hist(total.RFA$Total_Steps, col = "red",breaks=50,xlab="Total imputed Steps per Day", ylab="Daily step Frequency",
     main ="Daily imputed Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


```r
#Reports mean, Median &total steps/dday for Imputed data
meanstd.RFA<-as.data.frame(rbind(c("MeanSteps",round(sum(mean(total.RFA$Total_Steps)))),
                c("MedianSteps",round(sum(median(total.RFA$Total_Steps)))),
                c("TotalSteps",round(sum(total.RFA$Total_Steps)))))

colnames(meanstd.RFA) <- c("Descriptor", "Tally")
head(meanstd.RFA)
```

```
##    Descriptor  Tally
## 1   MeanSteps  10766
## 2 MedianSteps  10766
## 3  TotalSteps 656738
```

```r
#Combines mean, median and totals for imputed and original data
delta <- cbind(meanstd.RFA, meanstd[,2])
names(delta)<-c("Descriptor","imputed","original")
head(delta)
```

```
##    Descriptor imputed original
## 1   MeanSteps   10766     9354
## 2 MedianSteps   10766    10395
## 3  TotalSteps  656738   570608
```

```
## [1] "As seen by the above table, the imputation did alter the data"
```

```r
#determines change (delta) between imputation and original data
percdelta <-100*((as.numeric(levels(delta$original))[delta$original]-
                    as.numeric(levels(delta$imputed))[delta$imputed])/
                   ((as.numeric(levels(delta$original))[delta$original])))
delta <- cbind(delta, percdelta)
head(delta)
```

```
##    Descriptor imputed original  percdelta
## 1   MeanSteps   10766     9354 -15.095146
## 2 MedianSteps   10766    10395  -3.569024
## 3  TotalSteps  656738   570608 -15.094426
```

#Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values (imputed) for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
#creates new data for weekday/weekend analysis
day.id <- weekdays(as.Date(room.for.activities$date,abbreviate=TRUE))
jazzercise<-mutate(room.for.activities,day.id)
#correlates day names to general day identifiers
day.corr<- as.data.frame(cbind( 
  day.id=c("Monday","Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"), 
  general.id=c("Weekday","Weekday","Weekday","Weekday","Weekday","Weekend","Weekend")))
#merges into one data set with day names, interval, steps, date and general day identifiers
jazzercise <- merge(jazzercise, day.corr, by.x="day.id")
print(str(jazzercise))
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ day.id    : chr  "Friday" "Friday" "Friday" "Friday" ...
##  $ interval  : int  535 1755 915 805 525 1505 1805 645 1410 1925 ...
##  $ steps.RFA : num  6.06 0 0 68.21 2.96 ...
##  $ date      : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 61 54 47 40 61 40 54 33 19 26 ...
##  $ general.id: Factor w/ 2 levels "Weekday","Weekend": 1 1 1 1 1 1 1 1 1 1 ...
## NULL
```

```r
head(jazzercise)
```

```
##   day.id interval steps.RFA       date general.id
## 1 Friday      535  6.056604 2012-11-30    Weekday
## 2 Friday     1755  0.000000 2012-11-23    Weekday
## 3 Friday      915  0.000000 2012-11-16    Weekday
## 4 Friday      805 68.207547 2012-11-09    Weekday
## 5 Friday      525  2.962264 2012-11-30    Weekday
## 6 Friday     1505 36.075472 2012-11-09    Weekday
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
#computes the average step by interval
int.deaux <- aggregate(jazzercise$steps, 
                      list(time=jazzercise$interval,End.or.Days =jazzercise$general.id ),
                      FUN= mean)
names(int.deaux) <- c("Time","End.or.Days", "steps")
#plots the time average steps, now in panels!
xyplot(int.deaux$steps ~int.deaux$Time |int.deaux$End.or.Days,type ="l", col="orange", 
       xlab ="5 Minute Intervals", ylab ="Average steps accross all weekdays/ends", 
       layout =c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 
