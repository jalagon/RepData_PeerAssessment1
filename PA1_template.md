---
title: "Reproducible Research: Peer Assessment 1"
author: "jalagon"
output: 
html_document:
keep_md: yes
---


## Loading and preprocessing the data

1. Load the data (i.e. `read.csv()`)

First, download the data from internet.


```r
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url = URL, destfile = "activity.zip")
unzip("activity.zip")
```

Then, read data in R.


```r
# Read data. I assume you have downloaded zip data and extracted *activity.csv*  to working directory.
activity <- read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis.  

Change date variable to date format and check everything looks ok

```r
# Change date variable to date format
activity$date<-as.Date.factor(activity$date)

#Create time and dateTime variables from date and interval variables.
hour <- as.integer(activity$interval/100)
minute <- as.integer(round((activity$interval/100 - hour) * 100))
time <- strptime(paste(hour,":",minute, sep = ""), format = "%H:%M")
dateTime <- strptime(paste(activity$date," ",hour,":",minute, sep = ""), format = "%Y-%m-%d %H:%M")

# Explore data frame
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
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

```r
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
library(plyr)
library(dplyr)
# Calculate total number of steps per day
StepsPerDay <- summarise(group_by(activity, date), sum(steps))
StepsPerDay
```

```
## Source: local data frame [61 x 2]
## 
##          date sum(steps)
##        (date)      (int)
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## ..        ...        ...
```

2. Make a histogram of the total number of steps taken each day

```r
hist(StepsPerDay$`sum(steps)`,
     main = "Total number of steps per day",
     xlab = "Steps per day")
```

![plot of chunk Histogram](figure/Histogram-1.png) 


3. Calculate and report the mean and median of the total number of steps taken per day

```r
# Calculate mean and median across days, ignoring NA
meanSteps <- mean(StepsPerDay$`sum(steps)`, na.rm = TRUE)
medianSteps <- median(StepsPerDay$`sum(steps)`, na.rm = TRUE)
```

The mean and median steps taken per day are **1.0766189 &times; 10<sup>4</sup>** and **10765**, respectively.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
# Calculate Average Steps per Interval
AverageStepsPerInterval <- summarise(group_by(activity, interval), mean(steps, na.rm = TRUE))
plot(unique(time), AverageStepsPerInterval$`mean(steps, na.rm = TRUE)`, 
     type = "l",
     main = "Time series average steps by 5-minute interval",
     xlab = "5-minute interval",
     ylab = "Average Steps")
```

![plot of chunk Time Series](figure/Time Series-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# Find the max and extract the interval
x <- AverageStepsPerInterval[AverageStepsPerInterval[, 2] == max(AverageStepsPerInterval[, 2]), ]
maxInt <- paste(as.integer(x[1] / 100), ":", 
                as.integer(round((x[1] / 100 - as.integer(x[1] / 100)) * 100)), 
                sep = "")
maxSteps <- round(x[2])
```

The interval **8:35** has the max average number of steps, which is **206**.

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
#Calculate number of rows with NAs
numbMissing <- dim(activity)[1] - sum(complete.cases(activity))
percMissing <- round(numbMissing / dim(activity)[1] * 100)
```

There are **2304** cases with missing values, that is **13%**.

2. Devise a strategy for filling in all of the missing values in the dataset. 

My strategy will be **to replace missing values by the mean for that 5-minute interval**.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
# Used approach from http://www.mail-archive.com/r-help@r-project.org/msg58289.html
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activityComplete <- ddply(activity, ~ interval, transform, steps = impute.mean(steps))
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Calculate total number of steps per day
StepsPerDayComplete <- summarise(group_by(activityComplete, date), sum(steps))
# Generate Histogram
hist(StepsPerDayComplete$`sum(steps)`,
     main = "Total number of steps per day",
     xlab = "Steps per day (imputed NAs)")
```

![plot of chunk calculate](figure/calculate-1.png) 

```r
# Calculate mean and median across days
meanStepsComplete <- round(mean(StepsPerDayComplete$`sum(steps)`), digits = 2)
medianStepsComplete <- round(median(StepsPerDayComplete$`sum(steps)`), digits = 2)
```

The mean and median steps taken per day are both **1.076619 &times; 10<sup>4</sup>**. 

Therefore, the impact of imputing missing data was **negligible**.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels â<U+0080><U+0093> â<U+0080><U+009C>weekdayâ<U+0080> and â<U+0080><U+009C>weekendâ<U+0080> indicating whether a given date is a weekday or weekend day.


```r
as.factor(weekdays(activityComplete$date, abbreviate = TRUE))
```

```
##     [1] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##    [18] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##    [35] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##    [52] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##    [69] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##    [86] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##   [103] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##   [120] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [137] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##   [154] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##   [171] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##   [188] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [205] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##   [222] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##   [239] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##   [256] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [273] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##   [290] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##   [307] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [324] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [341] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##   [358] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##   [375] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [392] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [409] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##   [426] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [443] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [460] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [477] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##   [494] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [511] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [528] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##   [545] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##   [562] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [579] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [596] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##   [613] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [630] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [647] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [664] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##   [681] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [698] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [715] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##   [732] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##   [749] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [766] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [783] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##   [800] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##   [817] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [834] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##   [851] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##   [868] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##   [885] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [902] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##   [919] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##   [936] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##   [953] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##   [970] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##   [987] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1004] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1021] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1038] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1055] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1072] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1089] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [1106] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1123] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1140] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1157] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1174] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1191] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1208] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [1225] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1242] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1259] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1276] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [1293] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1310] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1327] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [1344] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1361] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1378] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1395] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [1412] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1429] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1446] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [1463] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1480] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1497] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1514] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [1531] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1548] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1565] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [1582] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1599] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1616] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1633] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [1650] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1667] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1684] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1701] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [1718] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1735] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1752] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [1769] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1786] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1803] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1820] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [1837] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1854] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1871] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [1888] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1905] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1922] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [1939] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [1956] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [1973] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [1990] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2007] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [2024] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2041] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2058] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2075] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2092] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2109] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2126] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [2143] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2160] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2177] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2194] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2211] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2228] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2245] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [2262] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2279] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2296] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2313] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [2330] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2347] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2364] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [2381] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2398] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2415] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2432] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [2449] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2466] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2483] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [2500] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2517] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2534] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2551] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [2568] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2585] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2602] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [2619] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2636] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2653] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2670] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [2687] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2704] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2721] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2738] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [2755] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2772] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2789] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [2806] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2823] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2840] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2857] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [2874] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2891] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2908] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [2925] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [2942] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [2959] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [2976] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [2993] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3010] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3027] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3044] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [3061] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3078] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3095] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3112] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3129] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3146] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3163] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [3180] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3197] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3214] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3231] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3248] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3265] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3282] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [3299] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3316] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3333] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3350] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [3367] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3384] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3401] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [3418] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3435] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3452] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3469] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [3486] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3503] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3520] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [3537] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3554] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3571] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3588] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [3605] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3622] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3639] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [3656] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3673] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3690] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3707] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [3724] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3741] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3758] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3775] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [3792] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3809] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3826] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [3843] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3860] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3877] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3894] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [3911] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3928] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [3945] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [3962] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [3979] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [3996] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4013] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [4030] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4047] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4064] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4081] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [4098] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4115] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4132] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4149] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4166] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4183] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4200] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [4217] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4234] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4251] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4268] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4285] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4302] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4319] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [4336] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4353] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4370] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [4387] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [4404] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4421] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4438] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [4455] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4472] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4489] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4506] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [4523] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4540] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4557] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [4574] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4591] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4608] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4625] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [4642] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4659] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4676] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [4693] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4710] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4727] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4744] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [4761] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4778] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4795] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4812] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [4829] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4846] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4863] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [4880] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4897] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4914] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4931] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [4948] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [4965] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [4982] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [4999] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5016] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5033] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5050] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [5067] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5084] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5101] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5118] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [5135] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5152] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5169] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5186] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5203] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5220] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5237] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [5254] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5271] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5288] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5305] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5322] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5339] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5356] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [5373] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5390] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5407] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [5424] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [5441] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5458] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5475] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [5492] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5509] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5526] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5543] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [5560] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5577] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5594] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [5611] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5628] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5645] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5662] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [5679] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5696] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5713] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [5730] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5747] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5764] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5781] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [5798] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5815] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5832] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5849] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [5866] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5883] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5900] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [5917] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [5934] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [5951] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [5968] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [5985] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6002] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6019] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6036] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6053] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6070] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6087] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [6104] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6121] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6138] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6155] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [6172] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6189] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6206] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6223] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6240] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6257] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6274] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [6291] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6308] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6325] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6342] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6359] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6376] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6393] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [6410] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6427] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6444] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [6461] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [6478] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6495] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6512] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [6529] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6546] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6563] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6580] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [6597] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6614] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6631] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [6648] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6665] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6682] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6699] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [6716] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6733] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6750] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [6767] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6784] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6801] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6818] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [6835] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6852] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6869] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6886] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [6903] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6920] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [6937] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [6954] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [6971] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [6988] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7005] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [7022] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7039] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7056] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7073] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7090] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7107] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7124] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [7141] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7158] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7175] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7192] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [7209] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7226] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7243] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7260] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7277] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7294] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7311] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [7328] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7345] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7362] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [7379] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7396] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7413] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7430] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [7447] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7464] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7481] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [7498] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [7515] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7532] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7549] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [7566] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7583] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7600] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7617] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [7634] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7651] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7668] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [7685] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7702] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7719] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7736] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [7753] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7770] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7787] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [7804] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7821] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7838] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7855] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [7872] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7889] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7906] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7923] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [7940] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [7957] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [7974] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [7991] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8008] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8025] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8042] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [8059] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8076] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8093] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8110] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8127] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8144] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8161] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [8178] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8195] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8212] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8229] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [8246] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8263] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8280] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8297] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8314] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8331] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8348] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [8365] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8382] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8399] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [8416] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8433] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8450] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8467] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [8484] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8501] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8518] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [8535] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [8552] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8569] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8586] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [8603] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8620] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8637] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8654] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [8671] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8688] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8705] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [8722] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8739] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8756] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8773] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [8790] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8807] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8824] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [8841] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8858] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8875] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8892] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [8909] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8926] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [8943] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [8960] vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar
##  [8977] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [8994] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9011] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9028] vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9045] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9062] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9079] mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb
##  [9096] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9113] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9130] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9147] mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9164] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9181] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9198] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié
##  [9215] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9232] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9249] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9266] sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié
##  [9283] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9300] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9317] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9334] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9351] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9368] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9385] mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom
##  [9402] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9419] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9436] dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar
##  [9453] mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9470] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9487] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9504] dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue
##  [9521] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9538] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9555] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb
##  [9572] dom lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue
##  [9589] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9606] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9623] jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun
##  [9640] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9657] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9674] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9691] jue vie sáb dom lun mar mié jue vie lun mar mié jue vie sáb dom lun
##  [9708] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9725] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9742] lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié
##  [9759] jue vie lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9776] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9793] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9810] lun mar mié jue vie sáb dom lun mar mié jue vie lun mar mié jue vie
##  [9827] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9844] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9861] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom
##  [9878] lun mar mié jue vie lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9895] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9912] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9929] vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie lun mar
##  [9946] mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie
##  [9963] sáb dom lun mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun
##  [9980] mar mié jue vie sáb dom lun mar mié jue vie sáb dom lun mar mié jue
##  [9997] vie sáb dom lun
##  [ reached getOption("max.print") -- omitted 7568 entries ]
## Levels: dom jue lun mar mié sáb vie
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

