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

1. Create a new factor variable in the dataset with two levels �<U+0080><U+0093> �<U+0080><U+009C>weekday�<U+0080>� and �<U+0080><U+009C>weekend�<U+0080>� indicating whether a given date is a weekday or weekend day.


```r
as.factor(weekdays(activityComplete$date, abbreviate = TRUE))
```

```
##     [1] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##    [18] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##    [35] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##    [52] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##    [69] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##    [86] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##   [103] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##   [120] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [137] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##   [154] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##   [171] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##   [188] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [205] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##   [222] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##   [239] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##   [256] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [273] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##   [290] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##   [307] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [324] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [341] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##   [358] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##   [375] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [392] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [409] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##   [426] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [443] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [460] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [477] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##   [494] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [511] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [528] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##   [545] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [562] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [579] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [596] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##   [613] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [630] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [647] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [664] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##   [681] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [698] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [715] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##   [732] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##   [749] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [766] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [783] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##   [800] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##   [817] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [834] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##   [851] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##   [868] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##   [885] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [902] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##   [919] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##   [936] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##   [953] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##   [970] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##   [987] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1004] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1021] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1038] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1055] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1072] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1089] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [1106] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1123] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1140] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1157] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1174] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1191] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1208] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [1225] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1242] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1259] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1276] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1293] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1310] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1327] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [1344] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1361] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1378] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1395] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [1412] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1429] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1446] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [1463] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1480] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1497] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1514] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [1531] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1548] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1565] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [1582] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1599] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1616] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1633] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [1650] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1667] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1684] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1701] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [1718] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1735] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1752] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [1769] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1786] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1803] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1820] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [1837] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1854] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1871] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [1888] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1905] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1922] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [1939] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [1956] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [1973] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [1990] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2007] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [2024] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2041] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2058] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2075] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2092] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2109] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2126] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [2143] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2160] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2177] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2194] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2211] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2228] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2245] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [2262] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2279] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2296] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2313] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2330] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2347] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2364] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [2381] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2398] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2415] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2432] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [2449] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2466] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2483] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [2500] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2517] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2534] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2551] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [2568] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2585] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2602] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [2619] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2636] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2653] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2670] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [2687] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2704] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2721] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2738] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [2755] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2772] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2789] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [2806] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2823] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2840] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2857] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [2874] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2891] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2908] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [2925] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [2942] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [2959] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [2976] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [2993] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3010] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3027] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3044] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [3061] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3078] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3095] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3112] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3129] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3146] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3163] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [3180] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3197] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3214] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3231] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3248] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3265] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3282] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [3299] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3316] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3333] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3350] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3367] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3384] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3401] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [3418] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3435] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3452] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3469] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [3486] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3503] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3520] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [3537] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3554] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3571] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3588] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [3605] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3622] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3639] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [3656] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3673] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3690] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3707] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [3724] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3741] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3758] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3775] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [3792] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3809] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3826] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [3843] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3860] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3877] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3894] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [3911] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3928] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [3945] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [3962] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [3979] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [3996] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4013] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [4030] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4047] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4064] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4081] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [4098] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4115] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4132] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4149] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4166] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4183] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4200] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [4217] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4234] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4251] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4268] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4285] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4302] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4319] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [4336] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4353] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4370] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [4387] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4404] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4421] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4438] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [4455] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4472] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4489] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4506] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [4523] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4540] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4557] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [4574] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4591] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4608] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4625] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [4642] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4659] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4676] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [4693] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4710] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4727] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4744] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [4761] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4778] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4795] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4812] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [4829] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4846] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4863] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [4880] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4897] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4914] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4931] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [4948] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [4965] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [4982] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [4999] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5016] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5033] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5050] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [5067] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5084] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5101] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5118] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [5135] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5152] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5169] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5186] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5203] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5220] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5237] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [5254] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5271] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5288] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5305] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5322] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5339] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5356] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [5373] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5390] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5407] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [5424] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5441] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5458] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5475] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [5492] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5509] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5526] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5543] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [5560] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5577] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5594] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [5611] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5628] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5645] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5662] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [5679] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5696] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5713] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [5730] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5747] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5764] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5781] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [5798] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5815] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5832] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5849] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [5866] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5883] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5900] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [5917] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [5934] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [5951] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [5968] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [5985] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6002] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6019] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6036] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6053] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6070] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6087] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [6104] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6121] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6138] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6155] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [6172] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6189] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6206] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6223] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6240] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6257] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6274] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [6291] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6308] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6325] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6342] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6359] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6376] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6393] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [6410] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6427] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6444] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [6461] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6478] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6495] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6512] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [6529] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6546] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6563] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6580] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [6597] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6614] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6631] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [6648] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6665] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6682] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6699] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [6716] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6733] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6750] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [6767] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6784] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6801] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6818] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [6835] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6852] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6869] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6886] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [6903] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6920] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [6937] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [6954] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [6971] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [6988] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7005] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [7022] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7039] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7056] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7073] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7090] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7107] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7124] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [7141] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7158] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7175] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7192] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [7209] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7226] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7243] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7260] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7277] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7294] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7311] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [7328] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7345] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7362] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [7379] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7396] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7413] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7430] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [7447] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7464] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7481] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [7498] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7515] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7532] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7549] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [7566] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7583] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7600] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7617] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [7634] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7651] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7668] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [7685] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7702] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7719] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7736] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [7753] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7770] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7787] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [7804] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7821] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7838] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7855] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [7872] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7889] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7906] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7923] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [7940] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [7957] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [7974] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [7991] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8008] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8025] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8042] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [8059] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8076] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8093] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8110] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8127] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8144] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8161] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [8178] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8195] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8212] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8229] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [8246] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8263] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8280] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8297] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8314] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8331] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8348] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [8365] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8382] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8399] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [8416] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8433] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8450] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8467] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [8484] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8501] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8518] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [8535] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8552] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8569] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8586] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [8603] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8620] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8637] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8654] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [8671] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8688] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8705] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [8722] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8739] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8756] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8773] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [8790] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8807] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8824] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [8841] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8858] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8875] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8892] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [8909] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8926] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [8943] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [8960] vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar
##  [8977] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [8994] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9011] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9028] vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9045] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9062] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9079] mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b
##  [9096] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9113] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9130] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9147] mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9164] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9181] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9198] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi�
##  [9215] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9232] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9249] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9266] s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi�
##  [9283] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9300] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9317] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9334] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9351] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9368] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9385] mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom
##  [9402] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9419] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9436] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar
##  [9453] mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9470] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9487] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9504] dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue
##  [9521] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9538] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9555] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b
##  [9572] dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9589] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9606] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9623] jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun
##  [9640] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9657] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9674] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9691] jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie s�b dom lun
##  [9708] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9725] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9742] lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi�
##  [9759] jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9776] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9793] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9810] lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar mi� jue vie
##  [9827] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9844] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9861] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom
##  [9878] lun mar mi� jue vie lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9895] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9912] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9929] vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie lun mar
##  [9946] mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie
##  [9963] s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun
##  [9980] mar mi� jue vie s�b dom lun mar mi� jue vie s�b dom lun mar mi� jue
##  [9997] vie s�b dom lun
##  [ reached getOption("max.print") -- omitted 7568 entries ]
## Levels: dom jue lun mar mi� s�b vie
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

