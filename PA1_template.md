---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```


## Loading and preprocessing the data

```r
unzip("activity.zip", junkpaths = TRUE)
activityData <- read.csv("activity.csv")

activityData$date <- as.Date(activityData$date)

head(activityData)
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
# Calculate the total number of steps taken per day
totalSteps <- activityData %>%
    group_by(date) %>%
    summarise(total = sum(steps, na.rm = TRUE))
# Histogram of the total number of steps taken each day
hist(totalSteps$total)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Mean & Median of the total number of steps taken per day
meanSteps <- mean(totalSteps$total, na.rm = TRUE)
medianSteps <- median(totalSteps$total, na.rm = TRUE)

meanSteps
```

```
## [1] 9354.23
```

```r
medianSteps
```

```
## [1] 10395
```



## What is the average daily activity pattern?

```r
avgDailyActivity <- activityData %>%
    group_by(interval) %>%
    summarize(average_steps = mean(steps, na.rm = TRUE))

with(avgDailyActivity, plot(x = interval, y = average_steps, type = "l",
                                xlab = "5 min Intervals in a day",
                                ylab = "Average Number of Steps", 
                                main = "The Average Daily Activity Pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
# 5-minute interval which contains maximum average number of steps
maxInterval = avgDailyActivity$interval[which.max(avgDailyActivity$average_steps)]
maxInterval
```

```
## [1] 835
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
