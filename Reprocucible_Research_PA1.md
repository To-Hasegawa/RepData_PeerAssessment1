---
title: "Reproducible_Research_PA1"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---

## Loading and preprocessing the data
1. Load the data

```r
df <- read.csv("activity.csv")
```

2. Process the data

```r
head(df)
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
summary(df)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

```r
#The format looks no problem to analyze.
```


## What is mean total number of steps taken per day?
1. Make a histgram

```r
#aggregate each day's step
StepsPerDay <- aggregate(x=df$steps, by=list(df$date), FUN=sum)

#make histgram
hist(StepsPerDay$x)
```

![](Reprocucible_Research_PA1_files/figure-html/histgram-1.png)<!-- -->

2. Calculate and report mean and median

```r
mean1 <- mean(StepsPerDay$x, na.rm = TRUE)
median1 <- median(StepsPerDay$x, na.rm = TRUE)

mean1
```

```
## [1] 10766.19
```

```r
median1
```

```
## [1] 10765
```


## What is the average daily activity pattern?
1. Make a time series plot

```r
AverageSteps <- aggregate(steps~interval, df, FUN=mean)
plot(x=AverageSteps$interval, y=AverageSteps$steps, type="l")
```

![](Reprocucible_Research_PA1_files/figure-html/time_series_plot1-1.png)<!-- -->

2. Which 5-minute interval is maximum steps?

```r
which.max(AverageSteps$steps)
```

```
## [1] 104
```

```r
#The answer is 104th row. Check 104 row.
AverageSteps$interval[104]
```

```
## [1] 835
```

```r
#The answer is AM8:35.
```


## Imputing missing values
1. Calculate total number of missing value

```r
table(is.na(df))
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

```r
#The number of NA is 2304.
```

2. Make a new dataset which filled missing value
My strategy is fill NA by average 5-minute interval value.

```r
df2 <- cbind(df, AverageSteps$steps)
df2$steps[is.na(df2$steps)] <- df2$`AverageSteps$steps`
```

```
## Warning in df2$steps[is.na(df2$steps)] <- df2$`AverageSteps$steps`: 置き換
## えるべき項目数が、置き換える数の倍数ではありませんでした
```

```r
df2$`AverageSteps$steps` <- NULL
head(df2)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

3. Make a histgram of the total number of steps taken each day

```r
#aggregate each day's step
StepsPerDay2 <- aggregate(steps~date, df2, FUN=sum)

#make histgram
hist(StepsPerDay2$steps)
```

![](Reprocucible_Research_PA1_files/figure-html/histgram_and_mean_and_median-1.png)<!-- -->

4. Calculate mean and median total number of steps taken per day

```r
#mean and median
mean2 <- mean(StepsPerDay2$steps)
median2 <- median(StepsPerDay2$steps)

#compare result by first assignment.
mean2 - mean1
```

```
## [1] 0
```

```r
#mean is not change because only add average value.

median2 - median1
```

```
## [1] 1.188679
```

```r
#median is a little bit change because the center is moved by filled value.
```
I got interesting result which difference of data filled of not.
In this time I filled missing data by 61 days average 5-mintute interval data.
Because of that the distribution of number of steps taken per day's mean and median don't change so mach.
But if it is filled by not suitable value, the distribution will change and lost the meaning of data.
So I think the impact of imputing missing data is significant and should do very carefully.


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable "weekday(TRUE)" and "weekend(FALSE)"

```r
library(timeDate)
df3 <- df2
df3$weekday <- isWeekday(df2$date)
head(df3)
```

```
##       steps       date interval weekday
## 1 1.7169811 2012-10-01        0    TRUE
## 2 0.3396226 2012-10-01        5    TRUE
## 3 0.1320755 2012-10-01       10    TRUE
## 4 0.1509434 2012-10-01       15    TRUE
## 5 0.0754717 2012-10-01       20    TRUE
## 6 2.0943396 2012-10-01       25    TRUE
```

2. Make a panel plot

```r
#prepare two of dataset
df_weekday <- subset(df3, df3$weekday=="TRUE")
df_weekdayMean <-aggregate(steps~interval, df_weekday, FUN=mean)

df_weekend <- subset(df3, df3$weekday=="FALSE")
df_weekendMean <-aggregate(steps~interval, df_weekend, FUN=mean)

#make panel plot
par(mfrow=c(2,1))
plot(x=df_weekdayMean$interval, y=df_weekdayMean$steps, main="Weekday", type="l")
plot(x=df_weekendMean$interval, y=df_weekendMean$steps, main="Weekend", type="l")
```

![](Reprocucible_Research_PA1_files/figure-html/panel_plot-1.png)<!-- -->





It looks different.
At weekend daytime steps is high, that is, at weekend the person walk actively.
