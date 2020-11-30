---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
    theme: united
    toc: true
---
## Required packages



## Loading and preprocessing the data

```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
unzipped<-unz(temp, "activity.csv")
data <- read.csv(unzipped)
unlink(temp)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

```r
total_steps_per_day<-aggregate(data[, 1], list(data$date), sum, na.rm=TRUE)
str(total_steps_per_day)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ Group.1: chr  "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
##  $ x      : int  0 126 11352 12116 13294 15420 11015 0 12811 9900 ...
```

```r
mean(total_steps_per_day$x)
```

```
## [1] 9354.23
```

```r
ggplot(total_steps_per_day, aes(x=x))+geom_histogram( alpha=0.5,bins = 50) +
labs(title="Histogram of Total Steps Per Day", x="Total Steps per Day",
    caption="Red dashed line for Mean Total Steps per Day, Green dashed line for Median Total Steps per Day")+
  geom_vline(aes(xintercept = mean(x)),col='red',size=1,linetype="dashed")+
  geom_vline(aes(xintercept = median(x)),col='green',size=1,linetype="dashed")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(total_steps_per_day$x)
```

```
## [1] 9354.23
```

```r
median(total_steps_per_day$x)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

```r
mean_steps_per_interval<-aggregate(data[, 1], list(data$interval), mean, na.rm=TRUE)
str(mean_steps_per_interval)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ Group.1: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ x      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
ggplot(mean_steps_per_interval, aes(x=Group.1,y=x))+
    geom_line( size=1) +
  labs(title="Line Plot of Total Steps Per Interval", x="Interval", y="Total steps")+
   theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean(mean_steps_per_interval$x)
```

```
## [1] 37.3826
```

```r
median(mean_steps_per_interval$x)
```

```
## [1] 34.11321
```

```r
###Interval with most steps on average
mean_steps_per_interval[which.max(mean_steps_per_interval$x),]$Group.1
```

```
## [1] 835
```

```r
###Average number of steps for interval with maximum average
mean_steps_per_interval[which.max(mean_steps_per_interval$x),]$x
```

```
## [1] 206.1698
```

## Imputing missing values
### Using predictive mean matching, m=5 imputations

```r
md.pairs(data)
```

```
## $rr
##          steps  date interval
## steps    15264 15264    15264
## date     15264 17568    17568
## interval 15264 17568    17568
## 
## $rm
##          steps date interval
## steps        0    0        0
## date      2304    0        0
## interval  2304    0        0
## 
## $mr
##          steps date interval
## steps        0 2304     2304
## date         0    0        0
## interval     0    0        0
## 
## $mm
##          steps date interval
## steps     2304    0        0
## date         0    0        0
## interval     0    0        0
```

```r
mean(is.na(data))
```

```
## [1] 0.04371585
```

```r
imputed_data <- mice(data, m = 5)
```

```
## 
##  iter imp variable
##   1   1  steps
##   1   2  steps
##   1   3  steps
##   1   4  steps
##   1   5  steps
##   2   1  steps
##   2   2  steps
##   2   3  steps
##   2   4  steps
##   2   5  steps
##   3   1  steps
##   3   2  steps
##   3   3  steps
##   3   4  steps
##   3   5  steps
##   4   1  steps
##   4   2  steps
##   4   3  steps
##   4   4  steps
##   4   5  steps
##   5   1  steps
##   5   2  steps
##   5   3  steps
##   5   4  steps
##   5   5  steps
```

```
## Warning: Number of logged events: 1
```

```r
imputed_data
```

```
## Class: mids
## Number of multiple imputations:  5 
## Imputation methods:
##    steps     date interval 
##    "pmm"       ""       "" 
## PredictorMatrix:
##          steps date interval
## steps        0    0        1
## date         1    0        1
## interval     1    0        0
## Number of logged events:  1 
##   it im dep     meth  out
## 1  0  0     constant date
```

```r
imputed_data2 <- complete(imputed_data, "long", inc = TRUE)

mean(data$steps, na.rm=TRUE)
```

```
## [1] 37.3826
```

```r
median(data$steps, na.rm=TRUE)
```

```
## [1] 0
```

```r
mean(imputed_data2$steps, na.rm=TRUE)
```

```
## [1] 38.24512
```

```r
median(imputed_data2$steps, na.rm=TRUE)
```

```
## [1] 0
```

```r
total_steps_per_day_imputed<-aggregate(imputed_data2[, 3], list(imputed_data2$date), sum, na.rm=TRUE)
str(total_steps_per_day_imputed)
```

```
## 'data.frame':	61 obs. of  2 variables:
##  $ Group.1: chr  "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
##  $ x      : int  60511 756 68112 72696 79764 92520 66090 69465 76866 59400 ...
```

```r
ggplot(total_steps_per_day_imputed, aes(x=x))+geom_histogram( alpha=0.5,bins = 50) +
labs(title="Histogram of Total Steps Per Day", x="Total Steps per Day",
    caption="Red dashed line for Mean Total Steps per Day, Green dashed line for Median Total Steps per Day")+
  geom_vline(aes(xintercept = mean(x)),col='red',size=1,linetype="dashed")+
  geom_vline(aes(xintercept = median(x)),col='green',size=1,linetype="dashed")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 20))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(total_steps_per_day$x)
```

```
## [1] 9354.23
```

```r
median(total_steps_per_day$x)
```

```
## [1] 10395
```
## Are there differences in activity patterns between weekdays and weekends?

```r
days<-wday(data$date, label = TRUE, abbr = TRUE)
data2<-cbind(days,data)
table(data2$days)
```

```
## 
##  Sun  Mon  Tue  Wed  Thu  Fri  Sat 
## 2304 2592 2592 2592 2592 2592 2304
```

```r
data2$weekdays[days=='Mon'|days=='Tue'|days=='Wed'|days=='Thu'|days=='Fri']<-"Weekdays"
data2$weekdays[days=='Sun'|days=='Sat']<-"Weekend"
data2$weekdays<-factor(data2$weekdays)
mean_steps_per_interval_weekday_v_weekend<-aggregate(data=data2,steps~weekdays+interval, mean, na.rm=TRUE)
xyplot(data=mean_steps_per_interval_weekday_v_weekend, steps~interval|weekdays,
   main="Scatterplots of steps by interval: Comparing weekends and weekdays",
   ylab="Steps", xlab="Interval", type = c("p", "g", "smooth"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
