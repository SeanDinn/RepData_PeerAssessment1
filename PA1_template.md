# Reproducible Research: Peer Assessment 1
## Summary: This project analyzes data collected from a personal fitness device

## Loading and preprocessing the data

The data, collected over two months, was imported into R. The date was properly formatted and the appropriate libraries were loaded.


```r
proj.data <- read.csv("activity.csv", stringsAsFactors = FALSE)
        proj.data$date <- as.Date(proj.data$date, format="%m/%d/%Y")
        library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
        library(ggplot2)
```

## What is mean total number of steps taken per day?

The mean number of steps was then analyzed. Data associated with missing values was removed. The data was then grouped by day and the mean values calculated and displayed as a histogram.


```r
        with.steps <- filter(proj.data, steps != "NA")
        by.date <- group_by(with.steps, date)
        sum.steps <- summarise(by.date, per_day = sum(steps))
        qplot(per_day, data = sum.steps, bins = 9)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
        summarize(sum.steps, mean.steps=round(mean(per_day)),median.steps=round(median(per_day)))
```

```
## # A tibble: 1 × 2
##   mean.steps median.steps
##        <dbl>        <dbl>
## 1      10766        10765
```

## What is the average daily activity pattern?

Data was collected in 5 minute intervals during the day and the mean steps per interval across the data set is presented below.


```r
        by.interval <- group_by(with.steps, interval)
        steps.per.interval <- summarize(by.interval, per_interval = mean(steps))
        plot(steps.per.interval, type="l", main="Daily Activity Pattern", ylab="Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
        print ("The interval with the average maximum number of steps is: ")
```

```
## [1] "The interval with the average maximum number of steps is: "
```

```r
        steps.per.interval$interval[steps.per.interval$per_interval==max
                                    (steps.per.interval$per_interval)]
```

```
## [1] 835
```

## Imputing missing values

The data was then reanalyzed after imputing the missing values. It was determin that there were 2304 missing values (steps)


```r
        missing.value <- is.na(proj.data$steps)
        print("The total number of missing values is:")
```

```
## [1] "The total number of missing values is:"
```

```r
        sum(missing.value)
```

```
## [1] 2304
```

The step values are imputed by using the mean number of steps for the interval that has a missing value.


```r
        imputed <- proj.data
        imputed$steps <- ifelse(is.na(imputed$steps)== TRUE, steps.per.interval$
        per_interval[steps.per.interval$interval %in% imputed$interval],imputed$steps) 
```

The analysis of steps per day was run again with the imputed values added


```r
        imputed.date <- group_by(imputed, date)
        imputed.steps <- summarise(imputed.date, per_day = sum(steps))
        qplot(per_day, data = imputed.steps, bins = 9)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
        summarize(imputed.steps, mean.steps=round(mean(per_day)),
                  median.steps=round(median(per_day)))
```

```
## # A tibble: 1 × 2
##   mean.steps median.steps
##        <dbl>        <dbl>
## 1      10766        10766
```

When compared to the original histogram, the histogram including the imputed values has the same basic shape. It appears as though many of the missing values were from intervals that came from increased activity times as that block (~10,000 per day) showed the most increase in count. The mean number of steps did not change and the median only changed by one step.

## Are there differences in activity patterns between weekdays and weekends?

We wanted to investigate if there were different daily activity patterns on weekdays versus weekends. Non-imputed data was used for this analysis. The dates were assigned to weekday or weekend and activity patterns are plotted below.


```r
        is.weekend <- c("Saturday", "Sunday")
        with.steps$day <- factor(weekdays(with.steps$date) %in% is.weekend, levels=c(FALSE, TRUE),
                          labels = c("Weekday", "Weekend"))
        by.interval.day <- group_by(with.steps, interval, day)
        steps.per.interval.split <- summarize(by.interval.day, per_interval = mean(steps))
        qplot(interval, per_interval, data=steps.per.interval.split, facets = day ~ ., 
              ylab="Number of Steps", geom="line" )
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Looking at the data, expected differences are apparent. First, during the week, activity starts earlier in the day compared to the weekend. On the weekend, the activity starts later, but there is sustained activity throughout the day.
