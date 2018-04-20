PA1_template
====================


```r
act <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
act <- na.omit(act)
act$date <- as.Date(act$date)
step_day <- aggregate(act$steps, by = list(act$date), FUN = sum)
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
ggplot(step_day, aes(x = Group.1, y = x, color = "red")) + geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

This is to plot the histogram of the total number steps taken each day


```r
mean_day <- mean(step_day$x)
median_day <- median(step_day$x)
```

This is to calculate the mean and median number of steps taken each day


```r
step_interval <- aggregate(act$steps, by = list(act$interval), FUN = mean)
plot(step_interval$Group.1, step_interval$x, type = "l", main = "Time series plot of average steps per interval", xlab = "Interval", ylab = "Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

This is to plot the time series plot of the averge number of steps taken


```r
subset(step_interval, x == max(step_interval$x))
```

```
##     Group.1        x
## 104     835 206.1698
```

The 5-minute interval that, on average, contains the maximum number of steps


```r
act <- read.csv("activity.csv", header = TRUE, stringsAsFactors = FALSE)
impute <- function(x, x.impute){ifelse(is.na(x), x.impute, x)}
```

Code to describe and show a strategy for imputing missing data


```r
act$steps <- impute(act$steps, mean(na.omit(act$steps)))
step_nona <- aggregate(act$steps, by = list(act$date), sum)
ggplot(step_nona, aes(x = Group.1, y = x, color = "red")) + geom_histogram(stat = "identity")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

Histogram of the total number of steps taken each day after missing values are imputed


```r
act$date <- weekdays(as.Date(act$date))
act_weekdays <- subset(act, date == "Monday" | date == "Tuesday" | date == "Wednesday" | date == "Thursday" | date == "Friday")
act_weekends <- subset(act, date == "Saturday" | date == "Sunday")
opar <- par(no.readonly = T)
par(mfrow = c(2, 1))
step_weekdays <- aggregate(act_weekdays$steps, by = list(act_weekdays$interval), mean, na.rm = T)
step_weekends <- aggregate(act_weekends$steps, by = list(act_weekends$interval), mean, na.rm = T)
plot(step_weekdays$Group.1, step_weekdays$x, type = "l")
plot(step_weekends$Group.1, step_weekends$x, type = "l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
par(opar)
dev.off()
```

```
## null device 
##           1
```

Panel plot comparing to the average number of steps taken per 5-minute interval across weekdays and weekends.


