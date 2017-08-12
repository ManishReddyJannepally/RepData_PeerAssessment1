Reproducible Research - Project 1
================

Loading and preprocessing the data
----------------------------------

-   loading the data

``` r
unzip("Project1_data.zip")
activity_data = read.csv("activity.csv",header = T,na.strings = "NA")
```

-   preprocessing the data

``` r
str(activity_data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

``` r
activity_data$date = as.Date(as.character(activity_data$date))
str(activity_data)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

Mean total number of steps taken per day
----------------------------------------

### Ignored missing values in dataset

-   Calculate the total number of steps taken per day

``` r
steps_perday <- aggregate(steps ~ date, data = activity_data, sum, na.rm = TRUE)
```

-   Make a histogram of the total number of steps taken each day

``` r
hist(steps_perday$steps, col = "grey",breaks = 16,ylim = c(0,10),xlim = c(0,25000)
                                        ,xlab = "Steps", main = "Histogram")
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/histogram%20of%20steps%20per%20day-1.png)

-   Calculate and report the mean and median of the total number of steps taken per day

``` r
#day wise means and medians
daywise_means = aggregate(steps ~ date, data = activity_data, FUN = mean)
daywise_medians = aggregate(steps ~ date, data = activity_data, FUN = median)
#mean and median of total steps per day
mean(steps_perday$steps)
```

    ## [1] 10766.19

``` r
median(steps_perday$steps)
```

    ## [1] 10765

Daily activity pattern
----------------------

-   Time series plot of 5-minute interval and average number of steps taken, aaveraged across all days

``` r
steps_interval = aggregate(steps~interval, data = activity_data, FUN = mean)
with(steps_interval,plot(steps~interval,type = 'l', xlab = "5-minute interval"))
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/time%20series%20plot-1.png)

-   5-minute interval that contains maximun number of steps

``` r
#which.max(steps_interval$steps)
#steps_interval[104,1]
max_steps_interval = steps_interval[which.max(steps_interval$steps),1]
max_steps_interval
```

    ## [1] 835

Imputing missing values
-----------------------

-   Calculate and report the total number of missing values in the dataset

``` r
missing_values = sum(is.na(activity_data))
missing_values
```

    ## [1] 2304

-   Strategy for filling in all of the missing values in the dataset

    I tried to impute NAs with the mean of steps variable but that strategy bought lot of variation from original dataset. Then I tried to impute NAs with their 5-minute interval's steps mean(averages from the time intervals). Step wise strategy explained in code.

``` r
head(activity_data)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
#created 2 new dataframes with and without missing values
activity_data_NA = activity_data[is.na(activity_data),]
activity_data_noNA = activity_data[!is.na(activity_data),]

#inserting 5-minute interval's steps mean into all corresponding NAs
activity_data_NA$steps = steps_interval$steps

#checking for NAs
sum(is.na(activity_data_NA))
```

    ## [1] 0

``` r
#binding the dataframes after imputing NAs to form a new dataframe
activity_data_imputed = rbind(activity_data_NA,activity_data_noNA)

#forming a propper ordered dataframe as before (activity_data)
activity_data_imputed = activity_data_imputed[order(activity_data_imputed[,2],
                                                    activity_data_imputed[,3]),]
```

"Forming a new data set" was also explained above for better understanding of imputing missing values.

-   Create a new dataset that is equal to the original dataset but with the missing data filled in

``` r
#binding the dataframes after imputing NAs to form a new dataframe
activity_data_imputed = rbind(activity_data_NA,activity_data_noNA)

#forming a propper ordered dataframe as before (activity_data)
activity_data_imputed = activity_data_imputed[order(activity_data_imputed[,2],
                                                    activity_data_imputed[,3]),]
head(activity_data_imputed)
```

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

-   Histogram of the total number of steps taken each day

``` r
steps_perday_imputed = aggregate(steps~date, data = activity_data_imputed,FUN = sum)
#Comparing the previous hist with NAs with new hist with imputed values
par(mfrow = c(1,2))
hist(steps_perday$steps, col = "grey",breaks = 16,ylim = c(0,10),xlim = c(0,25000)
                ,xlab = "Steps", main = "Histogram before imputing vlaues")
with(steps_perday_imputed,hist(steps, col = "grey", breaks = 16, xlab = "steps",
                               main = "histogram after imputed values"))
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/histogram%20with%20imputed%20values-1.png)

-   Mean and median total number of steps taken per day Here, I am calculating mean and median for total steps per day before and after imputing NAs, to give you better picture of impact because of imputed values

``` r
#mean of steps per day before imputing NAs
mean(steps_perday$steps)
```

    ## [1] 10766.19

``` r
#mean of steps per day after imputing NAs
mean(steps_perday_imputed$steps)
```

    ## [1] 10766.19

``` r
#median of steps per day before imputing NAs
median(steps_perday$steps)
```

    ## [1] 10765

``` r
#median of steps per day after imputing NAs
median(steps_perday_imputed$steps)
```

    ## [1] 10766.19

    The mean for the total steps per day were both 10766.19 and median has slightly varied when compared. This seems to confirm from the histogram that imputing the missing data with the mean averages from the time intervals helps to create a normal distribution of the total steps.

Differences in activity patterns between weekdays and weekends
--------------------------------------------------------------

-   Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

``` r
activity_data_imputed$weekday = as.factor(weekdays(activity_data_imputed$date))
levels(activity_data_imputed$weekday) = list(weekday = c("Monday", "Tuesday",
                "Wednesday","Thursday","Friday"), weekend = c("Saturday","Sunday"))
str(activity_data_imputed)
```

    ## 'data.frame':    52704 obs. of  4 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
    ##  $ weekday : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...

-   Time series plot of 5-minute interval and average steps taken, averaged across all weekdays and weekends

``` r
par(mfrow = c(2,1))
with(activity_data_imputed[activity_data_imputed$weekday == "weekday",],
     plot(aggregate(steps~interval, FUN = mean),type = 'l', main = "Weekdays"))
with(activity_data_imputed[activity_data_imputed$weekday == "weekend",],
     plot(aggregate(steps~interval, FUN = mean),type = 'l', main = "Weekends"))
```

![](PA1_template_files/figure-markdown_github-ascii_identifiers/weekdays%20and%20weekends%20plot-1.png)
