Introduction
------------

JHU Coursera -- Reproducible Research (Course 5), Week 2 Assignment

Prep Enviroment
---------------

    rm(list = ls())
    setwd("C:/Users/akash/Desktop/StatsCourses/JHU_Specialization/Course5/w2")
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.5

    library(knitr)

    ## Warning: package 'knitr' was built under R version 3.2.5

    opts_chunk$set(echo = TRUE, results = 'hold')

Load Data
---------

#### Assumes datafile is in setwd

    rdata <- read.csv('activity.csv', header = TRUE, sep = ",",                   
                colClasses=c("numeric", "character", "numeric"))

Tidy The Data
-------------

    rdata$date <- as.Date(rdata$date, format = "%Y-%m-%d")
    rdata$interval <- as.factor(rdata$interval)

#### Check

    str(rdata)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

**What Is The Mean Total Number Of Steps Taken Per Day?**
---------------------------------------------------------

#### Total Number Of Steps Taken Per Day

    steps_per_day <- aggregate(steps ~ date, rdata, sum)
    colnames(steps_per_day) <- c("date","steps")
    head(steps_per_day)

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

#### Histogram: Total Number of Steps Taken Per Day

    ggplot(steps_per_day, aes(x = steps)) + 
        geom_histogram(fill = "blue4", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day",
        x = "Number of Steps per Day",
        y = "Number of times in a day(Count)") + theme_bw() 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

#### Mean And Median: Total Number Of Steps Taken Per Day

    steps_mean   <- mean(steps_per_day$steps, na.rm=TRUE)
    steps_median <- median(steps_per_day$steps, na.rm=TRUE)

The mean is **10766.189** and median is **10765**.

**What is the Average Daily Activity Pattern?**
-----------------------------------------------

#### Time Series Plot: Average Number Of Steps Taken (Averaged Across All Days) Vs the 5-Minute Intervals

    steps_per_interval <- aggregate(rdata$steps, 
        by = list(interval = rdata$interval), FUN=mean, na.rm=TRUE)
    steps_per_interval$interval <- as.integer(levels(steps_per_interval$interval)[steps_per_interval$interval])
    colnames(steps_per_interval) <- c("interval", "steps")
    ggplot(steps_per_interval, aes(x=interval, y=steps)) + 
        geom_line(color="purple", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval",
        y="Number of steps") + theme_bw()

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

#### Max 5 Minute Interva;

    max_interval <- steps_per_interval[which.max(steps_per_interval$steps),]

The **835<sup>th</sup>** interval has maximum **206** steps.

**Imputing Missing Values**
---------------------------

###### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### Total Number Of Missing Values

    missing_vals <- sum(is.na(rdata$steps))

The total number of missing values are **2304**.

#### Solution: How To Fix The N/As

    na_fill <- function(data, pervalue) {
            na_index <- which(is.na(data$steps))
            na_replace <- unlist(lapply(na_index, FUN=function(idx){
                    interval = data[idx,]$interval
                    pervalue[pervalue$interval == interval,]$steps
            }))
            fill_steps <- data$steps
            fill_steps[na_index] <- na_replace
            fill_steps
    }

    rdata_fill <- data.frame(  
            steps = na_fill(rdata, steps_per_interval),  
            date = rdata$date,  
            interval = rdata$interval)
    str(rdata_fill)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
    ##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
    ##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...

#### Check

    sum(is.na(rdata_fill$steps))

    ## [1] 0

Zero output shows that there are *no missing values*.

#### Histogram: Total Number of Steps Taken Per Day (With Filled In N/As)

    fill_steps_per_day <- aggregate(steps ~ date, rdata_fill, sum)
    colnames(fill_steps_per_day) <- c("date","steps")
    ggplot(fill_steps_per_day, aes(x = steps)) + 
           geom_histogram(fill = "turquoise4", binwidth = 1000) + 
           labs(title="Histogram of Steps Taken per Day", 
           x = "Number of Steps per Day", 
           y = "Number of times in a day(Count)") + theme_bw() 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-13-1.png)

#### Mean And Median: Total Number Of Steps Taken Per Day (With Filled In N/As)

    steps_mean_fill   <- mean(fill_steps_per_day$steps, na.rm=TRUE)
    steps_median_fill <- median(fill_steps_per_day$steps, na.rm=TRUE)

The mean is **10766.189** and median is **10766.189**.

#### Do these values differ from the estimates from the first part of the assignment?

##### Before Filling In N/As

    Mean  : 10766.189
    Median: 10765

##### After Filling In N/As

    Mean  : 10766.189
    Median: 10766.189

The mean is virtually the same in Before vs After but the median
slightly increases.

**Are There Differences In Activity Patterns Between Weekdays And Weekends?**
-----------------------------------------------------------------------------

    weekdays_steps <- function(data) {
        weekdays_steps <- aggregate(data$steps, by=list(interval = data$interval),
                              FUN=mean, na.rm=T)
     
        weekdays_steps$interval <- 
                as.integer(levels(weekdays_steps$interval)[weekdays_steps$interval])
        colnames(weekdays_steps) <- c("interval", "steps")
        weekdays_steps
    }

    data_by_weekdays <- function(data) {
        data$weekday <- 
                as.factor(weekdays(data$date)) # weekdays
        weekend_data <- subset(data, weekday %in% c("Saturday","Sunday"))
        weekday_data <- subset(data, !weekday %in% c("Saturday","Sunday"))
        
        weekend_steps <- weekdays_steps(weekend_data)
        weekday_steps <- weekdays_steps(weekday_data)
        
        weekend_steps$dayofweek <- rep("weekend", nrow(weekend_steps))
        weekday_steps$dayofweek <- rep("weekday", nrow(weekday_steps))
        
        data_by_weekdays <- rbind(weekend_steps, weekday_steps)
        data_by_weekdays$dayofweek <- as.factor(data_by_weekdays$dayofweek)
        data_by_weekdays
    }

    data_weekdays <- data_by_weekdays(rdata_fill)

#### Panel Plot: Weekdays Vs Weekends

    ggplot(data_weekdays, aes(x=interval, y=steps)) + 
            geom_line(color="darksalmon") + 
            facet_wrap(~ dayofweek, nrow=2, ncol=1) +
            labs(x="Interval", y="Number of steps") +
            theme_bw()

![](PA1_template_files/figure-markdown_strict/plot_weekdays-1.png)

##### Weekdays has the highest peak, but Weekend has more steps over 100.

###### The End.
