Reproducible Research: Peer Assessment 1
----------------------------------------

    library(ggplot2)
    library(scales)
    library(Hmisc)

    ## Loading required package: lattice

    ## Loading required package: survival

    ## Loading required package: Formula

    ## 
    ## Attaching package: 'Hmisc'

    ## The following objects are masked from 'package:base':
    ## 
    ##     format.pval, round.POSIXt, trunc.POSIXt, units

    library(chron)

Loading and preprocessing the data
----------------------------------

1.  Load the data (i.e. read.csv())

Process the data:
-----------------

    total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    library(ggplot2)
    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](pa1.template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

1.  Calculate and report the mean and median total number of steps taken
    per day

<!-- -->

    mean(total.steps, na.rm=TRUE)

    ## [1] 9354.23

    median(total.steps, na.rm=TRUE)

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

1.  Make a time series plot

<!-- -->

    library(ggplot2)
    averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                          FUN=mean, na.rm=TRUE)
    ggplot(data=averages, aes(x=interval, y=steps)) +
            geom_line() +
            xlab("5-minute interval") +
            ylab("average number of steps taken")

![](pa1.template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

    averages[which.max(averages$steps),] 

    ##     interval    steps
    ## 104      835 206.1698

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    averages[which.max(averages$steps),]

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset

<!-- -->

    missing <- is.na(data$steps)
    # How many missing
    table(missing)

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

Number of missing values: r numMissingValues

There are many days/intervals where there are missing values (coded as
NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.

1.  Devise a strategy for filling in all of the missing values in
    the dataset.

To populate missing values, we choose to replace them with the mean
value at the same interval across days.

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    fill.value <- function(steps, interval) {
            filled <- NA
            if (!is.na(steps))
                    filled <- c(steps)
            else
                    filled <- (averages[averages$interval==interval, "steps"])
            return(filled)
    }
    filled.data <- data
    filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

    total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")

![](pa1.template_files/figure-markdown_strict/unnamed-chunk-9-1.png)

... and Calculate and report the mean and median total number of steps
taken per day.

    mean(total.steps)

    ## [1] 10766.19

    median(total.steps)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two level

<!-- -->

    library(chron)
    filled.data$date <- as.Date(filled.data$date,"%m/%d/%Y")

    filled.data$day <- ifelse(is.weekend(filled.data$date),"weekend","weekday")

1.  Make a panel plot containing a time series plot

<!-- -->

    averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
            xlab("5-minute interval") + ylab("Number of steps")

![](pa1.template_files/figure-markdown_strict/unnamed-chunk-12-1.png)
