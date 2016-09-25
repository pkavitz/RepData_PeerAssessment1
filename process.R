################################################################
##
## Paul Kavitz
## Reproducible Research
## September 22, 2016
##
################################################################

## Loading and preprocessing the data

library(dplyr)  ## load necessary libraries
library(lubridate)
library(ggplot2)
library(lattice)

if (file.exists("activity.zip")) {
    unzip("activity.zip")
} else {
    stop("Zip file not found.")
}

if (file.exists("activity.csv")) {
    activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
    activity <- mutate(activity, date=ymd(date))
} else {
    stop("Data not found.")
}

dailytotal <- aggregate(steps~date, activity, sum)
mediansteps <- median(dailytotal$steps)
meansteps <- mean(dailytotal$steps)

## What is mean total number of steps taken per day?

dailyhist <- ggplot(dailytotal, aes(x=steps)) +
    xlab("Steps") + ylab("Days with given number of steps") +
    geom_histogram(bins=25, col="white", fill="darkblue") +
    ggtitle("Histogram of Total Steps per Day") +
    geom_vline(aes(xintercept=mediansteps, col="red"), show.legend=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limit = c(0,9.5),
                       breaks = seq(from =0, to=9.5, by=1))
print(dailyhist)

## What is the average daily activity pattern?
intmean <- aggregate(steps~interval, activity, mean)
maxavgsteps <- max(intmean$steps)
maxavginterval <- intmean[which.max(intmean$steps), "interval"]
maxlabel <- paste("Max average steps\n at interval ", maxavginterval)

## Plot for Daily Activity Pattern

dailypattern <- ggplot(intmean,
                       aes(x=interval, y=steps)) +
    geom_line() +
    geom_point(x=maxavginterval, y=maxavgsteps, col="red") +
    geom_label(x=maxavginterval+400, y=maxavgsteps, col="black",
               aes(label=maxlabel)) +
    xlab("Interval") + ylab("Average Steps") +
    scale_x_continuous(breaks=seq(from=0, to=2400 , by = 200),
                       expand = c(0,0), limit=c(0, 2360)) +
    scale_y_continuous(breaks=seq(from=0, to=240, by = 20),
                       expand = c(0,0), limit=c(0,220)) +
    ggtitle("Average Steps per Interval")
print(dailypattern)

## Imputing missing values

missingsteps <- sum(is.na(activity$steps))   ## Count # of NAs
impactivity <- activity

## Function for determining whether date is a weekday or weekend
calcperiod <- function (d) {
    if (weekdays(d) %in% c("Saturday", "Sunday")) {
        return("Weekend")
    } else {
        return("Weekday")
    }
}

for (i in 1:nrow(impactivity))
{
    if (is.na(impactivity[i,"steps"])) {
        impactivity[i,"steps"] <-
            round(intmean[intmean$interval==impactivity[i,"interval"],"steps"])
    }
    impactivity[i,"weekperiod"] <- calcperiod(impactivity[i,"date"])
    next
}

imputedtotal <- aggregate(steps~date, impactivity, sum)
imputedmedian <- median(imputedtotal$steps)
imputedmean <- mean(imputedtotal$steps)

imputedhist <- ggplot(dailytotal, aes(x=steps)) +
    xlab("Steps") + ylab("Days with given number of steps") +
    geom_histogram(bins=25, col="white", fill="darkblue") +
    ggtitle("Histogram of Total Steps per Day") +
    geom_vline(aes(xintercept=imputedmedian, col="red"), show.legend=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limit = c(0,9.5),
                       breaks = seq(from =0, to=9.5, by=1))
print(imputedhist)

## Are there differences in activity patterns between weekdays and weekends?

## Use imputed dataset to contrast weekday and weekend activity pattern
impactivity <- mutate(impactivity, weekperiod=factor(weekperiod))
intmean2 <- aggregate(steps~interval+weekperiod, impactivity, mean)

## Plot to contrast daily activity pattern between weekday and weekend
par(mfcol = c(1,2))  ## Create canvas for two plots.
print(xyplot(steps~interval | weekperiod, intmean2, layout=c(1,2), type="l",
             xlab="Interval", ylab="Number of steps",
             main="Daily Activity"))