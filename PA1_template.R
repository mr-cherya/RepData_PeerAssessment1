##1.load and set data
getwd()
setwd("C:/Users/chern/Documents/R/RepData_PeerAssessment1")
directory <- "Part2Week3"
unzip(zipfile="activity.zip")

data <- read.csv("activity.csv", header = TRUE)

SpD <- tapply(data$steps, data$date, sum)

## 2. Histogram of the total number of steps taken each day
png("1.png", width=600, height=700)
hist(SpD, xlab = "Number of Steps", main = "Steps per Day")
dev.off()

## 3. Mean and median number of steps taken each day
mean(SpD, na.rm=TRUE)
median(SpD, na.rm=TRUE)

## 4. Time series plot of the average number of steps taken
library(ggplot2)
av <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                                FUN=mean, na.rm=TRUE)
png("2.png", width=800, height=700)

ggplot(data=av, aes(x=interval, y=steps)) +
     geom_line() +
     xlab("Interval") +
     ylab("Steps")+
     ggtitle("
             Average number of steps
             ")

dev.off()

## 5. The 5-minute interval that, on average, contains the maximum number of steps
av[which.max(av$steps),]

## 6. Code to describe and show a strategy for imputing missing data
missing <- sum(is.na(data$steps))
print(missing)

missing.value <- function(steps, interval) {
    missed <- NA
     if (!is.na(steps))
          missed <- c(steps)
     else
          missed <- (av[av$interval==interval, "steps"])
     return(missed)
}
new.data <- data
new.data$steps <- mapply(missing.value, new.data$steps, new.data$interval)


## 7. Histogram of the total number of steps taken each day after missing values are imputed
new.steps <- tapply(new.data$steps, new.data$date, FUN=sum)
png("3.png", width=600, height=700)
hist(new.steps, xlab = "Number of Steps", main = "Steps per Day without NA")
dev.off()

mean(new.steps, na.rm=TRUE)
median(new.steps, na.rm=TRUE)

## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

days <- function(date) {
     weekdays <- days(date)
     if (weekdays %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
          return("weekday")
     else if (weekdays %in% c("Saturday", "Sunday"))
          return("weekend")
     else
          stop("invalid date")
}
new.data$date <- as.Date(new.data$date)
new.data$day <- sapply(new.data$date, FUN=weekend)

av <- aggregate(steps ~ interval + weekdays, data = new.data, mean)

png("4.png", width=600, height=700)
ggplot(av, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
     ggtitle("
             Average number of steps taken per 5-minute interval across weekdays and weekends
             ")
     xlab("5-minute interval")+
     ylab("Number of steps")

dev.off()
