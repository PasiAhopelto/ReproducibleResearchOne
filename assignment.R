library(data.table)
library(ggplot2)

## Loading and processing data
data <- read.csv(file = "activity.csv")
activityDataPerDay <- data.table(data) [, 1:2, with = FALSE]
activityDataPerInterval <- data.table(data) [, c(1,3), with = FALSE]

## Total number of steps per day
stepsPerDay <- aggregate(. ~ date, data=activityDataPerDay, FUN=sum)
ggplot(stepsPerDay, aes(x=steps)) + geom_histogram(colour="black", fill="white") + scale_y_continuous(labels = function (x) ceiling(x))
ggsave("figures/steps-histogram.png", width=4, height=4, dpi=100)
meanPerDay <- mean(stepsPerDay$steps)
medianPerDay <- median(stepsPerDay$steps)

## Average daily pattern
stepsPerInterval <- aggregate(. ~ interval, data=activityDataPerInterval, FUN=mean)
ggplot(stepsPerInterval, aes(x=interval, y=steps)) + geom_line()
ggsave("figures/steps-per-interval.png", width=4, height=4, dpi=100)
intervalWithMaxAverageSteps <- stepsPerInterval[stepsPerInterval$steps == max(stepsPerInterval$steps),]

## Imputing missing values
rowsWithMissingDates <- nrow(data[is.na(data$date),])
rowsWithMissingInterval <- nrow(data[is.na(data$interval),])
rowsWithMissingSteps <- nrow(data[is.na(data$steps),])
dataWithNasFilledIn <- data
replacementStragegy <- function(interval) {
  stepsPerInterval[stepsPerInterval$interval == interval, c('steps')]
}
dataWithNasFilledIn[is.na(dataWithNasFilledIn$steps), c('steps')] <- replacementStragegy(stepsPerInterval$interval)
activityDataPerDayWithImputedValues <- data.table(dataWithNasFilledIn) [, 1:2, with = FALSE]
stepsPerDayWithImputedValues <- aggregate(. ~ date, data=activityDataPerDayWithImputedValues, FUN=sum)
ggplot(stepsPerDayWithImputedValues, aes(x=steps)) + geom_histogram(colour="black", fill="white") + scale_y_continuous(labels = function (x) ceiling(x))
ggsave("figures/steps-histogram-with-imputed-values.png", width=4, height=4, dpi=100)
meanPerDayWithImputedValues <- mean(stepsPerDayWithImputedValues$steps)
medianPerDayWithImputedValues <- median(stepsPerDayWithImputedValues$steps)

## Are there differences in activity patterns between weekdays and weekends
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
