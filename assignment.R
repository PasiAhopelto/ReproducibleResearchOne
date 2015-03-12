library(data.table)
library(ggplot2)
library(grid)

## Loading and processing data
data <- read.csv(file = "activity.csv")
activityDataPerDay <- data.table(data) [, 1:2, with = FALSE]
activityDataPerInterval <- data.table(data) [, c(1,3), with = FALSE]

## Total number of steps per day
stepsPerDay <- aggregate(. ~ date, data=activityDataPerDay, FUN=sum)
plot <- ggplot(stepsPerDay, aes(x=steps)) + geom_histogram(colour="black", fill="white") + scale_y_continuous(labels = function (x) ceiling(x))
print(plot)
meanPerDay <- mean(stepsPerDay$steps)
medianPerDay <- median(stepsPerDay$steps)

## Average daily pattern
stepsPerInterval <- aggregate(. ~ interval, data=activityDataPerInterval, FUN=mean)
plot <- ggplot(stepsPerInterval, aes(x=interval, y=steps)) + geom_line()
print(plot)
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
plot <- ggplot(stepsPerDayWithImputedValues, aes(x=steps)) + geom_histogram(colour="black", fill="white") + scale_y_continuous(labels = function (x) ceiling(x))
print(plot)
meanPerDayWithImputedValues <- mean(stepsPerDayWithImputedValues$steps)
medianPerDayWithImputedValues <- median(stepsPerDayWithImputedValues$steps)

## Are there differences in activity patterns between weekdays and weekends
dataWithDayInfo <- dataWithNasFilledIn
dataWithDayInfo[, 'day'] <- weekdays(as.Date(dataWithDayInfo$date))
dataWithDayInfo[dataWithDayInfo$day == 'lauantai' | dataWithDayInfo$day == 'sunnuntai', c('day')] <- "weekend"
dataWithDayInfo[dataWithDayInfo$day != 'weekend', c('day')] <- "weekday"
dataWithDayInfo$day <- as.factor(dataWithDayInfo$day)

weekendData <- dataWithDayInfo[dataWithDayInfo$day == 'weekend',]
weekdayData <- dataWithDayInfo[dataWithDayInfo$day == 'weekday',]
weekendStepsPerInterval <- aggregate(. ~ interval, data=weekendData, FUN=mean)
weekdayStepsPerInterval <- aggregate(. ~ interval, data=weekdayData, FUN=mean)
weekendPlot <- ggplot(weekendStepsPerInterval, aes(y=steps, x=interval)) + geom_line() + ggtitle("Weekends")
weekdayPlot <- ggplot(weekdayStepsPerInterval, aes(y=steps, x=interval)) + geom_line() + ggtitle("Weekdays")
layout <- matrix(seq(1, 2), ncol = 1, nrow = 2)
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
print(weekendPlot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(weekdayPlot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))