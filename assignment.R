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
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the missing data filled in.
# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

## Are there differences in activity patterns between weekdays and weekends
# Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.