##Bring the data in

unzip("activity.zip")
activity <- read.csv("activity.csv")

#Make a histogram of mean steps per day
meansteps <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
hist(meansteps$steps, col = "Blue", main = "Daily steps", xlab = "Total steps" )

#calculate mean and median values

mean(meansteps$steps)
median(meansteps$steps)

#calculate interval with highest frequency of steps

interval <- aggregate(steps ~ interval, activity, FUN = mean, na.rm = TRUE)
plot(interval, main = "Number of steps per 5 minute interval",type = "l", xlab = "5 minute interval",
     ylab = "Average number of steps")

#max interval calculation

interval$interval[which.max(interval$steps)]

#imputing missing values

sum(is.na(activity)) #also check using summary

#average over the 5 minute intervals

activity <- merge(activity, interval, by = "interval", suffixes = c("", 
                                                                          ".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[, c(1:3)]

#Reaggregate and redo histogram and mean and median values.
total2 <- aggregate(steps ~ date, data = activity, FUN = sum)
hist(total2$steps, col = "Blue", main = "Daily steps", xlab = "Total steps" )

mean(total2$steps)
median(total2$steps)

#Differences between weekends and weekdays

library(chron)
activity$weekend <- is.weekend(as.Date(activity$date))
days <- aggregate(steps ~ weekend + interval, activity, FUN = mean)


with(days, plot(interval, steps, main = "Activity Levels - Weekend versus Weekday", type = "n"))
with(subset(days, weekend == TRUE), points(interval, steps, type = "l", col = "blue"))
with(subset(days, weekend == FALSE), points(interval, steps, type = "l",col = "red"))
legend("topright", pch = 10, col = c("blue", "red"), legend = c("Weekend", "Weekday"))




