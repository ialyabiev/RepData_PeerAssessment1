
# LIBRARIES ---------------------------------------------------------------
library(lattice)  
# PART I -----------------------------------------------------------------
# Loading and preprocessing the data
#  1. Load the data (i.e. read.csv())
#  2. Process/transform the data (if necessary) into a format 
#     suitable for your analysis

datafile <- "data/activity.csv"
dt <- read.table(datafile, sep=",", header = T)

#convert 'date' into system date and time format.
#check :format(Sys.Date(), "%Y-%m-%d") 
#dt <- transform(dt, date = strptime(dt$date, "%Y-%m-%d"))


# PART II  -----------------------------------------------------------------
# What is mean total number of steps taken per day?
# Ignore( if not cleaned) the missing values in the dataset.
 
#  3. Make a histogram of the total number of steps taken each day
#  4. Calculate and report the 
#       mean and median total number of steps taken per day

steps.daily <- aggregate(steps ~ date,data=dt, FUN=sum, na.action = na.omit)[,2]
#length(steps.daily)



l <- min(steps.daily)
u <- max (steps.daily)
breaks = seq(l,u,length.out = 31)
histogram(steps.daily, breaks = breaks, col = "palegreen",
          xlab="Interval", ylab = "Number of steps", 
          main="Total number of steps taken each day")

#hist(steps.daily, breaks , labels = T, right = T, col = "palegreen", 
#     main = "Total number of steps taken each day")

mean(steps.daily)
median(steps.daily)


# PART III  -----------------------------------------------------------------
# What is the average daily activity pattern?

# 5. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken,  averaged across all days (y-axis)
# 6. Which 5-minute interval, on average across all the days in the dataset, 
#       contains the maximum number of steps?

steps.average = aggregate(steps ~ interval,data=dt, FUN=mean);


xyplot(steps~interval, steps.average, type = "l",
       xlab="Interval", ylab = "Number of steps", 
       main="Number of steps averaged across all days")

#with(steps.interval.average,
#     plot(steps~interval, type = "l")
#)
ind <- which(steps.average[,2] == max(steps.interval.average[,2]))     
steps.interval.average[ind,1]


# PART IV -----------------------------------------------------------------
# Imputing missing values

# 7. Calculate and report the total number of missing values in the dataset 
#         (i.e. the total number of rows with NAs)
# 8. Devise a strategy for filling in all of the missing values in the dataset. 
#       The strategy does not need to be sophisticated. 
#       For example, you could use the mean/median for that day,
#       or the mean for that 5-minute interval, etc.
# 9. Create a new dataset that is equal to the original dataset 
#      but with the missing data filled in.
# 10. Make a histogram of the total number of steps taken each day and 
#         -Calculate and report the mean and median total number of steps
#         taken per day. 
#         -Do these values differ from the estimates from 
#         the first part of the assignment? 
#         -What is the impact of imputing missing data on the estimates 
#         of the total daily number of steps?

#sum(is.na(dt[,1]))
sum(sapply(dt$steps,is.na))

steps2 <-cbind( steps.average = steps.average[,2],steps <- dt[,1])
na2mean <- function(x){ if(is.na(x[2]))( x[1]) else (x[2]) }
steps.new <- apply(steps2,1, na2mean)
dt.new <- data.frame(steps = steps.new,dt[,2:3])
#dt.new

steps.daily <- aggregate(steps ~ date,data=dt.new, FUN=sum)[,2]
l <- min(steps.daily)
u <- max (steps.daily)
breaks = seq(l,u,length.out = 31)
histogram(steps.daily, breaks = breaks, col = "palegreen",
          xlab="Interval", ylab = "Number of steps", 
          main="Total number of steps taken each day, no missing values")

#hist(steps.daily, breaks , labels = T, right = T, col = "palegreen", 
#     main = "Total number of steps taken each day")
#length(steps.daily)
mean(steps.daily)
median(steps.daily)



# PART V ------------------------------------------------------------------
# Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.

# 11. Create a new factor variable in the dataset with two levels – 
#         “weekday” and “weekend” indicating whether a given date is a 
#         weekday or weekend day.
# 
# 12. Make a panel plot containing a time series plot (i.e. type = "l") 
#         of the 5-minute interval (x-axis) and the average number of steps taken,
#         averaged across all weekday days or weekend days (y-axis). 
#         The plot should look something like the following, which was creating 
#         using simulated data:

Sys.setlocale("LC_TIME", "English")

wd <- strptime(dt.new$date, "%Y-%m-%d")$wda
wd <- sapply(wd, function(x){if(x %in% 1:5) "weekday" else "weekend" })
dt.new <- data.frame(dt.new,wd)

steps.average = aggregate(steps ~ wd + interval,data=dt, FUN=mean);
#qplot(interval,steps,data = steps.interval.average, facets =.~wd,geom = c("line"))

xyplot(steps ~ interval | wd, data = steps.average, layout = c(1, 2),
       type = "l", ylab="Number of steps", xlab = "Interval")

