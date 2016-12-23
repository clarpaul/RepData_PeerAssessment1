# Commands for RepData_PeerAssessment1

## Setup

# load packages used in processing and charting

library(dplyr)
library(ggplot2)

# create directory for figures
if (!file.exists("figures")) {dir.create("figures")}

## 1. Reading in the dataset and pre-processing the data

conxn <- unz("activity.zip","activity.csv")

# reads column 1 (steps), column 2 (day's date), column 3 (interval)
# note read.csv closes conxn after use
activity <- read.csv(conxn, colClasses = c("integer", "character", "integer"))
rm(conxn)




## 2. Histogram of number of steps taken each day

# Group dataframe by date and summarize over steps
# Note: It's very important to set na.rm = FALSE in 'sum', as otherwise summarize will include
# 'NA' dates as having sum(steps) == 0
stepsbydt_na <- activity %>% group_by(date) %>% summarize(StepsPerDay =
                                                               sum(steps, na.rm = FALSE))
# Plot histogram of steps taken each day. Note: let na.rm = FALSE so that explicit warning message
# generated, with count of dates that have sum(steps) == NA
histNoNA <- ggplot(stepsbydt_na, aes(StepsPerDay/1000)) + stat_bin(binwidth = 1, center = 0.5,
        closed = "right", na.rm = FALSE) + 
        scale_x_continuous(minor_breaks = seq(0,22,1), labels = function(x){paste0(x, "K")}) +
        scale_y_continuous(breaks = seq(0,20,2)) +
        labs(x = "Thousand Steps per Day", y = "Number of Days", title =
        "Histogram of Steps Taken per Day (Excluding NAs)")
print(histNoNA)
png("figures/histNoNA.png", width = 1000)
print(histNoNA)
dev.off()


## Mean and median number of steps taken each day
summary(stepsbydt_na$StepsPerDay, na.rm = FALSE)




## 4. Time series plot of the average number of steps taken

# Group_by interval, then average steps across all days for given interval.
# Note: here we remove NAs, so that we don't generate any results with mean(steps) == NA.
# We also normalize the "steps" value as average steps per minute, by dividing by 5.
stepsbyint <- activity %>% group_by(interval) %>% summarize(StepsPerMin =
                                       mean(steps, na.rm = TRUE)/5)
# function to change interval into a character time of day
mktime <- function(tm) {
        digit4_tm <- paste0(paste0(rep("0", 4 - nchar(tm)), collapse = ""),
                            tm, collapse = "")
        digit4_tm <- paste0(substr(digit4_tm, start = 1, stop = 2), ":",
                            substr(digit4_tm, start = 3, stop = 4))
}
#Vectorize fn and use in transmute
vtime <- Vectorize(mktime, SIMPLIFY = TRUE, USE.NAMES = FALSE)
stepsbyintchar <- stepsbyint %>% transmute(time = vtime(interval), StepsPerMin)
# convert character time to actual dttm (POSIXct) value
stepsbyinttm <- stepsbyintchar %>% transmute(time = as.POSIXct(time, format = "%H:%M"),
                                              StepsPerMin)
# plot average number of steps taken over course of day, with time shown in clock hours
stepTimeSeriesAvg <- ggplot(stepsbyinttm, aes(x = time, y = 5*StepsPerMin)) + geom_line() +
        scale_x_datetime(date_labels = "%I %p") + labs(x = "Time of Day",
        y = "Average Steps Per 5-Min Interval", 
        title = "Average steps per 5-min interval throughout day")
print(stepTimeSeriesAvg)
png("figures/stepTimeSeriesAvg.png", width = 1000)
print(stepTimeSeriesAvg)
dev.off()



## 5. The 5-minute interval that, on average, contains the maximum number of steps

stepsbyintchar[which.max(stepsbyintchar$StepsPerMin),]




## 6. Impute missing data

## 6.1 Total number of missing values in original data set

summary(activity$steps, na.rm = FALSE)
# Note that this is exactly equal to 8 days of missing data
sum(is.na(activity$steps))/(12*24)
# missing data is from the following 8 dates
with(activity, unique(date[is.na(steps)]))




## 6.3 Create new dataset with missing values filled in

# Though we know the exact location of all the NAs, we use a general procedure to
# replace them in the original data

# create vector of intervals needing imputation
NAintervals <- activity[is.na(activity$steps), "interval"]
# Create vector of indices (in the mean steps per interval dataframe)
# of the intervals needing imputation (i.e., which contain NA values).  
# There will be one value indexing the position of the interval associated with each NA.
# The length of the vector will be the number of NAs in the dataset.
NAindices <- match(NAintervals, stepsbyint$interval)
length(NAindices)

# Use the index values in 'NAindices' to replace the NA values in the orginal data-set
# with those in the mean step per interval dataframe. Multiply by 5, since we normalized
# the interval data in 'stepsbyint' to a value in average steps per minute.
activityimp <- activity
activityimp$steps[is.na(activity$steps)] <- 5*(stepsbyint$StepsPerMin[NAindices])





## 7. Make histogram of no. steps/day after imputation; report on changes in mean, median

# Group dataframe by date and summarize over steps. Since there should be no NAs, we
# let na.rm = FALSE in sum(), so that a warning is generated in the event we have not
# replaced all NAs with imputed data.
stepsbydtimp <- activityimp %>% group_by(date) %>% summarize(StepsPerDay =
                                                               sum(steps, na.rm = FALSE))
# Plot histogram of steps taken per day. Note that 8 days of imputed data have been
# added to the interval containing the median & mean steps per day
histNAimputed <- ggplot(stepsbydtimp, aes(StepsPerDay/1000)) + stat_bin(binwidth = 1, center = 0.5,
        closed = "right", na.rm = TRUE) +
        scale_x_continuous(minor_breaks = seq(0,22,1), labels = function(x){paste0(x, "K")}) +
        scale_y_continuous(breaks = seq(0,20,2)) +
        labs(x = "Thousand Steps Per Day", y = "Number of Days", title =
        "Histogram of Steps Taken Per Day (NAs Imputed)")
print(histNAimputed)
png("figures/histNAimputed.png", width = 1000)
print(histNAimputed)
dev.off()

# Mean and median number of steps taken each day
summary(stepsbydtimp$StepsPerDay, na.rm = FALSE)
# Calculate difference between imputed and non-imputed data on mean and median.  
# We must remove the NAs to obtain the mean & median.
mean(stepsbydtimp$StepsPerDay) - mean(stepsbydt_na$StepsPerDay, na.rm = TRUE)
median(stepsbydtimp$StepsPerDay) - median(stepsbydt_na$StepsPerDay, na.rm = TRUE)

# By adding the imputed values, the mean did not change at all.  This is to be expected, since
# all 8 days with NA values had imputed means equal to the population mean prior to imputing NAs.
# However, the median Steps Per Day moves up slightly, since the original mean is slightly
# higher than the original median.
summary(stepsbydt_na$StepsPerDay)

# We can see these changes to the distribution most clearly if we do a 2-panel plot of the
# Histograms before and after imputing values for the NAs.

# We first compute a factor variable that will be used to differentiate the two histograms
# in ggplot, then combine the two dataframes (with and without imputed data), and call ggplot.
NA_Treatment <- as.factor(c(rep("NAs Excluded", nrow(stepsbydt_na)),
                            rep("NAs Imputed", nrow(stepsbydtimp))))
stepsbydt_impexc <- rbind(stepsbydt_na, stepsbydtimp)
stepsbydt_impexc$NA_Treatment <- NA_Treatment
histPanelNAimputed <- ggplot(stepsbydt_impexc, aes(StepsPerDay/1000)) + stat_bin(binwidth = 1,
        center = 0.5, closed = "right", na.rm = FALSE) + 
        scale_x_continuous(minor_breaks = seq(0,22,1), labels = function(x){paste0(x, "K")}) +
        scale_y_continuous(breaks = seq(0,20,4)) +
        facet_grid(NA_Treatment~.) +
        labs(x = "Thousand Steps Per Day", y = "Number of Days", title =
        "Histograms by Treatment of NAs: Number of Days by Thousand Steps")
print(histPanelNAimputed)
png("figures/histPanelNAimputed.png")
print(histPanelNAimputed)
dev.off()



## 8.  Panel plot comparing the avg # steps per 5-min across weekdays & weekends

# (a) Convert the 'date' column of 'activityimp' dataframe to POSIXct format

activityimp$posixdate <- with(activityimp, as.POSIXct(date))

# (b) From 'posixdate' column, create a 'dayofwk' column

activityimp$dayofwk <- with(activityimp, weekdays(posixdate, abbreviate = TRUE))

# (c) From the 'date' column, use fn ifelse() to create colum 'daytype' containing 
#     factor variable with two levels: 'weekday' and 'weekend'

activityimp$daytype <- with(activityimp, as.factor(ifelse(dayofwk == "Sat" | dayofwk == "Sun",
                                                "weekend","weekday")))

# (d) Use dplyr to simultaneously call 'group_by' on the 'activityimp' dataframe with arguments
#     'interval' and 'daytype', then call 'summarize' on 'steps' by mean

# Ggroup_by interval and daytype, then average steps. For consistency with previous dataframes,
# we normalize to mean steps per minute.  We convert back to steps per 5-min interval for
# the plots explicitly requested to be number of steps per 5-minute interval.
stepsbyintdytype <- activityimp %>% group_by(interval, daytype) %>% summarize(StepsPerMin =
                                                                    mean(steps, na.rm = TRUE)/5)
# Leverage previous function 'mktime' to change intervals into character times of day.
# In particular, leverage vectorized form of fn, 'vtime',  for use in dplyr 'transmute'.
stepsbyintchardytype <- stepsbyintdytype %>% transmute(interval, time = vtime(interval), 
                                                       StepsPerMin, daytype)
# Convert character time to actual dttm value, which is needed so that ggplot can read the time.
# Note that all times will of necessity be accorded a date, as required by POSIXct. However, 
# the date is of no consequence, so long as we do not use a day with changes to/from Daylight
# Savings Time!  We prepare the variable 'time' by appending the first date of the period in
# question, on which we know no conversion to/from DST occurs.

stepsbyintchardytype$plottime <- paste("2012-10-01", stepsbyintchardytype$time)

# Note: 'as.date.frame()' needed to remove grouping information prior to transmuting
stepsbytmdytype <- stepsbyintchardytype %>% as.data.frame %>% transmute(plottime =
                as.POSIXct(plottime, format = "%Y-%m-%d %H:%M"), StepsPerMin, daytype)

# Plot average number of steps taken per 5-Min period in panel plot comparing 
# weekday and weekend steps
stepTimeSeriesPanel <- ggplot(stepsbytmdytype, aes(x = plottime, y = 5*StepsPerMin)) + 
        facet_grid(daytype ~.) + geom_line() +
        scale_x_datetime(date_labels = "%I %p") + labs(x = "Time of Day",
        y = "Average Steps Per 5-Min Interval",
        title = "Average steps per 5-min interval throughout day: Weekdays vs. Weekends")
print(stepTimeSeriesPanel)
png("figures/stepTimeSeriesPanel.png", width = 800)
print(stepTimeSeriesPanel)
dev.off()

# By eye-balling the charts, one can see that on weekends (as compared to weekdays) our walker
# is slower getting started, has lower maximum steps per interval, walks fewer steps
# throughout the day, and, finally, winds down more slowly (distribution of steps has its final
# local maximum later in the day, e.g., around 8PM or so on weekends vs. 7PM on weekdays).







summary(activity$interval/60) # summarize distribution of intervals in fractional hours
sum(is.na(activity$steps))/nrow(activity) # calc fraction of intervals for which steps = NA
str(activity)
hist(activity$steps)
hist(activity$date)
rug(unique(activity$date)) # show no. of date values in dataset beneath hist of dates
range(activity$date)


# show histogram of dates: highlight missing values, show intervals exceeding 24 hours
library(ggplot2)
activity$time <- ifelse(activity$interval <= 60*24 - 5, "0 - 24 hrs", "> 24 hrs")
activity$obs <- as.integer(row.names(activity))
ggplot(activity[complete.cases(activity),], aes(x = date, col = time)) + 
        geom_histogram(binwidth = 1) + 
        labs(title = paste("Data is missing for seven specific dates.",
                           "All dates have more than 24 hours of data.")) + 
        labs(y = "measurements")

# plot interval label vs. observation number
activity$intervalnum <- rep(1:288, times = 61)
ggplot(activity[1:288,], aes(x = intervalnum, y = interval/5)) + geom_point()

activity[1:288, c("intervalnum", "interval")]



sum(activity$interval > 60*24)/sum(activity$interval >= 0)
# avg steps per minute
sum(activity$steps, na.rm = TRUE)/sum(activity$interval[!is.na(activity$steps)])



## What is mean total  number of steps taken per day

stepsperday <- aggregate(steps ~ date, activity, "sum")
head(stepsperday)
hist(stepsperday$steps, breaks = 10, xlim = c(0,25000))
rug(stepsperday$steps)
summary(stepsperday$steps)

## What is the average daily activity pattern

stepsperinterval <- aggregate(steps ~ interval, activity, "mean")
head(stepsperinterval)
# show steps per minute as function of hours from start of day
with(stepsperinterval, plot(interval/60, steps/5, type = "l"))
abline(v = 24)

# show interval with max mean steps
stepsperinterval[which.max(stepsperinterval$steps),]

# show steps per minute for max mean steps, and summary of steps per minute
summary(stepsperinterval$steps/5)
