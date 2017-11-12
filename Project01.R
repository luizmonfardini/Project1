#Download the file
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/repdata%2Fdata%2Factivity.zip",method="curl")

#Unzip the file and create path_rf and files
unzip(zipfile="./data/repdata%2Fdata%2Factivity.zip",exdir="./data")
path_rf <- file.path("./data" , "Dataset")
files <- list.files(path_rf, recursive=TRUE)

# Read the data
data <- read.csv("./data/activity.csv", header = TRUE, sep = ",", na.strings = "NA")

# Convert data format
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

# Ignore Missing Values
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

# Data frame with the steps for a day
steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)

# Add column names
colnames(steps_each_day) <- c("date", "steps")

# Histogram of dataset
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "blue", xlab = "Number of Steps", main= "Histogram of the steps taken each day")

# Mean
mean(steps_each_day$steps)

# Median
median(steps_each_day$steps)

# Average
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)

# Add columns names
colnames(steps_per_interval) <- c("interval", "average_steps")

# Plot the average daily activity pattern 
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

# Maximum number of average steps
max_steps <- max(steps_per_interval$average_steps)
max_steps

# 5-min interval that contains the max number of steps
intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
intervale_max_steps

# Input missing values
sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))
sum(is.na(as.character(data$interval)))

# Find the indices of missing values (NAs)
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data

# Input missing values using the mean for that 5-min
complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
  steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
}))

# Check the complete data with the summary and str
summary(complete_data)
str(complete_data)

# Create a data frame with the steps taken for each day
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)

# Add column names to the created data frame
colnames(steps_each_day_complete) <- c("date", "steps")

# Histogram
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "blue", xlab = "Number of Steps", main= "Histogram of the steps taken each day")

# Mean
mean(steps_each_day_complete$steps)

# Median
median(steps_each_day_complete$steps)

# Create a factor variable
complete_data$day <- as.factor(weekdays(complete_data$date))

# Create a logical variable
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Sabado","Domingo")), TRUE, FALSE) 

# Calculate the avg number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by = list(interval = weekdays_data$interval), FUN = mean)

# Calculate the avg number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by = list(interval = weekends_data$interval), FUN = mean)

# Add columns names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

# Add a column
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

# Merge
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)

# Convert
week_data$day <- as.factor(week_data$day)

# Plot
library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")