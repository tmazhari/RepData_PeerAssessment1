### PART 1 - Loading and preprocessing the data ###
## Loading the data ##
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./data/projData.zip")
unzip(zipfile="./data/projData.zip",exdir="./data")
filePath <- "./data/activity.csv"
## Preprocessing the data ##
data1 <- read.csv(filePath, sep=",",header = TRUE)
head(data1, n=3)
str(data1)
# Transforming the date from Factor to date format
data1$date <- as.Date(data1$date, "%&Y-%m-%d")
str(data1)


### PART 2 - What is mean total number of steps taken per day? ###
## Calculate the total steps per day ##
totalStepsPerDay <- aggregate(data2$steps,list(date=data2$date),sum,na.rm=TRUE)
colnames(totalStepsPerDay) <- c("date", "totalSteps")
## Histogram of the total number of steps taken each day ##
hist(totalStepsPerDay$totalSteps, breaks=seq(from=0, to=25000, by=2000), col="red", main="Total number Of Steps Taken Each Day", xlab="Steps", ylab="Frequency")
## Mean and median of total steps per day
summary(totalStepsPerDay$totalSteps)['Mean']
summary(totalStepsPerDay$totalSteps)['Median']


### PART 3 - What is the average daily activity pattern? ###
## Average steps per interval ##
avgStepsPerInterval <- aggregate(x=list(avgSteps=data1$steps), by=list(interval=data1$interval), FUN=mean, na.rm=TRUE)
library(ggplot2)
avgStepsPerIntPlot <- ggplot(avgStepsPerInterval, aes(interval, avgSteps))
avgStepsPerIntPlot <- avgStepsPerIntPlot + geom_line()
print(avgStepsPerIntPlot)
## The interval with maximum steps ##
maxPosition <- which(avgStepsPerInterval$avgSteps == max(avgStepsPerInterval$avgSteps))
maxInterval <- avgStepsPerInterval[maxPosition, 1]


### PART 4 - Imputing missing values ###
## Total number of missing values ##
table(complete.cases(data1))
## Filling missing value and creating the imputed dataset ##
install.packages("Hmisc")
install.packages("survival")
library(survival)
library(Hmisc)
imputedData <- data2
imputedData$steps <- impute(data1$steps, fun=mean)
## Histogram of the total number of steps taken each day from the imputed dataset ##
totalStepsPerDay_imputed <- aggregate(imputedData$steps,list(date=imputedData$date),sum,na.rm=TRUE)
colnames(totalStepsPerDay_imputed) <- c("date", "totalSteps")
## Histogram of the total number of steps taken each day ##
hist(totalStepsPerDay_imputed$totalSteps, breaks=seq(from=0, to=25000, by=2000), col="red", main="Total number Of (Imputed) Steps Taken Each Day", xlab="Steps", ylab="Frequency")
## Mean and median of total steps per day from imputed dataset
summary(totalStepsPerDay_imputed$totalSteps)['Mean']
summary(totalStepsPerDay_imputed$totalSteps)['Median']
# Explain if the values differ and what is the impact of imputing


### PART 5 - Are there differences in activity patterns between weekdays and weekends? ###
## Creating the weekend-weekday factor ##
imputedData$day <- weekdays(imputedData$date)
weekDays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
weekendDays <- c('Saturday', 'Sunday')
imputedData$dayType <- ifelse(imputedData$day %in% weekDays, 'weekday', 'weekend')
head(imputedData)
## Average steps averaged across weekday days and weekends days per interval ##
avgStepsPerInterval2 <- aggregate(imputedData$steps, by=list(imputedData$dayType, imputedData$day, imputedData$interval), mean)
colnames(avgStepsPerInterval2) <- c("dayType", "day", "interval", "avgSteps")
# Drwing the plot
avgStepsPerIntPlot2 <- ggplot(avgStepsPerInterval2, aes(interval, avgSteps))
avgStepsPerIntPlot2 <- avgStepsPerIntPlot + geom_line()
avgStepsPerIntPlot2 <- avgStepsPerIntPlot2 + facet_grid(dayType ~ .)
print(avgStepsPerIntPlot2)