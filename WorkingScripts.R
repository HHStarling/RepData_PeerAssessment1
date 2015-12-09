## Load dependencies
library(dplyr)
library(ggplot2)

## Pull data from online location
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileNm <- "activity.csv"
temp <- tempfile()
download.file(fileUrl,temp)
allData <- tbl_df(read.table(unz(temp, fileNm), sep=",", header=TRUE, 
                             stringsAsFactors = FALSE, na.strings = c("?","","NA")))
unlink(temp)

## Processing code to create analytic data
## Review data set
head(allData)
tail(allData)
summary(allData)
str(allData)

## Change date column to be date data type
allData$date <- as.Date(allData$date, format = "%Y-%m-%d")
## allData$time <- as.difftime(allData$interval, units="mins")

## Mean total steps taken per day
### Calculate total number of steps per day
days <- group_by(allData, date)
## summaryDays <- summarize(days, totalSteps=sum(steps), meanSteps=mean(steps, na.rm=TRUE), medSteps=median(steps, na.rm=TRUE))
summaryDays <- summarize(days, totalSteps=sum(steps))
meanSteps <- mean(summaryDays$totalSteps, na.rm=T)
medSteps <- median(summaryDays$totalSteps, na.rm=T)

## histogram of total number of steps per day
hist(summaryDays$totalSteps, xlab="Number of Steps", main="Histogram # Steps per day (total)")
abline(v=meanSteps, col="red", lwd=3, lty=3)
abline(v=medSteps, col="blue")
rug(summaryDays$totalSteps)
text(x=meanSteps, y=25, pos=4, labels=paste("mean",meanSteps,sep="="), col="red")
text(x=medSteps, y=20, pos=4, labels=paste("median",medSteps,sep="="), col="blue")

## average daily activity pattern
patterns <- group_by(allData, interval)
summaryPatterns <- summarize(patterns, avgSteps=mean(steps, na.rm=T))

## plot intervals by average steps
## find interval with max average number steps
intMaxRow <- which.max(summaryPatterns$avgSteps)
xValue <- summaryPatterns$interval[intMaxRow]
        
## uses ggplot library
g <- ggplot(summaryPatterns, aes(interval, avgSteps))
g <- g + labs(title="Avg Daily Activity Pattern") + labs(x="5 Minute Interval") + labs(y="Average # Steps")
g <- g + annotate("text", x=xValue + 500, y=175, label=paste("max=interval",xValue), col="red")
g <- g + geom_vline(xintercept=xValue, col="red")
g <- g + geom_line()
print(g)

## inputting missing values
## total number of NAs
colSums(is.na(allData))

## replace NAs with mean value for that interval over the data set
## identify NA rows in logical vector
goodData <- complete.cases(allData)









## ??
## update allData NA values
completeData <- allData
completeData[!goodData,completeData$steps] <- 
        summaryPatterns[summaryPatterns$interval,summaryPatterns$avgSteps]

## add rownames matching interval
row.names(summaryPatterns) <- summaryPatterns$interval




completeData[is.na(completeData)] <- summaryPatterns$






