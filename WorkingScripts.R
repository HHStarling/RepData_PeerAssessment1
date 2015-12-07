## Load dependencies
library(dplyr)

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
allData$dateTime <- paste(allData$date, allData$interval)
allData$dateTime <- as.Date(allData$dateTime, format = "%Y-%m-%d %M")

## Add column with time based on interval column
mutate(allData, obsTime=strptime(allData$interval, format="%k"))

## Mean total steps taken per day
### Calculate total number of steps per day
days <- group_by(allData, date)
summaryDays <- summarize(days, totalSteps=sum(steps), meanSteps=mean(steps, na.rm=TRUE), medSteps=median(steps, na.rm=TRUE))
