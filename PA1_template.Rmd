## Assignment Instructions.

1.Code for reading in the dataset and/or processing the data.

2.Histogram of the total number of steps taken each day.

3.Mean and median number of steps taken each day.

4.Time series plot of the average number of steps taken.

5.The 5-minute interval that, on average, contains the maximum number of steps.

6.Code to describe and show a strategy for imputing missing data.

7.Histogram of the total number of steps taken each day after missing values are imputed.

8.Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

9.All of the R code needed to reproduce the results (numbers, plots, etc.) in the report.

## Step 1

### Code for reading in the dataset and/or processing the data.
```{r, echo = TRUE}
setwd("C:/Users/thang/Desktop/Coursera/course5/RepData_PeerAssessment1")
activity<-read.csv("activity.csv")
head(activity)
```

### Exploring the basics of this data.
```{r}
dim(activity)
names(activity)
head(activity)
str(activity)
#total number of missing data
sum(is.na(activity$steps))/dim(activity)[[1]]
#transforming the date column into date format using lubridate
library(lubridate)
activity$date<-ymd(activity$date)
length(unique(activity$date))
```

## Step 2

### Histogram of the total number of steps taken each day.
```{r, echo = TRUE}
library(magrittr)
library(dplyr)
databydate <- activity %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()
hist(databydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
```

## Step 3

### Mean and median number of steps taken each day.

```{r, echo = TRUE}
library(dplyr)
mean(databydate$tsteps)
median(databydate$tsteps)
```

## Step 4

### Average daily activity pattern.

### Time series plot of the average number of steps taken.

```{r, echo = TRUE}
library(ggplot2)
databyinterval <- activity%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 
ggplot(databyinterval, aes(x=interval, y=tsteps))+ geom_line()
```

## Step 5

### The 5-minute interval that, on average, contains the maximum number of steps.

```{r, echo = TRUE}
#This is assuming that the words on average means averaging steps by date and interval
databyinterval[which(databyinterval$tsteps== max(databyinterval$tsteps)),]
```

## Step 6

### Code to describe and show a strategy for imputing missing data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r,echo=TRUE}
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps))))
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r,echo=TRUE}
#Before 
#Display first 10 rows of data
head(activity,10)
```

```{r,echo=TRUE}
library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- activity%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

```

## Step 7

### Histogram of the total number of steps taken each day after missing values are imputed.

```{r,echo=TRUE}
FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
```

```{r,echo=TRUE}
#Summary
summary(FullSummedDataByDay)
```

```{r,echo=TRUE}
hist(FullSummedDataByDay$totalsteps, main = "Histogram of Daily Steps", 
     col="blue", xlab="Steps")
```

### Comparing mean,median of new and old data.

```{r,echo=TRUE}
oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
# Old mean and New mean
oldmean
newmean
```

```{r,echo=TRUE}
oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
# Old median and New median
oldmedian
newmedian
```

## Step 8

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends.

```{r,echo=TRUE}
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
```

```{r,echo=TRUE}
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
```

