# Untitled
Rafaela Becerra Robalino  
26 de junio de 2017  

##Loading and preprocessing the data

Show any code that is needed to
1.Load the data (i.e. read.csv())

```r
library("knitr")
opts_chunk$set(echo=TRUE, warning=FALSE, comment=NA,message=FALSE)
setwd("C:/Users/rafaela.becerra/Desktop/Reproductible research/Week 2/Assignment")
```


2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
data$date<-as.Date(data$date, "%Y-%m-%d")
```
##What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.
1.Calculate the total number of steps taken per day

```r
data1<-data%>%group_by(date)%>%summarise(sum_steps=sum(steps, na.rm=TRUE))
head(data1)
```

```
# A tibble: 6 x 2
        date sum_steps
      <date>     <int>
1 2012-10-01         0
2 2012-10-02       126
3 2012-10-03     11352
4 2012-10-04     12116
5 2012-10-05     13294
6 2012-10-06     15420
```
2. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
plot1<-ggplot(data1,aes(data1$sum_steps))+geom_histogram()+labs(title="Histogram for steps", x="Steps per day")
plot(plot1)
```

![](PA1_template_files/figure-html/scatterplot-1.png)<!-- -->

3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean<-mean(data$steps, na.rm=TRUE)
median<-median(data$steps, na.rm=TRUE)
```
The mean of steps per day is 37.3825996. the median for the same data is 0

##What is the average daily activity pattern?
1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data2<-data%>%group_by(interval)%>%summarise(mean_steps=mean(steps, na.rm=TRUE))
plot2<-plot(data2$interval, data2$mean_steps, type="l", ylab="Mean Steps", xlab="Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max<-data2[data2$mean_steps==max(data2$mean_steps),1]
```
The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835

##Imputing missing values

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
data3<-data[is.na(data$steps),]
NAS<-length(data3$steps)
```
The number of missing values is 2304

2.Strategy for filling in all of the missing values in the dataset by using the mean for that 5-minute interval.
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataNONA<-data[!is.na(data$steps),]
dataNA<-left_join(select(data3, date, interval), select(data2, interval, mean_steps), by="interval")
colnames(dataNA)[3]<-"steps"
datafinal<-rbind(dataNONA, dataNA)
```
New data set

```r
head(datafinal)
```

```
    steps       date interval
289     0 2012-10-02        0
290     0 2012-10-02        5
291     0 2012-10-02       10
292     0 2012-10-02       15
293     0 2012-10-02       20
294     0 2012-10-02       25
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
datafinalsum<-datafinal%>%group_by(date)%>%summarise(sum_steps=sum(steps))
plot2<-ggplot(datafinalsum,aes(datafinalsum$sum_steps))+geom_histogram()+labs(title="Histogram for steps", x="Steps per day")
plot(plot2)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean_withoutNAS<-mean(datafinalsum$sum_steps)
median_withoutNAS<-median(datafinalsum$sum_steps)
```
The mean replacing NAS with the average of the interval is 1.0766189\times 10^{4}, and the median is 1.0766189\times 10^{4}. This values differ from the values without considering NAS, by imputting the mean of each interval to the NAS values the number of steps increased.

Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
datafinal$week<-weekdays(datafinal$date)
weekend<-c("sábado", "domingo")
datafinal$dayweek<-"weekdays"
datafinal[datafinal$week%in%weekend==TRUE,5]<-"weekend"
datafinal$dayweek<-as.factor(datafinal$dayweek)
```
2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
datafinal2<-datafinal%>%group_by(interval,dayweek)%>%summarise(average_steps=mean(steps))
ggplot(datafinal2, aes(interval, average_steps))+geom_line()+facet_wrap(~ dayweek)+labs(x="Interval", y="Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
