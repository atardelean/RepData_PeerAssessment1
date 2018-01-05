# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp<-tempfile()
download.file(fileURL,temp, method="curl")
data<-read.csv(unz(temp, "activity.csv"), header=TRUE, sep=",", na.strings="NA", colClasses=c("numeric","Date","factor"))
unlink(temp)
```

## What is mean total number of steps taken per day?

Calculate the total number of steps per day, ignoring missing values

```r
z<-with(data,tapply(steps, date, sum, na.rm=T))
data_s<-data.frame(Date=names(z), Total.Steps=z)
```

Create a histogram of the total number of steps per day

```r
quartz()
with(data_s, {
        hist(Total.Steps, main="Daily Total Steps", xlab="Total Steps")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
dev.copy(png, file="plot1.png")
```

```
## quartz_off_screen 
##                 4
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```
Calculate and report the mean and median of the total number of steps per day

```r
mean<-mean(data_s$Total.Steps)
median<-median(data_s$Total.Steps)
x<-data.frame(c1=c("Mean", mean), c2=c("Median", median))
print(x)
```

```
##                 c1     c2
## 1             Mean Median
## 2 9354.22950819672  10395
```

## What is the average daily activity pattern?
Calculate and create a times series plot of the average number of steps across all days taken in each 5-minute interval

```r
z<-with(data, tapply(steps, interval, mean, na.rm=T))
data_s2<-data.frame(interval=names(z), Avg.Steps=z)
data_s2$interval<-as.numeric(levels(data_s2$interval))

attach(data_s2)
new_data<-data_s2[order(interval),]
detach(data_s2)

quartz()
with(new_data, {
        plot(interval, Avg.Steps, type="l", main="Average Daily Pattern", ylab="Average Steps", xlab="5-minute Interval")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
dev.copy(png, file="plot2.png")
```

```
## quartz_off_screen 
##                 5
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```
Find the 5-minute interval with maximum average of steps across all days

```r
d2<-dplyr::mutate(data_s2, max=max(Avg.Steps))
d2<-dplyr::filter(d2,Avg.Steps==max)
print("5-minute Interval with maximum average across all days:")
```

```
## [1] "5-minute Interval with maximum average across all days:"
```

```r
print(d2$interval)
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the number of missing values:

```r
sum(is.na(data$date))
```

```
## [1] 0
```

```r
sum(is.na(data$interval))
```

```
## [1] 0
```

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
Replace missing values with the average number of steps across all days for each 5-minute interval

```r
data_list<-list(data, data_s2)
data_2<-plyr::join_all(data_list, "interval")
data_3<-transform(data_2,steps_m=ifelse(!is.na(steps), steps, Avg.Steps))
data_m<-dplyr::select(data_3, steps_m, interval, date )
z<-with(data_m,tapply(steps_m, date, sum, na.rm=T))
data_ms<-data.frame(Date=names(z), Total.Steps=z)
```
Create a histogram of the total number of steps per day

```r
quartz()
with(data_ms, {
        hist(Total.Steps, main="Daily Total Steps - Imputed Data", xlab="Total Steps")
})
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
dev.copy(png, file="plot3.png")
```

```
## quartz_off_screen 
##                 6
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```
Calculate and report the mean and median of the total number of steps per day

```r
mean<-mean(data_ms$Total.Steps)
median<-median(data_ms$Total.Steps)
x<-data.frame(c1=c("Mean", mean), c2=c("Median", median))
print(x)
```

```
##                 c1               c2
## 1             Mean           Median
## 2 10766.1886792453 10766.1886792453
```
With imputed values, both the median and the mean increased slightly. The median and the mean are now equal. The daily number of steps have increased with imputed values.


## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable "weekday" with two levels weekend and weekday

```r
data_m2<-dplyr::mutate(data_m, wd=weekdays(date))
data_m2<-transform(data_m2, weekday=ifelse((wd=="Saturday" | wd=="Sunday"), "weekend", "weekday"))
```
Calculate the average number of steps taken in 5-minute interval during weekdays and weekend

```r
data_m2<-dplyr::mutate(data_m2, Interval.Weekday=paste(as.character(interval),weekday, sep="."))
z<-with(data_m2,tapply(steps_m, Interval.Weekday, mean, na.rm=T))
data_m3<-data.frame(Interval.Weekday=names(z), Avg.Steps=z)
f<-strsplit(as.character(data_m3$Interval.Weekday), "\\.")
mat<-matrix(unlist(f), ncol=2, byrow=TRUE)
df<-as.data.frame(mat)
colnames(df)<-c("interval", "weekday")
data_m4<-cbind(data_m3,df)
data_m4<-dplyr::mutate(data_m4, interval=as.numeric(as.character(interval)))
attach(data_m4)
data_m5<-data_m4[order(interval),]
detach(data_m4)
```
Make a panel plot of the average number of steps taken in 5-minute interval

```r
d1<-subset(data_m5, weekday=="weekend")
d2<-subset(data_m5, weekday=="weekday")
quartz()
par(mfrow=c(2,1),mar=c(4,4,2,1))

plot(d2$interval, d2$Avg.Steps, type="l", main="Activity Patterns between weekdays and weekends", ylab="Average Steps on Weekdays", xlab="5-minute Interval"
    )
plot(d1$interval, d1$Avg.Steps, type="l", ylab="Average Steps on Weekends", xlab="5-minute Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
dev.copy(png, file="plot4.png")
```

```
## quartz_off_screen 
##                 7
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```
