# Reproducible Research
## Assignment 1
***

### Summary and SetUp

In this assignment I looked at two months of activity data from a monitoring device and performed various explorations.  
The first step is to initialize the environment and load the data.  


```{r}
require(dplyr)
require(ggplot2)
setwd("~/a_Research and Reading/Data Scientist Toolbox/Reproducible Research/repdata-data-activity")
activity <- read.csv("activity.csv",header = TRUE)
```

***
### Look at Daily Total Steps

To look at the the pattern of daily total steps, summarize the raw data by date, create a graph of the summary, and calculate the mean and median daily total.

```{r}
totday <- activity %>% group_by(date) %>% summarize(sum = sum(steps, na.rm = TRUE))
totday <- mutate(totday, avg = mean(sum))
totday <- mutate(totday,median = median(sum))
```

The average daily step total is 9354.2295082.
The median daily step total is 10395.

We'll look at bin widths of 1000 and 2000 steps to get a picture of the daily distribution. 

```{r}
ghist <- ggplot(totday, aes(sum)) + geom_histogram(fill = "blue", binwidth = 2000)
ghist <- ghist + labs(title = "Distribution of Total Steps", x = "Steps per Day", y = "Number of days")
ghist
```
![figure 1](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-3-1.png)

```{r}
ghist <- ggplot(totday, aes(sum)) + geom_histogram(fill = "blue", binwidth = 1000)
ghist <- ghist + labs(title = "Distribution of Total Steps", x = "Steps per Day", y = "Number of days")
ghist
```
![figure 2](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-4-1.png)
***
### Look at Daily Activity Pattern

Now we want to average each 5-minute interval across all days, which will give us a look at how an average day is distributed.  For this analysis, we'll ignore the NA values, which, because no data is recorded might imply that the device isn't worn during this interval.


```{r}
avgday <- activity %>% group_by(interval) %>% summarize(avgsteps = mean(steps, na.rm = TRUE))

g <- ggplot(avgday,aes(interval, avgsteps)) + geom_line(aes(color = "red"))
g <- g + labs(x = "5-minute Interval", y = "Average Steps", title = "Average Steps per 5-Minute Interval")
g <- g + theme(legend.position = "none")
g
```
![figure 3](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-5-1.png)

Given all days of data, which interval during the day has the highest average number of steps?

```{r}
maxinterval <- avgday %>% filter(avgday$avgsteps == max(avgday$avgsteps))
```

It looks like `r maxinterval$interval` is the time during the day with the highest average activity of `r maxinterval$avgsteps` steps.

   
*** 
### How Do Missing Values Affect the Analysis?

There are 2304 missing or NA values in our dataset.  If we assume that the person simply forgot to wear the device during an otherwise normal day, then we could assume that during each NA interval, the average number of steps for that interval were performed.  

Therefore, we'll create a new dataset in which each NA interval value is replaced with the mean(steps) for that interval.

First, let's join our original set called 'activity' with the set we created called 'avgday' which contains the interval and average steps. From this, we can replace each NA with the appropriate value, and create our 'activity_imputed' set as an equivalent to the original 'activity'.  


```{r}
joined <- inner_join(activity,round(avgday,0))
joined$steps[is.na(joined$steps)] <- joined$avgsteps[is.na(joined$steps)]
activity_imputed <- joined[,1:3]
```

Re-compute the mean and median from the new set, and plot new histograms of daily activity.

```{r}
totday_imputed <- activity_imputed %>% group_by(date) %>% summarize(sum = sum(steps, na.rm = TRUE))
totday_imputed <- mutate(totday_imputed, avg = mean(sum))
totday_imputed <- mutate(totday_imputed,median = median(sum))
```

The average daily step total for the imputed set is: 
```{r}
min(totday_imputed$avg)
```

The median daily step total for the imputed set is:
```{r}
min(totday_imputed$median)
```

As we see in the statistics and in the distributions, using some imputed values instead of ignoring values will nudge the averages up a bit.

Again, use bin widths of 1000 and 2000 for the histograms.

```{r}
ghist_imputed <- ggplot(totday_imputed, aes(sum)) + geom_histogram(fill = "blue", binwidth = 2000)
ghist_imputed <- ghist_imputed + labs(title = "Distribution of Total Steps With Imputed Missing Values", x = "Steps per Day", y = "Number of days")
ghist_imputed
```
![figure 4](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-11-1.png)

```{r}
ghist_imputed <- ggplot(totday_imputed, aes(sum)) + geom_histogram(fill = "blue", binwidth = 1000)
ghist_imputed <- ghist_imputed + labs(title = "Distribution of Total Steps With Imputed Missing Values", x = "Steps per Day", y = "Number of days")
ghist_imputed
```
![figure 5](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-12-1.png)

***
### Does Activity Differ Between Weekdays and Weekends?

Using the imputed dataset, we'll recalculate interval averages across weekdays vs weekends, so we can see differences in activity patterns if such differences exist. 


First - add the indicator column.
```{r}
activity_imputed <- mutate(activity_imputed, 
       dayOfWeek = factor((weekdays(as.Date(date)) %in% c('Saturday','Sunday')),
                         levels = c(FALSE,TRUE),labels=c('weekday','weekend')))
```

Second - create a summary data frame on interval, dayOfWeek, avg(steps).
```{r}
avgday_imputed <- activity_imputed %>% group_by(interval,dayOfWeek) %>% summarize(avgsteps = mean(steps))
```

Third - create a facet plot showing the comparison between weekdays and weekends avg steps.
```{r}
gweekday <- ggplot(avgday_imputed, aes(x = interval, y = avgsteps))
gweekday <- gweekday + geom_line() + facet_grid(dayOfWeek ~ .)
gweekday <- gweekday + labs(x = "Interval", y = "Average # of Steps", title = "Step Activity Weekdays VS Weekends")
gweekday
```
![figure 6](https://github.com/lmarin17/RepData_PeerAssessment1/blob/master/PA1_template_files/figure-html/unnamed-chunk-15-1.png)
***
### Conclusion

Using 5-minute interval step totals for a significant amount of time gives us some nice feedback opportunities with which to make some analysis.  Providing the analysis back to the wearer might help him to improve his activity patterns, and maybe even make lifestyle modifications necessary to meet his fitness goals.  

From this particular dataset, we see that overall activity patterns for this individual peak generally in the morning, especially during weekdays, whereas on the weekend, less intense but more sustained patterns of activity emerge.
