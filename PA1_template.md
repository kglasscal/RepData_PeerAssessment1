---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
    number_sections: true
  pdf_document:
    toc: true
    toc_depth: 2
    number_sections: true
---
<!--  pdf_document: default -->

---
# Loading and preprocessing the data
The data set contains the number of steps in 5 min intervals 
The data for this assignment is stored in **RepData_PeerAssessment1/activity.csv
**. 

**Data Contents**  
The data in  **activity.csv** consists of 17,568 data points with three 
variables per point. As described in the problem statement, the variables are:

* steps -- the number of steps taken on a given day and interval
* date -- the date of a set of measurements, <em>(range: 2012-10-01 to 2012-11-30)
</em>
* interval -- the five minute interval for a given measurement <em>(range: 0 to 2355)</em>
  
The data from **activity.csv** appears to load correctly but it contains several 
<span style="color: red; font-weight: bold;">"NA"</span> values. To get an assessment of how many 
NA values and under which heading, we must print out a summary of the code.


```r
library("tidyverse")
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.2      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library("ggplot2")

### tidyTable function
tidyTable <- function(sourceTable)
{
    tidyActivityTable <- sourceTable[!is.na(sourceTable$steps),]
    return (tidyActivityTable)
}

### loadData function
loadData <- function (fileName)
{
    activityTable <- read.csv(fileName)
    return (activityTable)
}

getActivityTable <- function (fileName)
{
    activityTable <- loadData(fileName)
    tidyActivityTable <- tidyTable(activityTable)

    dataTables <- list(activityTable, tidyActivityTable)

    return (dataTables)
}
```

## What is mean total number of steps taken per day?
<!--
For this part of the assignment, you can ignore the missing values in the dataset.
    1. Calculate the total number of steps taken per day
    2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
    3. Calculate and report the mean and median of the total number of steps taken per day
-->

```r
compute_mean_steps <- function(dataTable)
{
    # filter total steps by date
    steps <- dataTable %>%
        group_by(date) %>%
        summarise(total_steps = sum(steps, na.rm = TRUE))
    mean_steps <- steps %>%
        summarise(mean_steps = mean(total_steps, na.rm = TRUE))
    median_steps <- steps %>%
        summarise(median_steps = median(total_steps, na.rm = TRUE))

    stepData <- list(steps, mean_steps, median_steps)

    return (stepData)
}

dataTables <- getActivityTable('activity.csv')
stepData <- compute_mean_steps(dataTables[[2]])

hist(stepData[[1]][2]$total_steps,xlab = "Weight",col = "yellow",border = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?
<!--
    1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
-->


```r
compute_timeseries <- function(dataTable)
{
    # filter total steps by date
    timeseries <- dataTable %>%
        group_by(interval) %>%
        summarise(timeseries = sum(steps, na.rm = TRUE))
    step_average <- timeseries %>%
        summarise(step_average = mean(timeseries, na.rm = TRUE))

    timeSeriesData <- list(timeseries, step_average)

    return (timeSeriesData)
}

timeSeriesData <- compute_timeseries(dataTables[[2]])
ts <- timeSeriesData[[1]]
print(typeof(ts))
```

```
## [1] "list"
```

```r
ggplot(timeSeriesData[[1]], aes(x = interval, y = timeseries)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
idx = which.max(timeSeriesData[[1]]$timeseries)
print(max(timeSeriesData[[1]]$timeseries))
```

```
## [1] 10927
```

```r
print(idx)
```

```
## [1] 104
```

```r
print(timeSeriesData[[1]]$timeseries[idx])
```

```
## [1] 10927
```



## Imputing missing values

<!--
Note that there are a number of days/intervals where there are missing
values (coded as `NA`). The presence of missing days may introduce
bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the **mean** and **median** total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

-->



## Are there differences in activity patterns between weekdays and weekends?

<!--
For this part the `weekdays()` function may be of some help here. Use
the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

1. Make a panel plot containing a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using **simulated data**:

![Sample panel plot](instructions_fig/sample_panelplot.png) 

**Your plot will look different from the one above** because you will
be using the activity monitor data. Note that the above plot was made
using the lattice system but you can make the same version of the plot
using any plotting system you choose.

-->


### Bibliography
Detailed markdown guides  
  
1. https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf
1. https://markdown-guide.readthedocs.io/en/latest/basics.html   
  
Concise markdown guides (cheatsheets)  
  
  
1. https://rmarkdown.rstudio.com/lesson-1.html




<!--
### TESTING CODE
  

```r
dataReadTest <- function(activityTable, tidyActivityTable)
{
    activityTable <- loadData('activity.csv')
    tidyActivityTable <- tidyTable(activityTable)
    
    # TESTING: These are for testing, remove when ready to sumbit
    print(head(activityTable, n = 3)) # print the first three lines of the activityTable
    print(tail(activityTable, n = 3)) # print the last three lines of the activityTable
    
    print(head(tidyActivityTable, n = 3)) # print the first three lines of the tidyActivityTable
    print(tail(tidyActivityTable, n = 3)) # print the last three lines of the tidyActivityTable

}
dataReadTest()
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
##       steps       date interval
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
##       steps       date interval
## 17278     0 2012-11-29     2345
## 17279     0 2012-11-29     2350
## 17280     0 2012-11-29     2355
```
-->
