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

## Auxillary functions
The following are auxillary functions used to format the text. Normally this 
would be hidden, but for this assignment all code must be displayed, so the code
for all auxillary functions are displayed in this section.  


```r
# This code was taken from the "R Markdown Cookbook":
#   https://bookdown.org/yihui/rmarkdown-cookbook/font-color.html (sec 5.1.1)
#
# The intent is to support text coloring, an example of which is displayed at 
# the end of the "Load the data file" section below.

colorize <- function(x, color) {
    if (knitr::is_latex_output()) {
        sprintf("\\textcolor{%s}{%s}", color, x)
    } else if (knitr::is_html_output()) {
        sprintf("<span style='color: %s;'>%s</span>", color, x)
    } else x
}
```
---
## Loading and preprocessing the data
The data for this assignment is stored in **RepData_PeerAssessment1/activity.csv
**. 

In this section, the code will download the data in **activity.csv** to an R 
table. 
  
The data from **activity.csv** appears to load correctly but it contains several 
**<span style='color: red;'>NA</span>** values. To get an assessment of how many NA values
and under which heading, we must print out a summary of the code.


```r
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
```
  
# TESTING DATA READ
  

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

  
The data from **activity.csv** appears to load correctly but it contains several 
**<span style='color: red;'>NA</span>** values. To get an assessment of how many NA values
and under which heading, we must print out a summary of the code.


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?


### Bibliography
Detailed markdown guides  
  
1. https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf?_ga=2.101798721.1913430008.1665358646-243373298.1665358646
1. https://markdown-guide.readthedocs.io/en/latest/basics.html   
  
Concise markdown guides (cheatsheets)  
  
  
1. https://rmarkdown.rstudio.com/lesson-1.html
