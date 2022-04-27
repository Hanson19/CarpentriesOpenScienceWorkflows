Carpentires Example Streamflow Analysis
================
Katherine Hanson

# script purpose and organization

This is an example script developed durng a Carpentires workshop. Within
it, we will develope a workflow to:

1.  Prepare an R environment
2.  Download and save USGS streamflow data
3.  Tidy, QAQC, and gap fill data
4.  Report some summary statistics

## 1. Prepare the R environment

We will load any relevant packes and set the default for our code
visibility in the final report.

``` r
knitr::opts_chunk$set(echo = TRUE)
library(dataRetrieval)
```

    ## Warning: package 'dataRetrieval' was built under R version 4.1.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.1.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(zoo)
```

    ## Warning: package 'zoo' was built under R version 4.1.3

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.1.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 4.1.3

## 2. Download and save USGS streamflow data

We will use the `readNWISdv` function from the `dataRetrieval` package
to download stream stage data for the USGS gage on the Arkansas River
near Larned.

``` r
#gage id
usgs_id <- "07141220"

#download the data
data_raw <- 
  readNWISdv(siteNumber = usgs_id,
             parameterCd = "00065",
             startDate = "2018-10-01",
             endDate = "2021-09-30")

#inspect the data
summary(data_raw)
```

    ##   agency_cd           site_no               Date            X_00065_00003   
    ##  Length:1089        Length:1089        Min.   :2018-10-01   Min.   : 3.760  
    ##  Class :character   Class :character   1st Qu.:2019-07-03   1st Qu.: 4.330  
    ##  Mode  :character   Mode  :character   Median :2020-03-31   Median : 4.670  
    ##                                        Mean   :2020-03-31   Mean   : 4.919  
    ##                                        3rd Qu.:2020-12-28   3rd Qu.: 4.850  
    ##                                        Max.   :2021-09-30   Max.   :14.510  
    ##  X_00065_00003_cd  
    ##  Length:1089       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
#save the data
write.csv(data_raw, "data/ExampleStreamStage_Raw.csv")
```

## 3. Tidy, gap-fill and QAQC data

In this section, we will perform some basic data cleaning operations to
get our data ready for future analysis.

``` r
# create a new data frame with better column names
data_tidy <- 
  data_raw %>% 
  rename(stage_ft = X_00065_00003,
         stage_QAcode = X_00065_00003_cd) %>% 
  select(-agency_cd, -site_no)

#look at the new data frame
head(data_tidy)
```

    ##         Date stage_ft stage_QAcode
    ## 1 2018-10-01     4.35            A
    ## 2 2018-10-02     4.34            A
    ## 3 2018-10-03     4.32            A
    ## 4 2018-10-04     4.30            A
    ## 5 2018-10-05     4.31            A
    ## 6 2018-10-06     4.34            A

``` r
#first step in commuing with data is to plot
ggplot(data_tidy, aes(x = Date, y = stage_ft))+
  geom_line()
```

![](CarpentiresRmdDemo_files/figure-gfm/clean-data-1.png)<!-- -->

``` r
# check for missing dates by comparing all possible dates to the dates you have
first_date <- min(data_tidy$Date)
last_date <- max(data_tidy$Date)
all_date <- seq(first_date, last_date, by = "day") #make vector of all dates
length(all_date) == length(data_tidy$Date)
```

    ## [1] FALSE

``` r
# determine missing dates
missing_dates <- all_date[!(all_date %in% data_tidy$Date)]

# add missing date to data frame
new_dates <- data.frame(Date = missing_dates,
                        stage_ft = NA,
                        stage_QAcode = "Gapfill")

data_clean <- 
  bind_rows(data_tidy, new_dates) %>% 
  arrange(Date)

summary(data_clean)
```

    ##       Date               stage_ft      stage_QAcode      
    ##  Min.   :2018-10-01   Min.   : 3.760   Length:1096       
    ##  1st Qu.:2019-07-01   1st Qu.: 4.330   Class :character  
    ##  Median :2020-03-31   Median : 4.670   Mode  :character  
    ##  Mean   :2020-03-31   Mean   : 4.919                     
    ##  3rd Qu.:2020-12-30   3rd Qu.: 4.850                     
    ##  Max.   :2021-09-30   Max.   :14.510                     
    ##                       NA's   :7

``` r
# fill in those gap using lines interpolation
data_clean$stage_ft <- na.approx(data_clean$stage_ft)

summary(data_clean)
```

    ##       Date               stage_ft      stage_QAcode      
    ##  Min.   :2018-10-01   Min.   : 3.760   Length:1096       
    ##  1st Qu.:2019-07-01   1st Qu.: 4.330   Class :character  
    ##  Median :2020-03-31   Median : 4.670   Mode  :character  
    ##  Mean   :2020-03-31   Mean   : 4.920                     
    ##  3rd Qu.:2020-12-30   3rd Qu.: 4.853                     
    ##  Max.   :2021-09-30   Max.   :14.510

``` r
# plot and inspect
ggplot(data_clean, aes(x = Date, y = stage_ft, color = stage_QAcode))+
  geom_point()
```

![](CarpentiresRmdDemo_files/figure-gfm/clean-data-2.png)<!-- -->

``` r
# save data
write.csv(data_clean, "data/ExampleStreamStage_Clean.csv")
```

## 4. Write up some summary statistics.

We analyzed data from USGS gage 07141220. During the period of record,
the maximum stage was 14.51 and the minum stage was 3.76. There were a
total of 7 missing dates that we gap-filled using linear interpolation.
