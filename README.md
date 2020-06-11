README
================
Matt Kaye
6/7/2020

``` r
suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(rprojroot))
suppressPackageStartupMessages(library(lubridate))

PROJECT_ROOT <- find_root('CitiBike.Rproj')
```

## Exploring the New York City CitiBike data

This project explores the CitiBike program data from New York City.

A few notes:

  - The full project can be found
    [Here](https://mrkaye97.github.io/CitiBike/).
  - All R code is in `~/CitiBike/R`
  - All python code is in `~/CitiBike/python`
  - Some data used in certain files can be found in `~/CitiBike/data`,
    but much is pulled directly from online sources.

### Setup

The project requires no explicit setup. All data is either included in
this repo or comes directly from the CitiBike website.

A handful of R and Python packages are needed to run the project. See
the bottom of this README for a list.

### Data

The data comes from the [CitiBike
program](https://www.citibikenyc.com/system-data) directly, and is
publicly available for download.

Below is a glimpse at the structure of the data

``` r
fread(paste(PROJECT_ROOT, '/data/citibike_dat_1.csv', sep = "")) %>%
  mutate(starttime = starttime %>% as_datetime(),
         stoptime = stoptime %>% as_datetime(),
         bikeid = bikeid %>% as.factor()) %>%
  sample_n(size = 100) %>%
  glimpse()
```

    ## Rows: 100
    ## Columns: 15
    ## $ tripduration              <int> 388, 2564, 435, 218, 632, 1373, 168, 1164, …
    ## $ starttime                 <dttm> 2020-03-10 08:33:41, 2020-03-07 12:58:37, …
    ## $ stoptime                  <dttm> 2020-03-10 08:40:10, 2020-03-07 13:41:22, …
    ## $ `start station id`        <int> 167, 327, 267, 3641, 3443, 3668, 439, 3232,…
    ## $ `start station name`      <chr> "E 39 St & 3 Ave", "Vesey Pl & River Terrac…
    ## $ `start station latitude`  <dbl> 40.74890, 40.71534, 40.75098, 40.74287, 40.…
    ## $ `start station longitude` <dbl> -73.97605, -74.01658, -73.98765, -73.98919,…
    ## $ `end station id`          <int> 305, 389, 3707, 505, 3159, 3067, 317, 437, …
    ## $ `end station name`        <chr> "E 58 St & 3 Ave", "Broadway & Berry St", "…
    ## $ `end station latitude`    <dbl> 40.76096, 40.71045, 40.74146, 40.74901, 40.…
    ## $ `end station longitude`   <dbl> -73.96724, -73.96525, -73.98329, -73.98848,…
    ## $ bikeid                    <fct> 31377, 38780, 21188, 28690, 38057, 41949, 4…
    ## $ usertype                  <chr> "Subscriber", "Subscriber", "Customer", "Su…
    ## $ `birth year`              <int> 1992, 1972, 1967, 1973, 1967, 1992, 1993, 1…
    ## $ gender                    <int> 1, 2, 1, 1, 1, 1, 1, 1, 2, 0, 1, 1, 1, 0, 1…

## Plot Examples

#### Visualizing Commute Patterns – Heatmap of start and end stations by time of day

<img src="https://raw.githubusercontent.com/mrkaye97/CitiBike/master/viz/commutes.svg">

#### Visualizing Rides Broken Down by Year, Day of the Week, and Time of Day

<img src="https://raw.githubusercontent.com/mrkaye97/CitiBike/master/viz/coronavirus-and-day-dsn.svg" width="980" height="1000">
