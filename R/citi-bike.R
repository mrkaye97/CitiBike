rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggmap)
library(data.table)

readfiles <- function(x) {
  temp <- tempfile() 
  download.file(paste('https://s3.amazonaws.com/tripdata/', x, '-citibike-tripdata.csv.zip', sep = ""), temp)
  data <- temp %>%
    unzip() %>%
    fread()
}

d <- c(202001:202004) %>%
  map(readfiles) %>%
  rbindlist() %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  group_by(ssid) %>%
  mutate(numstarts = n()) %>%
  ungroup()

manhattan_map <- get_map(location = 'new york, new york', maptype = "watercolor", zoom = 12)

ggmap(manhattan_map)

qmplot(sslon, sslat, data = temp, maptype = "toner-background", size = propstarts, alpha = .01) 
