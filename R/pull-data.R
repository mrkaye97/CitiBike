rm(list = ls())

library(tidyverse)
library(data.table)
library(parallel)
library(rprojroot)
library(sf)
library(lubridate)

PROJECT_ROOT <- find_root('CitiBike.Rproj')

readfiles <- function(x) {
  temp <- tempfile() 
  download.file(paste('https://s3.amazonaws.com/tripdata/', x, '-citibike-tripdata.csv.zip', sep = ""), temp)
  data <- fread(cmd = paste('unzip -p', temp, sep = " "))
  
  do.call(file.remove, list(list.files(path = getwd(), pattern = '*.csv')))
  
  return(data)
}

cores <- detectCores()
raw <- mclapply(c(202003:202004, 201903:201904), readfiles, mc.cores = cores) %>%
  rbindlist() %>%
  mutate(starttime = as_datetime(starttime),
         stoptime = as_datetime(stoptime))


breaks <- quos(1:500000, 500001:1000000, 1000001:1500000, 1500001:2000000, 
               2000001:2500000, 2500001:3000000, 3000001:3500000, 3500001:4000000,
               4000001:4500000, 4500001:nrow(df))

multiwrite <- function(b, i) {
  raw %>%
    slice(!!!b) %>%
    fwrite(file = paste(PROJECT_ROOT, '/data/citibike_dat_', as.character(i), '.csv', sep = ""))
}

map2(breaks, 1:10, multiwrite)
