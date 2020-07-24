rm(list = ls())

library(tidyverse)
library(lubridate)
library(data.table)
library(viridis)
library(ggthemes)
library(bigreadr)
library(rprojroot)
library(bsts)
library(vroom)
library(janitor)

PROJECT_ROOT <- find_root('CitiBike.Rproj')

read_data <- function(date) {
  temp <- tempfile()
  download.file(paste('https://s3.amazonaws.com/tripdata/', date, '-citibike-tripdata.csv.zip', sep = ""), temp, mode="wb")
  
  tryCatch(
    {
      dat <- fread(unzip(temp))
      unlink('*.csv')
      dat <- dat %>%
        clean_names(case = 'lower_camel') %>%
        rename_all(list(tolower)) %>%
        mutate(date = starttime %>% as_date()) %>%
        group_by(date) %>%
        summarize(numrides = n(), .groups = 'drop') %>%
        ungroup()
      
      return(dat)
    },
    error = function(e){
      return(data.frame())
    }
  )
}

dates <- c(201701:201712, 201801:201812, 201901:201912, 202001:202006)
df <- map(dates, read_data) %>%
  rbindlist()

df %>%
  write_csv('data/daily_rides.csv')


