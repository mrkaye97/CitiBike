rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggmap)
library(data.table)
library(viridis)

readfiles <- function(x) {
  temp <- tempfile() 
  download.file(paste('https://s3.amazonaws.com/tripdata/', x, '-citibike-tripdata.csv.zip', sep = ""), temp)
  data <- fread(cmd = paste('unzip -p', temp, sep = " "))
  
  do.call(file.remove, list(list.files(path = getwd(), pattern = '*.csv')))
  
  return(data)
}

raw <- c(202001:202004) %>%
  map(readfiles) %>%
  rbindlist() 

df <- raw %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  group_by(ssid, sslat, sslon) %>%
  summarize(numstarts = n()) %>%
  ungroup() %>%
  arrange(desc(numstarts)) %>%
  slice(1:50)
  

manhattan_map <- get_map(location = 'new york, new york', maptype = "watercolor", zoom = 12)

ggmap(manhattan_map)

qmplot(sslon, sslat, data = df, maptype = "toner-background", color = numstarts, size = numstarts)+
  scale_color_viridis()+
  scale_alpha_continuous()
  
  
  
  
