rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggmap)
library(data.table)
library(viridis)
library(sf)
library(units)
library(ggthemes)
library(microbenchmark)

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


times <- function(d, ampm, stend) {
  timefilter <- ifelse(stend == 's', 'sthr', 'endhr')
  grouping.vars <- ifelse(stend == 's', c(ssid, sslat, sslon), c(esid, eslat, eslon))
  coords <- ifelse(stend == 's', c('sslon', 'sslat'), c('eslon', 'eslat'))
  
  am.start <- temp %>%
    rename(sslat = 'start station latitude',
           sslon = 'start station longitude',
           eslat = 'end station latitude',
           eslon = 'end station longitude',
           ssid = 'start station id',
           esid = 'end station id') %>%
    mutate(sthr = hour(starttime),
           endhr = hour(e)) %>%
    filter(!!timefilter %in% c(7:9)) %>%
    group_by(grouping.vars) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(count = count %>% log()) %>%
    st_as_sf(coords = coords) %>%
    st_set_crs(4326)
}


am.start <- temp %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  mutate(sthr = hour(starttime)) %>%
  filter(sthr %in% c(7:9)) %>%
  group_by(ssid, sslat, sslon) %>%
  summarize(numstarts = n()) %>%
  ungroup() %>%
  mutate(numstarts = numstarts %>% log()) %>%
  st_as_sf(coords = c('sslon', 'sslat')) %>%
  st_set_crs(4326)

am.end <- raw %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  filter(hour(endtime) %in% c(7:9)) %>%
  group_by(esid, eslat, eslon) %>%
  summarize(numends = n()) %>%
  ungroup() %>%
  mutate(numends = numends %>% log()) %>%
  st_as_sf(coords = c('eslon', 'eslat')) %>%
  st_set_crs(4326)

nyc <- st_read('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON', 
              stringsAsFactors = F) %>%
  st_as_sf() %>%
  st_set_crs(4326)


temp <- st_join(nyc, df, st_intersects, left = T)

temp %>%
  filter(boro_name == 'Manhattan') %>%
  ggplot()+
  geom_sf(aes(fill = numstarts), na.rm = T)+
  scale_fill_viridis()+
  theme_fivethirtyeight()+
  theme(axis.text = element_blank())
  