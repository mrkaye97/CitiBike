rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggmap)
library(data.table)
library(viridis)
library(sf)
library(units)
library(ggthemes)
library(parallel)

PROJECT_ROOT <- find_root('CitiBike.Rproj')

df <- list.files(paste(PROJECT_ROOT, '/data', sep = ""), full.names = T, pattern = "citibike*") %>%
  map(fread) %>%
  rbindlist() %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  mutate(year = year(starttime)) %>%
  group_by(ssid, sslon, sslat, year) %>%
  summarize(numrides = n()) %>%
  ungroup() %>%
  st_as_sf(coords = c('sslon', 'sslat')) %>%
  st_set_crs(4326)

nyc <- st_read('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON', 
               stringsAsFactors = F) %>%
  filter(boro_name %in% c('Queens', 'Brooklyn', 'Manhattan')) %>%
  st_as_sf() %>%
  st_set_crs(4326)

df <- st_join(nyc, temp, st_intersects, left = T) %>%
  filter(!is.na(numrides)) %>%
  group_by(boro_name, year) %>%
  summarize(numrides = sum(numrides)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(year = as.factor(year))
  
plt <- df %>%
  ggplot(aes(boro_name, numrides, fill = year))+
  geom_bar(stat = 'identity', position = 'dodge')+
  theme_fivethirtyeight()+
  scale_fill_manual(values = c("#DD5E66FF", "#5901A5FF"))

ggsave(filename = 'coronavirus-overall-impact.svg', 
       device = 'svg', 
       path = '/users/matt/documents/github/citibike/viz/', 
       plot = plt, 
       dpi = 'retina', 
       width = 12,
       height = 8,
       units = 'in')



