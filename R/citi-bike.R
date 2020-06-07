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
library(parallel)
library(bigreadr)

readfiles <- function(x) {
  temp <- tempfile() 
  download.file(paste('https://s3.amazonaws.com/tripdata/', x, '-citibike-tripdata.csv.zip', sep = ""), temp)
  data <- fread(cmd = paste('unzip -p', temp, sep = " "))
  
  do.call(file.remove, list(list.files(path = getwd(), pattern = '*.csv')))
  
  return(data)
}

cores <- detectCores()
raw <- mclapply(c(202001:202004), readfiles, mc.cores = cores) %>%
  rbindlist()

times <- function(ampm, stend, currcat) {
  dropcoords <- ifelse(stend == 's', list(c('eslon', 'eslat')), list(c('sslon', 'sslat'))) %>%
    unlist()
  keepcoords <- ifelse(stend == 's', list(c('sslon', 'sslat')), list(c('eslon', 'eslat'))) %>%
    unlist()
  
  
  raw %>%
    rename(sslat = 'start station latitude',
           sslon = 'start station longitude',
           eslat = 'end station latitude',
           eslon = 'end station longitude',
           ssid = 'start station id',
           esid = 'end station id') %>%
    mutate(sthr = hour(starttime),
           endhr = hour(stoptime)) %>%
    filter(!!!ifelse(ampm == 'a', quos(sthr %in% c(7:9)), quos(sthr %in% c(16:18))),
           !!!ifelse(ampm == 'a', quos(endhr %in% c(7:9)), quos(endhr %in% c(16:18)))) %>%
    group_by(!!!ifelse(stend == 's', quos(ssid, sslat, sslon), quos(esid, eslat, eslon))) %>%
    summarize(count = n(),
              sslon = first(sslon),
              sslat = first(sslat),
              eslon = first(eslon),
              eslat = first(eslat)) %>%
    ungroup() %>%
    mutate(category = currcat) %>%
    select(-dropcoords) %>%
    rename(lon = keepcoords[1],
           lat = keepcoords[2])
}

ampms <- c('a', 'p', 'a', 'p')
ses <- c('s', 's', 'e', 'e')
cats <- c('ams', 'pms', 'ame', 'pme')

full <- mcmapply(times, ampm = c(ampms), stend = c(ses), currcat = c(cats), SIMPLIFY = F, mc.cores = cores) %>%
  rbindlist(fill = T) %>%
  mutate(sid = coalesce(esid, ssid),
         esid = NULL,
         ssid = NULL,
         category = category %>% as.factor()) %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(4326)

nyc <- st_read('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON', 
               stringsAsFactors = F) %>%
  st_as_sf() %>%
  st_set_crs(4326)

all_nbhds <- expand_grid(ntacode = nyc %>% pull(ntacode) %>% unique(),
                         category = cats) %>%
  left_join(nyc, by = 'ntacode') %>%
  st_as_sf()

df <- st_join(all_nbhds, full, st_intersects, left = T) %>%
  mutate(category = case_when(is.na(category.y) ~ category.x,
                              category.x == category.y ~ category.x,
                              TRUE ~ 'rem'),
         category.x = NULL,
         category.y = NULL) %>%
  filter(category != 'rem') %>%
  group_by(ntacode, category, boro_name) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  filter(boro_name == 'Manhattan') %>%
  mutate('Number of Rides (Log Scale)' = log(count),
         category = fct_recode(category, 
                               'AM Start' = 'ams',
                               'AM End' = 'ame',
                               'PM Start' = 'pms',
                               'PM End' = 'pme'))

plt <- df %>%
  ggplot()+
  geom_sf(aes(fill = `Number of Rides (Log Scale)`), na.rm = T)+
  scale_fill_viridis(option = 'plasma', alpha = .8, na.value = 'black')+
  theme_fivethirtyeight()+
  theme(axis.text = element_blank())+
  facet_wrap(~category %>% fct_relevel(c('AM Start', 'AM End', 'PM Start', 'PM End')), 
             nrow = 1)+
  labs(title = 'Starting and Ending Neighborhoods for CitiBike Rides',
       caption = 'Data comes from Jan-Apr 2020.\n AM commutes start and end between 7:00 and 9:00.\nPM Commutes start and end between 16:00 and 18:00')

ggsave(filename = 'commutes.svg', 
       device = 'svg', 
       path = '/users/matt/documents/github/citibike/viz/', 
       plot = plt, 
       dpi = 'retina', 
       width = 12,
       height = 8,
       units = 'in')



