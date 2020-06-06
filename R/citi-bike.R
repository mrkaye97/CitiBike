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

readfiles <- function(x) {
  temp <- tempfile() 
  download.file(paste('https://s3.amazonaws.com/tripdata/', x, '-citibike-tripdata.csv.zip', sep = ""), temp)
  data <- fread(cmd = paste('unzip -p', temp, sep = " "))
  
  do.call(file.remove, list(list.files(path = getwd(), pattern = '*.csv')))
  
  return(data)
}

cores <- detectCores()
raw <- mclapply(c(202001:202004), readfiles, mc.cores = cores) %>%
  rbindlist() %>%
  sample_n(100000)

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
  mutate(count = replace_na(count, 0)) %>%
  group_by(ntacode, category, boro_name) %>%
  summarize(count = sum(count)) %>%
  ungroup() %>%
  filter(boro_name %in% c('Manhattan', 'Brooklyn', 'Queens', 'Bronx'))

plt <- df %>%
  ggplot()+
  geom_sf(aes(fill = count), na.rm = T)+
  scale_fill_viridis()+
  theme_fivethirtyeight()+
  theme(axis.text = element_blank())+
  facet_wrap(~category)

ggsave(filename = 'plt.png', 
       device = 'png', 
       path = '/users/matt/downloads/', 
       plot = plt, 
       dpi = 'retina', 
       width = 6,
       height = 8,
       units = 'in')



