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
library(rprojroot)
library(patchwork)

PROJECT_ROOT <- find_root('CitiBike.Rproj')

df <- list.files(paste(PROJECT_ROOT, '/data', sep = ""), full.names = T, pattern = "citibike*") %>%
  map(fread) %>%
  rbindlist() %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id')

p1 <- df %>%
  mutate(year = year(starttime) %>% as.factor(),
         dow = lubridate::wday(starttime, label = T),
         ww = case_when(dow %in% c('Sat', 'Sun') ~ 'Weekend',
                        TRUE ~ 'Weekday') %>% as.factor(),
         hr = hour(starttime)) %>%
  ggplot(aes(hr, fill = year))+
  geom_bar()+
  facet_grid(ww ~ year, scales = 'free')+
  scale_x_continuous(breaks = c(6, 12, 18, 24), 
                     labels = c('06:00', 'Noon', '18:00', 'Midnight'))+
  theme_fivethirtyeight()+
  theme(legend.position = 'none')


nyc <- st_read('https://data.cityofnewyork.us/api/geospatial/cpf4-rkhq?method=export&format=GeoJSON', 
               stringsAsFactors = F) %>%
  filter(boro_name %in% c('Queens', 'Brooklyn', 'Manhattan')) %>%
  st_as_sf() %>%
  st_set_crs(4326)

grid <- df %>%
  select(ssid, sslon, sslat) %>%
  distinct() %>%
  st_as_sf(coords = c('sslon', 'sslat')) %>%
  st_set_crs(4326)

grid <- st_join(nyc, grid, st_intersects, left = T) %>%
  filter(!is.na(ssid)) %>%
  as.data.frame() %>%
  select(ssid, boro_name) %>%
  distinct()

df <- df %>%
  left_join(grid, by = 'ssid') %>%
  mutate(year = year(starttime) %>% as.factor()) %>%
  group_by(boro_name, year) %>%
  summarize(numrides = n()) %>%
  ungroup() %>%
  drop_na()

p2 <- df %>%
  ggplot(aes(boro_name, numrides, fill = year))+
  geom_bar(stat = 'identity', position = 'dodge')+
  theme_fivethirtyeight()+
  theme(legend.position = 'right', legend.direction = 'vertical', legend.title.align = .5)

patchworkplot <- p1 / p2 + plot_layout(widths = c(4, 4), 
                                       heights = unit(c(6, 8), c('cm', 'cm')),
                                       guides = "collect") &
  scale_fill_viridis(option = 'plasma', begin = .5, end = .1, discrete = T)

ggsave(filename = 'coronavirus-and-day-dsn.svg', 
       device = 'svg', 
       path = '/users/matt/documents/github/citibike/viz/', 
       plot = patchworkplot, 
       dpi = 'retina')


