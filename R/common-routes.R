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
library(leaflet)
library(rjson)
library(mapview)

PROJECT_ROOT <- find_root('CitiBike.Rproj')

station_json <- fromJSON(file = 'https://feeds.citibikenyc.com/stations/stations.json')[['stationBeanList']]

read_json <- function(x) {
  station_json[[x]] %>%
    data.frame()
}

stations <- purrr::map(1:length(station_json), read_json) %>%
  rbindlist() %>%
  select(id, stAddress1) %>%
  rename(address = stAddress1) %>%
  mutate(address = paste(address, 'New York, NY', sep = ", "))
  

df <- list.files(paste(PROJECT_ROOT, '/data', sep = ""), full.names = T) %>%
  map(fread) %>%
  rbindlist() %>%
  rename(sslat = 'start station latitude',
         sslon = 'start station longitude',
         eslat = 'end station latitude',
         eslon = 'end station longitude',
         ssid = 'start station id',
         esid = 'end station id') %>%
  filter(year(starttime) == 2019) %>%
  mutate(ssid = ssid %>% as.numeric(),
         esid = esid %>% as.numeric()) %>%
  inner_join(stations %>% rename(ssid = id, ssaddress = address), by = 'ssid') %>%
  inner_join(stations %>% rename(esid = id, esaddress = address), by = 'esid') %>%
  group_by(ssid, sslon, sslat, esid, eslon, eslat, ssaddress, esaddress) %>%
  summarize(numroute = n()) %>%
  ungroup() %>%
  arrange(desc(numroute)) %>%
  top_n(50) %>%
  mutate(routeid = 1:50)

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}

s <- df$ssaddress
e <- df$esaddress

res <- data.frame()
for (i in 1:50) {
  s1 <- s[i] %>% unname()
  e1 <- e[i] %>% unname()
  
  r <- route(s1, e1, alternatives = F, mode = 'bicycling', output = 'all', structure = 'route')
  res <- decodeLine(r$routes[[1]]$overview_polyline$points) %>%
    mutate(routeid = i) %>%
    bind_rows(res)
}

res <- res %>%
  mutate(routeid = as.factor(routeid)) %>%
  group_by(routeid) %>%
  filter(!any(!(lat > 39.5 & lat < 41)),
         !any(!(lon > -74 & lon < -72)))
  


df <- df %>%
  mutate(routeid = routeid %>% as.factor()) %>%
  inner_join(res, by = 'routeid')


grouped_coords <- function(coord, group, order) {
  data_frame(coord = coord, group = group) %>%
    group_by(group) %>%
    purrrlyr::by_slice(~c(.$coord, NA), .to = "output") %>%
    left_join(
      data_frame(group = group, order = order) %>% 
        distinct()) %>%
    arrange(order) %>%
    .$output %>%
    unlist()
}

pal <- colorFactor(
  palette = "magma", domain = NULL)

# Map using Leaflet R
l <- leaflet(res) %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolylines(
    lng = ~grouped_coords(lon, routeid, rownames(res)),
    lat = ~grouped_coords(lat, routeid, rownames(res)),
    color = ~pal(df$numroute))

mapshot(l, file = paste(PROJECT_ROOT, "/viz/common-routes.png", sep = ""))
saveWidget(l, file= paste(PROJECT_ROOT, "/viz/common-routes.html", sep = ""))

