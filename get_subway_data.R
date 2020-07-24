rm(list = ls())

library(tidyverse)
library(lubridate)
library(rprojroot)
library(data.table)


url <- "http://web.mta.info/developers/fare.html"
html <- paste(readLines(url, skipNul = T), collapse="\n")
matched <- str_match_all(html, "<a href=\"(.*?)\"") %>% unlist()
new <- matched[grepl("data/nyct/fares", matched)]
new <- new[!grepl("href", new)]
pattern <- c('_20', '_19', '_18', '_17') %>%
  paste(collapse = '|')
new <- new[grepl(pattern, new)]
dates <- str_extract(new, "[0-9]+") %>%
  as.character()

read_subway <- function(d) {
  temp <- paste('http://web.mta.info/developers/data/nyct/fares/fares_', d, '.csv', sep = "") %>%
    fread(skip = 2) %>%
    dplyr::select(-c(1,2)) %>%
    dplyr::select_if(function(col) is.numeric(col) | is.character(col)) %>%
    as_tibble() %>%
    mutate(across(dplyr::everything(), as.numeric)) %>%
    rowwise() %>%
    mutate(tot = sum(c_across(dplyr::everything()))) %>%
    ungroup() %>%
    summarize(tot = sum(tot)) %>%
    mutate(date = d)
  
  return(temp)

}

subway <- map(dates, read_subway) %>%
  rbindlist() %>%
  mutate(year = paste("20", str_sub(date, 1, 2), sep = ""),
         month = str_sub(date, 3, 4),
         day = str_sub(date, 5, 6),
         date = as_date(paste(year, month, day, sep = "-")),
         year = NULL,
         month = NULL,
         day = NULL)

subway %>%
  write_csv('data/subway_rides.csv')




