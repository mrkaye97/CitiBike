rm(list = ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(ggthemes)
library(rprojroot)
library(bsts)
library(XML)


PROJECT_ROOT <- find_root('CitiBike.Rproj')

df <- read_csv('data/daily_rides.csv')

df %>% 
  ggplot(aes(date, numrides))+
  geom_line()+
  theme_fivethirtyeight()

ss <- list()
ss <- AddSeasonal(ss, df %>% pull(numrides), nseasons = 52, season.duration = 7)
ss <- AddTrig(ss, df %>% pull(numrides), period = 365, frequencies = 1)
ss <- AddSemilocalLinearTrend(ss, df %>% pull(numrides))
ss <- AddAutoAr(ss, df %>% pull(numrides))

model1 <- bsts(df$numrides,
               state.specification = ss,
               niter = 1000)

burnin <- 500
tibble(date = df$date, 
       data = df$numrides,
       ar = colMeans(model1$state.contributions[-(1:burnin),"Ar1",]),
       week = colMeans(model1$state.contributions[-(1:burnin),"seasonal.52.7",]),
       trend = colMeans(model1$state.contributions[-(1:burnin),"trend",]),
       trig = colMeans(model1$state.contributions[-(1:burnin),"trig.365",]),
#       reg = colMeans(model1$state.contributions[-(1:burnin),"regression",]),
       ) %>%
  pivot_longer(c(data, ar, week, trend, trig), names_to = 'component', values_to = 'value') %>%
  ggplot(aes(x = date, y = value, color = component))+
  geom_line()+
  facet_wrap(~component, scales = 'free', ncol = 1)


n <- 100
preds <- predict(model1, horizon = n)
new <- data.frame(date = seq(max(df$date)+1, max(df$date) + n , by = '1 day'),
                  numrides = preds$mean,
                  upper = preds$interval[2,],
                  lower = preds$interval[1,],
                  type = 'pred')

df %>%
  mutate(type = 'orig') %>%
  bind_rows(new) %>%
  filter(date > max(date) - 250) %>%
  ggplot(aes(date, numrides, color = type))+
  geom_line()+
  geom_ribbon(aes(ymax = upper, ymin = lower, alpha = .1), fill = 'gray', color = 'darkgray')


