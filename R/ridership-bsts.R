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
ss <- AddSeasonal(ss, df %>% pull(numrides), nseasons = 7, season.duration = 1)
#ss <- AddMonthlyAnnualCycle(ss, df %>% pull(numrides), date.of.first.observation = df$date[1])
#ss <- AddSeasonal(ss, df %>% pull(numrides), nseasons = 26, season.duration = 14)
ss <- AddSemilocalLinearTrend(ss, df %>% pull(numrides))
ss <- AddAutoAr(ss, df %>% pull(numrides))
ss <- AddTrig(ss, df %>% pull(numrides), period = 365, frequencies = 1, method = 'harmonic')

model1 <- bsts(forecast::tsclean(df$numrides),
               state.specification = ss,
               niter = 1500)

burnin <- 1000
tibble(date = df$date, 
       data = forecast::tsclean(df$numrides),
       ar = colMeans(model1$state.contributions[-(1:burnin),"Ar1",]),
       week = colMeans(model1$state.contributions[-(1:burnin),"seasonal.7.1",]),
       trend = colMeans(model1$state.contributions[-(1:burnin),"trend",]),
       seas = colMeans(model1$state.contributions[-(1:burnin),"trig.365", ])
#       reg = colMeans(model1$state.contributions[-(1:burnin),"regression",]),
       ) %>%
  pivot_longer(c(data, ar, trend, seas, week), names_to = 'component', values_to = 'value') %>%
  ggplot(aes(x = date, y = value, color = component))+
  geom_line()+
  facet_wrap(~component, scales = 'free', ncol = 1)


n <- 900
preds <- predict(model1, horizon = n)
new <- data.frame(date = seq(max(df$date)+1, max(df$date) + n , by = '1 day'),
                  numrides = preds$mean,
                  upper = preds$interval[2,],
                  lower = preds$interval[1,],
                  type = 'pred')

df %>%
  mutate(type = 'orig') %>%
  bind_rows(new) %>%
  filter(date > max(date) - 1500) %>%
  ggplot(aes(date, numrides, color = type))+
  geom_line()+
  geom_ribbon(aes(ymax = upper, ymin = lower, alpha = .1), fill = 'gray', color = 'darkgray')


save(model1, df, file = 'R/ridership_bsts.Rdata')

