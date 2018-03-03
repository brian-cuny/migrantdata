library(tidyverse)
library(magrittr)
library(zoo)

raw.data <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\us_daily_electric_system_operating_data.csv', 
                     stringsAsFactors=FALSE, header=TRUE, skip=4)

locations <- raw.data[seq(1, length(raw.data), 4), ] %>% 
  unite(location, 1, 2, sep='') %>%
  select(1) %>%
  unlist() %>%
  rep(each=3)

raw.data %<>% .[which(seq(1, length(raw.data)) %% 4 != 1), ] %>% 
  transform(location=locations) %>%
  gather(date, mega.watt.hours, 2:32) %>%
  spread(megawatthours, mega.watt.hours) %>%
  setNames(c('location', 'date', 'demand', 'generated', 'net')) %>%
  separate(date, c('month', 'day', 'year'))

raw.data$month %<>% str_replace('X', '')