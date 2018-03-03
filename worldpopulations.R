library(tidyverse)
library(magrittr)
library(zoo)

raw.data <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\populationbycountry19802010millions.csv', 
                     stringsAsFactors=FALSE, header=TRUE, na.strings=c('NA', '--')) %>%
  setNames(c('country', 1980:2010)) %>%
  gather(year, population, 2:32) %>%
  na.omit() %>%
  arrange(country) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\population_answer.csv')