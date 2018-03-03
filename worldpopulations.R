library(tidyverse)
library(magrittr)
library(zoo)
library(ggmap)
library(sigmoid)
library(ggrepel)

raw.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\populationbycountry19802010millions.csv', 
                     na=c('NA')) %>%
  rename(country=X1) %>%
  gather(year, population, 2:32) %>%
  arrange(country) %T>%
  write.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\population_answer.csv')

missing.data <- raw.data %>%
  subset(population == '--' | is.na(population))

country.locations <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\country_locations.csv', stringsAsFactors=FALSE)

plotting.data <- missing.data %>%
  count(country, population) %>%
  inner_join(country.locations, by=c('country'='name')) %>%
  mutate(mod.pop = sigmoid((n / max(n)))*5)



ggplot(plotting.data) + 
  borders('world', color='gray50', fill='gray50') + 
  geom_point(aes(x=longitude, y=latitude, color=n), 
             shape=ifelse(is.na(plotting.data$population), 21, 16),
             size = plotting.data$mod.pop
             ) +
  geom_label_repel(aes(x=longitude, y=latitude, label=country))








