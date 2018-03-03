library(tidyverse)
library(magrittr)
library(zoo)

# read in data, remove uneeded parts and set column names -----------------
raw.data <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\UN_MigrantStockByAge_2015.csv', 
                     stringsAsFactors=FALSE, header=FALSE, na.strings='..') %>%
  .[-c(1:15, 17:22), -c(1, 3:4, 6:22, 39, 56)] %>%
  setNames(c('country', 'code', paste('m', .[1, 3:18]), paste('f', .[1, 19:34])) %>% unlist()) %>%
  .[-1, ] %>%
  na.omit()
# read in data, remove uneeded parts and set column names -----------------

# create list of major and sub group locations ----------------------------
all.groupings <- raw.data %>% 
  subset(code == '', select=c('country')) 

major.group.locs <- c(1, 7, 13, 18, 23)
major.group <- all.groupings[major.group.locs, ]
sub.group <- all.groupings[-major.group.locs, ]

all.groupings %<>% mutate(major.group = replace(.$country, !.$country %in% major.group, NA) %>% na.locf(), 
                          sub.group = replace(.$country, !.$country %in% sub.group, NA)
                         ) %>%
  subset(!is.na(sub.group), -1)
# create list of major and sub group locations ----------------------------

# create mapping of major and sub groups ----------------------------------
raw.data %<>% subset(!.$country %in% major.group) %>% .[-1, ]

country.mapping <- data.frame(rep(NA, nrow(raw.data)), rep(NA, nrow(raw.data)))
for(i in 1:nrow(raw.data)){
  if(raw.data$code[i] == ''){
    all.groupings %<>% .[-1, ]
  }else{
    country.mapping[i, ] <- all.groupings[1, ]
  }
}
# create mapping of major and sub groups ----------------------------------


# combine data frames, set column names, and reorder content --------------
answer <- cbind(country.mapping, raw.data) %>%
  setNames(c('group', 'subgroup', 'country', names(raw.data)[2:length(raw.data)])) %>%
  subset(code != '') %>% 
  .[, c(3, 1, 2, 5:36)] %>%
  gather(category, quantity, 4:35) %>%
  separate(category, c('gender', 'age'), ' ') %>%
  arrange(group, subgroup, country)


answer$quantity %<>% str_replace_all(., '\\s', '') %>% as.numeric()
# combine data frames, set column names, and reorder content --------------

write.csv(answer, 'C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\answer.csv')



# top 10 most migrants ----------------------------------------------------
library(maps)


country.rates <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\answer.csv', stringsAsFactors=FALSE)
country.locations <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\country_locations.csv', stringsAsFactors=FALSE)


both <- inner_join(country.rates, country.locations, by=c('country'='name')) %>%
  mutate(total = rowSums(.[, 6:21])) %>% 
  subset(select=c(2, 5, 23:25)) %>%
  .[order(desc(.$total)), ] %>%
  .[1:10, ]

pch <- ifelse(both$total > 5000000, 19, 2)
map('world', col='darkgrey', lwd=0.5, mar=c(0.1, 0.1, 0.1, 0.1))
points(both$longitude, both$latitude, pch=pch)













