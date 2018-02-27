library(tidyverse)
library(magrittr)
library(zoo)

raw.data <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\projects\\UN_MigrantStockByAge_2015.csv', stringsAsFactors=FALSE, skip=14, header=FALSE, na.strings='..') 

raw.data %<>%
  subset(select=-c(1, 3:4, 6:22, 39, 56))

column.names <- c('country', 'code', paste('m', raw.data[2, 3:18]), paste('f', raw.data[2, 19:34])) %>% unlist() %>% setNames(NULL)

raw.data %<>% .[-(1:8), ] %>% setNames(column.names) %>%
  na.omit()

area <- raw.data %>% subset(code == '', select=c('country')) 

group <- area[c(1, 7, 13, 18, 23), ]
subgroup <- area[-c(1, 7, 13, 18, 23), ]
area %<>% mutate(big = replace(.$country, !.$country %in% group, NA))
area %<>% mutate(small = replace(.$country, !.$country %in% subgroup, NA))

area$big %<>% na.locf()

area %<>% subset(!is.na(small), select=-1)

raw.data %<>% subset(!.$country %in% group) %>% .[-1, ]

final.country <- data.frame(rep(NA, nrow(raw.data)), rep(NA, nrow(raw.data)))
for(i in 1:nrow(raw.data)){
  if(raw.data$code[i] == ''){
    area %<>% .[-1, ]
  }else{
    final.country[i, ] <- area[1, ]
  }
}

answer <- cbind(final.country, raw.data) %>%
  subset(code != '', select=-4) %>% 
  setNames(c('group', 'subgroup', 'country', names(raw.data)[3:length(raw.data)])) %>%
  .[, c(3, 1, 2, 4:35)]

answer[, 4:35] %<>% map(~str_replace_all(., '\\s', '') %>% as.numeric())

write.csv(answer, 'answer.csv')
