library(tidyverse)
library(magrittr)
library(zoo)
library(ggmap)
library(ggrepel)

# read in data, remove uneeded parts and set column names -----------------
raw.data <- read.csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\UN_MigrantStockByAge_2015.csv', 
                     stringsAsFactors=FALSE, header=FALSE, na.strings='..', encoding='utf-8') %>%
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

country.mapping <- data.frame(group=rep(NA, nrow(raw.data)), subgroup=rep(NA, nrow(raw.data)))
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
  subset(code != '') %>% 
  rowid_to_column('id')

country.id.table <- answer[, 1:4]


migration.data <- answer[, -c(2:5)] %>%  
  gather(category, quantity, 2:33) %>%
  separate(category, c('gender', 'age'), ' ') %>%
  transform(age = ifelse(age == '5-9', '05-09', age)) %>%
  spread(gender, quantity)%>%
  arrange(id)

migration.data[, 3:4] %<>% map(~str_replace_all(., '\\s', '') %>% as.numeric())
# combine data frames, set column names, and reorder content --------------



# analysis ----------------------------------------------------------------
populations <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\population_answer.csv') %>%
  subset(year == 2010, select=-1) %>%
  mutate(population = population %>% as.numeric() * 1000000) %>%
  na.omit()

country.locations <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\country_locations.csv')

combined <- inner_join(country.id.table, populations, by='country') %>%
  inner_join(migration.data, by='id') %>%
  group_by(country) %>%
  summarise(total = (sum(f) + sum(m)) / sum(population)) %>%
  arrange(desc(total)) %>%
  .[1:10, ] %>%
  inner_join(country.locations, by=c('country'='name'))

ggplot(combined) +
  borders('world', color='gray50', fill='gray50') + 
  geom_point(aes(x=longitude, y=latitude, color=country), size=4) +
  geom_label_repel(aes(x=longitude, y=latitude, label=country)) +
  scale_color_brewer(palette='Set3')







