library(tidyverse)
library(magrittr)
library(zoo)

raw.data <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\migrantdata\\us_daily_electric_system_operating_data.csv', 
                     skip=4)

locations <- raw.data[seq(1, length(raw.data), 4), 1:2] %>% 
  replace_na(list(`01/01/2018`='')) %>%
  unite(location, 1, 2, sep='') %>%
  select(1) %>%
  unlist()

tidy.data <- raw.data[which(1:length(raw.data) %% 4 != 1), ] %>% 
  mutate(location=locations %>% 
              rep(each=3)
            ) %>%
  gather(date, mega.watt.hours, 2:32) %>%
  spread(megawatthours, mega.watt.hours) %>%
  rename(generated=`Net generation`, net=`Total net actual interchange`, demand=Demand) %>%
  separate(date, c('month', 'day', 'year'))

tidy.data[, 2:7] %<>%
  map(~as.numeric(.))

# analysis ----------------------------------------------------------------

tidy.data %>%
  ggplot(aes(x=day, y= generated / demand, color=location)) + 
  geom_point() +
  geom_smooth(se=FALSE) + 
  scale_color_brewer(palette='Set1') +
  theme(legend.position='bottom') +
  guides(color=guide_legend(nrow=1, override.aes=list(size=2))) +
  labs(x='Day in January 2018', 
       y='Proportion of Required Energy Generated',
       color='Location',
       title='California and New England Not Generating Enough Electricity to Meet Need'
       )
  
  
