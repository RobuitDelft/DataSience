library(tidyverse)
length(levels(gapminder$region))
past_year=1970

p <- gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region,dollars_per_day, fill = continent))

p + geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




