library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)

df <- read_xlsx('./tidytuesday/data/tidy_tuesday_week2.xlsx') %>% 
  as_tibble

# Data Frame original tiene 100 lineas por anio
# (podrian ser 100 cuadros, 100 jugadores?) supongo que son jujadores

df %>% 
  gather(-year, key="position", val="cap") %>% 
  ggplot(aes(year, cap)) + 
  geom_jitter() +
  geom_smooth() +
  facet_wrap(~position)


ggsave("plot.png")  
