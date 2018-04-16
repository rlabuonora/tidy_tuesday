library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)

df <- read_xlsx('./tidytuesday/data/tidy_tuesday_week2.xlsx') %>% 
  as_tibble  %>% 
  gather(-year, key="position", val="salary") %>% 
  group_by(year, position) %>% 
  top_n(n = 16, wt = salary) %>% 
  mutate(salary = salary/1000000,
         year_label = (str_sub(year, 3, 4)),
         of_def = case_when(
           position %in% c("Cornerback", "Defensive Lineman",
                           "Safety", "Linebacker",
                           "Special Teamer") ~ "Defense",
           position %in% c("Running Back", "Quarterback",
                           "Offensive Lineman", "Wide Receiver",
                           "Tight End") ~ "Offense"
         )
  )
         
df  %>% 
  ggplot(aes(year, salary)) + 
  geom_jitter(aes(alpha=0.5, col=position)) +
  scale_y_continuous(name="Average Cap") +
  scale_x_continuous(labels=c("'12", "'14", "'16", "'18"),
                     breaks = c(2012, 2014, 2016, 2018)) + 
  labs(title = "The average pay for top running backs has stalled", 
       subtitle = "Average cap value of the 16 highest-paid players in each position") +
  geom_smooth(col = "orange") +
  facet_wrap(facets= of_def~position) +
  theme(legend.position = "none")


ggsave("week1/plot.png")  
