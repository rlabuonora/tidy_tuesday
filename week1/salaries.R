library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)


# import and tidy data
df <- read_xlsx('./tidytuesday/data/tidy_tuesday_week2.xlsx') %>% 
  as_tibble  %>% 
  gather(-year, key="position", val="salary") %>% 
  group_by(year, position) %>% 
  top_n(n = 16, wt = salary) %>% 
  ungroup %>% 
  mutate(salary = salary/1000000,
         of_def = case_when(
           position %in% c("Cornerback", "Defensive Lineman",
                           "Safety", "Linebacker",
                           "Special Teamer") ~ "Defense",
           position %in% c("Running Back", "Quarterback",
                           "Offensive Lineman", "Wide Receiver",
                           "Tight End") ~ "Offense"
         )
  )

df_percent <- df %>% 
  group_by(year) %>% 
  mutate(total_year_salary = sum(salary)) %>% 
  group_by(position, add = TRUE) %>% 
  summarise(salary_participation = sum(salary) * 100 / first(total_year_salary),
            of_def = first(of_def))


# plot   
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
  theme(legend.position = "none") +
  ggsave("week1/plot2.png", width=6, height=6)



df_percent %>% 
  ggplot(aes(year, salary_participation)) +
  geom_line(aes(group=position, col=position)) +
  facet_wrap(~of_def) + 
  ggsave("week1/plot1.png")  
