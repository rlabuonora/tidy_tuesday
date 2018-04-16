library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)


shorthand <- tribble(
  ~position, ~abbrev,
  "Running Back", "RB",
  "Quarterback", "QB",
  "Offensive Lineman", "OL",
  "Tight End", "TE",
  "Wide Receiver", "WR",
  "Cornerback", "CB",
  "Defensive Lineman", "DE",
  "Linebacker", "LB",
  "Safety", "S",
  "Special Teamer", "ST"
)


# import and tidy data
df <- read_xlsx('./tidytuesday/data/tidy_tuesday_week2.xlsx') %>% 
  as_tibble  %>% 
  gather(-year, key="position", val="salary") %>% 
  left_join(shorthand) %>% 
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



# plot salary over years per position
df  %>% 
  ggplot(aes(year, salary)) + 
  geom_jitter(aes(alpha=0.7)) +
  scale_y_continuous(name="Average Cap",
                     breaks = c(10, 20, 30),
                     labels=c("10k", "20k", "30k")) +
  scale_x_continuous(name="Year",
                     labels=c("'12", "'14", "'16", "'18"),
                     breaks = c(2012, 2014, 2016, 2018)) + 
  labs(title = "The average pay for top running backs has stalled", 
       subtitle = "Average cap value of the 16 highest-paid players in each position") +
  geom_smooth(col = "orangered") +
  facet_wrap(facets= of_def~abbrev, nrow=2) +
  theme(legend.position = "none",
        plot.background = element_rect(color = "#e6e6e6"),
        panel.grid.major = element_line(color = "#b6b6b6"),
        panel.background = element_blank(),
        strip.background = element_blank(),
        ) +
  ggsave("week1/plot1.png", width=6, height=4)


# calculate participation per position
df_percent <- df %>% 
  group_by(year) %>% 
  mutate(total_year_salary = sum(salary)) %>% 
  group_by(position, add = TRUE) %>% 
  summarise(salary_participation = sum(salary) * 100 / first(total_year_salary),
            of_def = first(of_def))
# plot salary participations
df_percent %>% 
  ggplot(aes(year, salary_participation)) +
  geom_line(aes(group=position, col=position)) +
  facet_wrap(~of_def) + 
  ggsave("week1/plot2.png")  
