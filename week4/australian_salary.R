# australian salary

library(readr)


df <- read_csv("./tidytuesday/data/week4_australian_salary.csv") %>% 
  mutate(occupation = gsub("[\x01-\x1f\x7f-\xff]", "", occupation),
         occupation = factor(occupation))

df %>%
  ggplot(aes(average_taxable_income, occupation)) + geom_point()
