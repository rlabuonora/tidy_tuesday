# australian salary

library(readr)

library(ggplot2)
library(ggalt)

df <- read_csv("./tidytuesday/data/week4_australian_salary.csv")

df_long <- df  %>% 
  mutate(occupation = gsub("[\x01-\x1f\x7f-\xff]", "", occupation)) %>% # sacar caracteres invalidos
  mutate(occ = unlist(lapply(str_split(df$occupation, ";"), function(x) x[[1]]))) %>%    # para quedarme con el primer item
  mutate(occ = factor(occ)) %>% 
  select(occ, gender, individuals, average_taxable_income) %>% 
  gather(var, val, individuals, average_taxable_income) # make it long


# spread multiple columns
# https://stackoverflow.com/questions/43695424/tidyr-spread-multiple-columns0
d <-  df_long %>% 
  unite("gender_var", gender, var) %>% 
  spread(gender_var, val) %>% 
  arrange(desc(Male_average_taxable_income - Female_average_taxable_income)) %>% 
  head(10)

# otra forma con enframe, etc.:
# https://github.com/jennybc/row-oriented-workflows/blob/master/ex07_group-by-summarise.md

d %>%
  ggplot(aes(x = Female_average_taxable_income, xend = Male_average_taxable_income, y = occ)) + 
  geom_dumbbell()




# simple dot plot
df %>%
  ggplot(aes(average_taxable_income, occ, col = gender)) + geom_point() + 
  labs(x = NULL, y = NULL, 
       title = "Salaries",
       caption = "#tidytuesday") + 
  theme(plot.title = element_text(hjust = 0.5))


# dumbell plot
# https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Dumbbell%20Plot

