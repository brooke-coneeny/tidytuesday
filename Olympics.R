library(tidyverse)

# Loading in the data
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

# Select variables of interest 
olympics_variables <- olympics %>%
  filter(!is.na(medal), season == "Summer") %>%
  select(year, sex, noc, team, medal)

# How many people earned medals by gender each year?
all_medalist <- olympics_variables %>%
  group_by(year, sex) %>%
  summarise(num_medal = n()) 
  
female_medalist <- all_medalist %>%
  filter(sex == "F") %>%
  dplyr::rename(num_fem = num_medal) %>%
  select(year, num_fem)

male_medalist <- all_medalist %>%
  filter(sex == "M") %>%
  dplyr::rename(num_mal = num_medal) %>%
  select(year, num_mal)

combined_medalist <- merge(female_medalist, male_medalist, by = "year")
  
# What percent were female by country? male?

combined_medalist <- combined_medalist %>%
  mutate(per_fem = num_fem / (num_fem + num_mal),
         per_mal = num_mal / (num_fem + num_mal)) 

# Plot these results
female_plot_percent <- combined_medalist %>%
  ggplot(aes(x = year, y = per_fem)) +
  geom_line() +
  theme_bw()

female_plot_num <- combined_medalist %>%
  ggplot(aes(x = year, y = num_fem)) +
  geom_line() +
  theme_bw()

male_plot_percent <- combined_medalist %>%
  ggplot(aes(x = year, y = per_mal)) +
  geom_line() +
  theme_bw()

male_plot_num <- combined_medalist %>%
  ggplot(aes(x = year, y = num_mal)) +
  geom_line() +
  theme_bw()

combined_plot_num <- all_medalist %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = year, y = num_medal, fill = sex)) +
  geom_col(position = "dodge") +
  theme_bw()

combined_plot_percentage <- combined_medalist %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = year, y = per_fem)) +
  geom_line() +
  geom_line(aes(x = year, y = per_mal)) +
  theme_bw()

# Looking at specifically softball

softball_medals_by_team <- olympics %>%
  filter(sport == "Softball", !is.na(medal)) %>%
  select(team, medal, games) %>%
  group_by(team, medal) %>%
  summarise(count = n()) 

medals_team_plot <- softball_medals_by_team %>%
  ggplot(aes(x = team, y = count, fill = medal)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(
    title = "Which Country Dominates in Softball?",
    x = "Country",
    y = "Number of Medals",
    fill = "Medal Type",
    caption = "Plot created by Brooke Coneeny, data credit to Kaggle"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    plot.background = element_rect(fill = "azure2"),
    panel.background = element_rect(fill = "azure2"),
    legend.background = element_rect(fill = "azure2"),
    plot.caption.position =  "plot",
    text = element_text(size = 10),
    plot.title = element_text(size = 15)
  )

softball_medals_by_year <- olympics %>%
  filter(sport == "Softball", !is.na(medal)) %>%
  select(team, medal, games) %>%
  group_by(team, medal) %>%
  summarise(count = n()) %>%
  rename(num_medals = count) %>%
  pivot_wider(
    names_from = "medal",
    values_from = "num_medals",
    values_fill = 0
  )




  

