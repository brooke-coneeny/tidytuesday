library(tidyverse)

#Loading in the data
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

#Select variables of interest
scoobydoo <- scoobydoo %>%
  select(season, title, 
         starts_with("caught"),
         starts_with("captured"),
         starts_with("unmask"),
         starts_with("snack"),
         -contains("other"),
         -contains("not"))

#Organize data using pivot longer, creating a character column, and converting true/false values to numeric 
scoobydoo_organized <- scoobydoo %>%
  pivot_longer(
    cols = -c("season", "title"),
    names_to = c(".value", "character"),
    names_sep = "_",
    values_drop_na = TRUE
  ) %>%
  mutate(
    across(c("caught","captured","unmask","snack"), ~as.logical(.x) %>% as.integer())
  )

#Group by season and character to find how many monsters each character caught per season 
scoobydoo__grouped <- scoobydoo_organized %>%
  select(season, character, caught) %>%
  filter(caught == 1) %>%
  group_by(season, character) %>%
  count(caught) %>%
  ungroup()

#Comparing how many monsters each character caught, filled by season
character_season_comparison <- scoobydoo__grouped %>%
  filter(season %in% c("1","2","3","4")) %>%
  ggplot(aes(x = character, y = n, fill = season)) +
  geom_col(position = "dodge") +
  theme_bw() +
  labs(
    title = "Who Caught the Most Monsters each Season?",
    x = "Character Name",
    y = "Monsters Caught",
    fill = "Season",
    caption = "Plot created by Brooke Coneeny, data credit to Kaggle and plummye"
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


