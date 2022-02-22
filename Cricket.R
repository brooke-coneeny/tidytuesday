library(tidyverse)

#Loading in the data
matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

#Select variables of interest and making new ones 
matches <- matches %>%
  select(team1, team2, team1_away_or_home, team2_home_away, winner) %>%
  #Making a variable which denotes if team1 won or team2
  mutate(team1_win = ifelse(team1 == winner, 1, 0),
         team2_win = ifelse(team2 == winner, 1, 0)) %>%
  #Getting rid of winner column now
  rm(winner)
  
