library(tidyverse)

games <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/games.csv"))
players <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/players.csv"))
plays <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/plays.csv"))
game1 <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/tracking_gameId_2017090700.csv"))

plays_select <- plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(pass_or_run = ifelse(is.na(PassLength), "R", "P")) %>%
  select(gameId, playId, quarter, GameClock, down, yardsToGo, possessionTeam,
         yardlineSide, yardlineNumber, offenseFormation, personnel.offense,
         pass_or_run, playDescription)

plays_select <- plays_select %>%
  mutate(len = nchar(personnel.offense)) %>%
  filter(len < 31) 

plays_select <- plays_select %>%
  mutate(num_rb = case_when(
    len == 27 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 22 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 16 ~ substring(plays_select$personnel.offense, 1, 1)
  ),
  num_wr = case_when(
    len == 27 ~ substring(plays_select$personnel.offense, 24, 24),
    len == 22 ~ substring(plays_select$personnel.offense, 19, 19),
    len == 16 ~ substring(plays_select$off_pers, 13, 13)
  ))


  mutate(num_rb = substring(plays_select$off_pers, 1, 1),
         num_te = substring(plays_select$off_pers, 7, 7),
         num_wr = substring(plays_select$off_pers, 13, 13))

plays_select$num_rb <- as.numeric(plays_select$num_rb)
plays_select$num_te <- as.numeric(plays_select$num_te)
plays_select$num_wr <- as.numeric(plays_select$num_wr)

colSums(is.na(plays_select))




