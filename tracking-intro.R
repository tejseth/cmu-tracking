library(tidyverse)
library(tidymodels)
library(vip)

games <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/games.csv"))
players <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/players.csv"))
plays <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/plays.csv"))
game1 <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/tracking_gameId_2017090700.csv"))

plays_select <- plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(pass_or_run = ifelse(is.na(PassLength), "R", "P")) %>%
  select(gameId, playId, quarter, GameClock, down, yardsToGo, possessionTeam, 
         yardlineNumber, offenseFormation, personnel.offense, pass_or_run)

plays_select <- plays_select %>%
  mutate(len = nchar(personnel.offense)) %>%
  filter(len < 31) 

plays_select <- plays_select %>%
  mutate(num_rb = case_when(
    len == 27 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 22 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 16 ~ substring(plays_select$personnel.offense, 1, 1)))

plays_select <- plays_select %>%
  mutate(num_wr = case_when(
    len == 27 ~ substring(plays_select$personnel.offense, 19, 19),
    len == 22 ~ substring(plays_select$personnel.offense, 19, 19),
    len == 16 ~ substring(plays_select$personnel.offense, 13, 13)))

plays_select <- plays_select %>%
  mutate(num_te = case_when(
    len == 27 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 22 ~ substring(plays_select$personnel.offense, 7, 7),
    len == 16 ~ substring(plays_select$personnel.offense, 7, 7)))

plays_select <- plays_select %>%
  select(-personnel.offense, -len)
  
plays_select$num_rb <- as.numeric(plays_select$num_rb)
plays_select$num_te <- as.numeric(plays_select$num_te)
plays_select$num_wr <- as.numeric(plays_select$num_wr)

plays_select <- plays_select %>%
  filter(!is.na(offenseFormation)) %>%
  filter(!is.na(num_rb)) %>%
  filter(!is.na(yardlineNumber))
  
colSums(is.na(plays_select))

plays_select <- plays_select %>%
  mutate(is_shotgun = case_when(
    offenseFormation == "SHOTGUN" | offenseFormation == "EMPTY" ~ 1,
    TRUE ~ 0
  ),
  is_under_center = case_when(
    offenseFormation == "SINGLEBACK" | offenseFormation == "I_FORM" |
    offenseFormation == "JUMBO" | offenseFormation == "ACE" ~ 1,
    TRUE ~ 0
  ),
  is_pistol = case_when(
    offenseFormation == "PISTOL" | offenseFormation == "WILDCAT" ~ 1,
    TRUE ~ 0
  ))

pbp_17 <- nflfastR::load_pbp(2017)

pbp_17_select <- pbp_17 %>%
  select(old_game_id, play_id, half_seconds_remaining, score_differential)

plays_select$gameId <- as.character(plays_select$gameId)
pbp_17_select$old_game_id <- as.character(pbp_17_select$old_game_id)

plays_select <- plays_select %>%
  left_join(pbp_17_select, by = c("gameId" = "old_game_id", "playId" = "play_id"))

colSums(is.na(plays_select))

plays_model_data <- plays_select %>%
  mutate(label = as.factor(ifelse(pass_or_run == "P", 1, 0))) %>%
  select(quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, half_seconds_remaining, 
         score_differential, label) %>%
  select(label, everything())

split_pbp <- initial_split(plays_model_data, 0.75, strata = label)

train_data <- training(split_pbp)

test_data <- testing(split_pbp)

pbp_rec <- recipe(label ~ ., data = train_data) %>% 
  step_corr(all_numeric(), threshold = 0.7) %>% 
  step_center(all_numeric()) %>%  # substract mean from numeric
  step_zv(all_predictors()) # remove zero-variance predictors

lr_mod <- logistic_reg(mode = "classification") %>% 
  set_engine("glm")

lr_wflow <- workflow() %>% 
  add_model(lr_mod) %>% # parsnip model
  add_recipe(pbp_rec)   # recipe from recipes

pbp_fit_lr <- lr_wflow %>% 
  fit(data = train_data) # fit the model against the training data

pbp_pred_lr <- predict(pbp_fit_lr, test_data) %>% 
  # Get probabilities for the class for each observation
  bind_cols(predict(pbp_fit_lr, test_data, type = "prob")) %>% 
  # Add back a "truth" column for what the actual play_type was
  bind_cols(test_data %>% select(label))

pbp_pred_lr %>% 
  # get Area under Curve
  roc_auc(truth = label, 
          .pred_1)

pbp_pred_lr %>% 
  # collect and report metrics
  metrics(truth = label, 
          .pred_class)

pbp_pred_lr %>% 
  # calculate ROC curve
  roc_curve(truth = label, 
            .pred_1) %>% 
  # ggplot2 autoplot for AB line 
  # and the path of ROC curve
  autoplot()

pbp_fit_lr %>%
  pull_workflow_fit() %>% 
  vip(num_features = 20)

#create df of player positions and corresponding nflid
player_positions <- players %>%
  select(nflId, PositionAbbr)

#join player positions to tracking data by nflid
tracking <- inner_join(player_positions, tracking,
                       by = c("nflId" = "nflId"))


