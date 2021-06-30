library(tidyverse)
library(tidymodels)
library(vip)
library(data.table)
library(ggthemes)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 13, hjust = 0.5),
      axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16),
      axis.text = element_text(size = 12)
    )
}

games <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/games.csv"))
players <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/players.csv"))
plays <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/plays.csv"))
game1 <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/tracking_gameId_2017090700.csv"))

ID <- c("090700", "091000", "091001", "091002", "091003", "091004", "091005", "091007", "091008", "091009", "091010",
        "091011", "091012", "091100", "091101", "091400", "091700", "091701", "091702", "091703", "091704","091705",
        "091706", "091707", "091708", "091709","091710", "091711", "091712", "091713", "091800", "092100", "092401",
        "092402", "092403", "092404", "092405", "092406", "092407","092408", "092409", "092410", "092411", "092412",
        "092413", "092500", "092800", "100100", "100101", "100102", "100103", "100104", "100105", "100106","100107",
        "100108", "100109", "100110", "100111", "100112", "100113", "100200", "100500", "100800", "100801","100802", 
        "100803", "100804", "100805", "100806", "100807", "100808", "100809", "100810", "100811", "100900", "101200",
        "101500","101501", "101502", "101503", "101504", "101505", "101506", "101507", "101508", "101509", "101510",
        "101511", "101600")

#blank dataframe to store tracking data
df_tracking <- list()

#iterating through all weeks
for(i in 1:length(ID)){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("Data/tracking_gameId_2017",ID[i],".csv"),
                               col_types = cols())
  
  df_tracking[[i]] <- df_tracking_temp
  
}

tracking <- rbindlist(df_tracking)

plays_select <- plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(pass_or_run = ifelse(is.na(PassLength), "Run", "Pass")) %>%
  select(gameId, playId, quarter, GameClock, down, yardsToGo, possessionTeam, 
         yardlineNumber, offenseFormation, personnel.offense, defendersInTheBox, pass_or_run)

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
  filter(!is.na(yardlineNumber)) %>%
  filter(!is.na(defendersInTheBox))
  
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
  is_pistol = ifelse(offenseFormation == "PISTOL", 1, 0),
  is_wildcat = ifelse(offenseFormation == "WILDCAT", 1, 0))

pbp_17 <- nflfastR::load_pbp(2017)

pbp_17_select <- pbp_17 %>%
  select(old_game_id, play_id, half_seconds_remaining, score_differential)

plays_select$gameId <- as.character(plays_select$gameId)
pbp_17_select$old_game_id <- as.character(pbp_17_select$old_game_id)

plays_select <- plays_select %>%
  left_join(pbp_17_select, by = c("gameId" = "old_game_id", "playId" = "play_id"))

colSums(is.na(plays_select))

#create df of player positions and corresponding nflid
player_positions <- players %>%
  select(nflId, PositionAbbr)

#join player positions to tracking data by nflid
tracking <- inner_join(player_positions, tracking,
                       by = c("nflId" = "nflId"))

offense_positions <- c("C", "FB", "G", "QB", "RB", "TE", "WR", "T")

width_of_form <- tracking %>%
  filter(event == "ball_snap") %>%
  filter(PositionAbbr %in% offense_positions) %>%
  group_by(gameId, playId) %>%
  summarize(width = max(y) - min(y))

width_of_form$gameId <- as.character(width_of_form$gameId)

plays_select <- plays_select %>%
  left_join(width_of_form, by = c("gameId", "playId"))

colSums(is.na(plays_select))

tracking$gameId <- as.character(tracking$gameId)
plays_select$gameId <- as.character(plays_select$gameId) 

linemen_width <- tracking %>%
  filter(PositionAbbr %in% c("T", "G", "C")) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarise(linemen_width = max(y)- min(y))

plays_select <- plays_select %>%
  left_join(linemen_width, by = c("gameId", "playId"))

plays_select <- plays_select %>%
  mutate(prev_play = lag(pass_or_run)) %>%
  mutate(prev_pass = ifelse(prev_play == "P", 1, 0)) %>%
  select(-prev_play)

plays_select$prev_pass[is.na(plays_select$prev_pass)] <- 0

plays_select %>%
  filter(linemen_width < 8) %>%
  filter(linemen_width > 5) %>%
  ggplot(aes(x = pass_or_run, y = width)) +
  geom_jitter(color = "black", alpha = 0.05) +
  geom_boxplot(aes(fill = pass_or_run)) +
  scale_fill_brewer(palette = "Set2") +
  theme_reach() +
  labs(y = "Width of Formation",
       x = "",
       title = "How Width of Formation Affected Runs and Passes in 2017",
       caption = "By Tej Seth | @mfbanalytics | Data from Big Data Bowl")
ggsave('width.png', width = 15, height = 10, dpi = "retina")


colSums(is.na(plays_select))

###########################################################################

plays_model_data <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

split_pbp <- initial_split(plays_model_data, 0.75, strata = pass_or_run)

train_data <- training(split_pbp)

test_data <- testing(split_pbp)

pbp_rec <- recipe(pass_or_run ~ ., data = train_data) %>% 
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
  bind_cols(test_data %>% select(pass_or_run))

pbp_pred_lr %>% 
  group_by(.pred_class, pass_or_run) %>%
  summarize(perc = n() / nrow(pbp_pred_lr)) %>%
  arrange(-perc)
#We're getting it right 73.1% of the time

table("Predictions" = pbp_pred_lr$.pred_class, "Observed" = pbp_pred_lr$pass_or_run)

model_stats <- lr_wflow %>% 
  finalize_workflow(pbp_fit_lr) %>%
  fit(train_data) %>%
  pull_workflow_fit() %>%
  tidy()

model_stats %>%
  mutate(ord_term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = estimate, y = ord_term)) +
  geom_bar(aes(fill = ifelse(estimate > 0, "darkblue", "darkred")), stat = "identity") +
  scale_color_identity(aesthetics = c("fill")) +
  theme_bw() +
  theme(legend.position = "none")

rf_mod <- rand_forest(trees = 1000) %>% 
  set_engine("ranger", 
             importance = "impurity", # variable importance
             num.threads = 4) %>%     # Parallelize
  set_mode("classification")

rf_wflow <- workflow() %>% 
  add_model(rf_mod) %>%  # New model
  add_recipe(pbp_rec)    # Same recipe

pbp_fit_rf <- rf_wflow %>% # New workflow
  fit(data = train_data)   # Fit the Random Forest
# Get predictions and check metrics

pbp_pred_rf <- predict(pbp_fit_rf, test_data) %>% 
  bind_cols(test_data %>% select(pass_or_run)) %>% 
  bind_cols(predict(pbp_fit_rf, test_data, type = "prob"))

`pbp_pred_rf` %>% # Random Forest predictions
  metrics(truth = pass_or_run, .pred_class)

`pbp_pred_lr` %>% # Logistic Regression predictions
  metrics(truth = pass_or_run, .pred_class)

roc_rf <- pbp_pred_rf %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Random Forest")

roc_lr <- pbp_pred_lr %>% 
  roc_curve(truth = pass_or_run, .pred_Pass) %>% 
  mutate(model = "Logistic Regression")

bind_rows(roc_rf, roc_lr) %>% 
  # Note that autoplot() would also work here!
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = model)) + 
  geom_path(lwd = 1, alpha = 0.5) +
  geom_abline(lty = 3) + 
  theme_bw() +
  scale_color_manual(values = c("#374785", "#E98074")) +
  theme(legend.position = "top")

