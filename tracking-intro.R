library(tidyverse)
library(tidymodels)
library(vip)
library(data.table)
library(ggthemes)
library(caret)
library(mlbench)
library(DescTools)
library(randomForest)
library(nflfastR)
library(ggimage)

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

games_select <- games %>%
  select(gameId, week)

plays <- plays %>%
  left_join(games_select, by = c("gameId"))

plays_select <- plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(pass_or_run = ifelse(is.na(PassLength), "Run", "Pass")) %>%
  select(gameId, playId, week, quarter, GameClock, down, yardsToGo, possessionTeam, 
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
tracking <- left_join(player_positions, tracking,
                       by = c("nflId" = "nflId"))

offense_positions <- c("C", "FB", "G", "QB", "RB", "TE", "WR", "T")

width_of_form <- tracking %>%
  filter(event == "ball_snap") %>%
  filter(PositionAbbr %in% offense_positions) %>%
  group_by(gameId, playId) %>%
  summarize(width = max(y) - min(y),
            width_sd = sd(y, na.rm = T))

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
  summarise(linemen_width = max(y)- min(y), 
            linemen_height = max(x) - min(x),
            linemen_sd = sd(y))

plays_select <- plays_select %>%
  left_join(linemen_width, by = c("gameId", "playId"))

plays_select <- plays_select %>%
  mutate(prev_play = lag(pass_or_run)) %>%
  mutate(prev_pass = ifelse(prev_play == "Pass", 1, 0)) %>%
  select(-prev_play)

plays_select$prev_pass[is.na(plays_select$prev_pass)] <- 0

plays_select %>%
  filter(width < 3) %>% 
  ggplot(aes(x = pass_or_run, y = linemen_width)) +
  geom_jitter(color = "black", alpha = 0.05) +
  geom_boxplot(aes(fill = pass_or_run)) +
  scale_fill_brewer(palette = "Set2") +
  theme_reach() +
  labs(y = "Width of Formation",
       x = "",
       title = "How Width of the Formation Affected Runs and Passes in 2017")
ggsave('width.png', width = 15, height = 10, dpi = "retina")

colSums(is.na(plays_select))

running_back_deep <- tracking %>%
  filter(PositionAbbr %in% c("QB", "RB")) %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarize(rb_depth = max(x) - min(x))

plays_select <- plays_select %>%
  left_join(running_back_deep, by = c("gameId", "playId"))

plays_select <- plays_select %>%
  mutate(rb_deep = as.factor(ifelse(rb_depth >= 4, 1, 0))) %>%
  select(-rb_depth)

fullbacks <- tracking %>%
  filter(PositionAbbr == "FB") %>%
  filter(event == "ball_snap") %>%
  group_by(gameId, playId) %>%
  summarize(is_fullback = 1)

plays_select <- plays_select %>%
  left_join(fullbacks, by = c("gameId", "playId"))

plays_select$is_fullback[is.na(plays_select$is_fullback)] <- 0

plays_select$is_fullback <- as.factor(plays_select$is_fullback)
plays_select$is_shotgun <- as.factor(plays_select$is_shotgun)
plays_select$is_under_center <- as.factor(plays_select$is_under_center)
plays_select$is_pistol <- as.factor(plays_select$is_pistol)
plays_select$is_wildcat <- as.factor(plays_select$is_wildcat)
plays_select$prev_pass <- as.factor(plays_select$prev_pass)
plays_select$down <- as.factor(plays_select$down)

ball_los <- tracking %>%
  filter(frame.id == 1) %>%
  select(gameId, playId, y) 
names(ball_los)[3] <- c("los")

ball_los %>% 
  filter(los > 21) %>%
  filter(los < 32) %>%
  ggplot(aes(x = los)) +
  geom_density(fill = "darkgreen") +
  theme_bw() +
  scale_x_continuous(breaks = pretty_breaks(n = 15))

ball_los <- ball_los %>%
  mutate(hash = case_when(
    los <= 24.5 ~ "L",
    los >= 29 ~ "R",
    TRUE ~ "C"
  ))

plays <- plays %>%
  left_join(ball_los, by = c("gameId", "playId"))

str(plays_select)

simple_data_model <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber, 
         half_seconds_remaining, score_differential)

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

plays_join <- plays_select %>%
  filter(!is.na(width)) %>%
  distinct()

plays_model_data <- plays_join %>%
  select(pass_or_run, quarter, week, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_height, rb_deep, is_fullback, width_sd, linemen_sd)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

colSums(is.na(plays_model_data))

############################ Using Caret Package ##############################

set.seed(123)

training.samples <- simple_data_model$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- simple_data_model[training.samples, ]
test.data <- simple_data_model[-training.samples, ]

model <- glm(pass_or_run ~ ., 
             data = train.data, family = binomial)

summary(model)

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

BrierScore(model)
#Simple model's brier score is 0.198

mean(predicted.classes == test.data$pass_or_run)
#Simple model's prediction rate is 63.2%

set.seed(234)

rf_split <- initial_split(plays_model_data, strata = pass_or_run)
rf_train <- training(rf_split)
rf_test <- testing(rf_split)

rf_rec <- recipe(pass_or_run ~ ., data = rf_train)

rf_prep <- prep(rf_rec)
juiced <- juice(rf_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(tune_spec)

set.seed(345)
rf_folds <- vfold_cv(rf_train)

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = rf_folds,
  grid = 10
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE, size = 5) +
  theme_bw() +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

rf_grid <- grid_regular(
  mtry(range = c(0, 10)),
  min_n(range = c(25, 35)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = rf_folds,
  grid = rf_grid
)

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

library(vip)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(juiced$pass_or_run ~ .,
      data = juiced %>% select(-pass_or_run)
  ) %>%
  vip(geom = "point") +
  theme_bw()

final_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(rf_split)

final_res %>%
  collect_metrics()

test_preds <- final_res %>%
  collect_predictions() %>%
  bind_cols(rf_test)

simple_rf <- randomForest(pass_or_run ~ ., data = rf_train, ntree = 1000, 
                          importance= TRUE, mtry = 2, min_n = 25)

importance <- as.data.frame(simple_rf$importance)

mydf <- cbind(rownames(importance), importance)
rownames(mydf) <- NULL
colnames(mydf) <- c("variable", "Pass","Run","MeanDecreaseAccuracy", "MeanDecreaseGini")

mydf %>%
  filter(MeanDecreaseAccuracy > 0.005) %>%
  mutate(type = fct_reorder(variable, MeanDecreaseAccuracy)) %>%
  ggplot(aes(x = MeanDecreaseAccuracy, y = type)) +
  geom_bar(stat = "identity", fill = "darkred", color = "darkblue", alpha = 0.9) +
  theme_reach() +
  labs(y = "Variable",
       x = "Mean Decrease in Accuracy",
       title = "Pass or Run Variable Importance")
ggsave('tracking-vip.png', width = 15, height = 10, dpi = "retina")

rf_test_probs <- predict(simple_rf, rf_test, type = "prob")
rf_test_preds <- predict(simple_rf, rf_test, type = "response")

all_probs <- predict(simple_rf, plays_model_data, type = "prob")
all_preds <- predict(simple_rf, plays_model_data, type = "response")

probs_and_preds <- cbind(plays_join, all_probs, all_preds)

probs_and_preds <- probs_and_preds %>%
  mutate(is_pass = ifelse(pass_or_run == "Pass", 1, 0),
         proe = is_pass - Pass)

proe_stats <- probs_and_preds %>%
  group_by(possessionTeam) %>%
  summarize(avg_proe = 100*mean(proe, na.rm = T),
            brier = mean((Pass-probs_and_preds$is_pass)^2)) %>%
  left_join(teams_colors_logos, by = c("possessionTeam" = "team_abbr")) %>%
  arrange(-avg_proe)

proe_stats %>%
  mutate(team = fct_reorder(possessionTeam, -avg_proe)) %>%
  ggplot(aes(x = team, y = avg_proe)) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.9) +
  geom_image(aes(x = team, y = ifelse(avg_proe > 0, avg_proe + 0.5, avg_proe - 0.5), image = team_logo_espn),
             asp = 16/9, size = 0.035) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  labs(x = "Team",
       y = "Pass Rate Over Expected",
       title = "Each Team's Pass Rate Over Expcted, 2017",
       subtitle = "Data from the Big Data Bowl, Week 1-6") +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, colour = "gray")) +
  scale_y_continuous(breaks = pretty_breaks(n = 10))
ggsave('tracking-1.png', width = 15, height = 10, dpi = "retina")

proe_stats %>%
  mutate(team = fct_reorder(possessionTeam, brier)) %>%
  ggplot(aes(x = team, y = brier)) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.9) +
  #geom_image(aes(x = team, y = brier + 0.01, image = team_logo_espn), asp = 16/9, size = 0.035) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_reach() +
  labs(x = "Team",
       y = "Brier Score",
       title = "Each Team's Expected Pass Brier Score, 2017",
       subtitle = "Data from the Big Data Bowl, Week 1-6") +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, colour = "gray")) +
  scale_y_continuous(breaks = pretty_breaks(n = 10), limits = c(0.25, 0.41))
  

expected_pass <- probs_and_preds %>%
  filter(Pass >= 0.5) %>%
  group_by(possessionTeam) %>%
  summarize(pass_rate = mean(is_pass),
            pass_brier = mean((Pass - probs_and_preds$is_pass)^2))

expected_run <- probs_and_preds %>%
  filter(Run >= 0.5) %>%
  group_by(possessionTeam) %>%
  summarize(run_rate = 1 - mean(is_pass),
            run_brier = mean((Run - (1-probs_and_preds$is_pass))^2))

expected_all <- expected_pass %>%
  left_join(expected_run, by = c("possessionTeam")) %>%
  left_join(teams_colors_logos, by = c("possessionTeam" = "team_abbr"))

expected_all %>%
  ggplot(aes(x = pass_brier, y = run_brier)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  theme_reach() +
  geom_hline(yintercept = mean(expected_all$run_brier), linetype = "dashed") +
  geom_vline(xintercept = mean(expected_all$pass_brier), linetype = "dashed") +
  labs(x = "Pass Brier Score",
       y = "Run Brier Score",
       title = "Team Predictability in 2017 on Runs and Passes",
       subtitle = "Data from the Big Data Bowl, Week 1-6") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  annotate("text", x = 0.915, y = 0.935, label = "Pass Predictable \n Run Predictable") +
  annotate("text", x = 0.915, y = 0.745, label = "Pass Predictable \n Run Unpredictable") +
  annotate("text", x = 0.725, y = 0.745, label = "Pass Unpredictable \n Run Unpredictable") +
  annotate("text", x = 0.725, y = 0.935, label = "Pass Unpredictable \n Run Unpredictable") 
ggsave('tracking-2.png', width = 15, height = 10, dpi = "retina")

confusionMatrix(rf_test_preds, rf_test$pass_or_run)

proe_stats <- proe_stats %>%
  mutate(posteam = ifelse(possessionTeam == "OAK", "LV", possessionTeam))

epa_stats <- pbp_17 %>%
  filter(week < 7) %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam) %>%
  summarize(avg_epa = mean(epa, na.rm = T)) %>%
  left_join(proe_stats, by = c("posteam"))

cor(epa_stats$brier, epa_stats$avg_epa)

epa_stats %>%
  ggplot(aes(x = brier, y = avg_epa)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  geom_smooth(method = "lm", color = "black") +
  theme_reach() +
  geom_hline(yintercept = mean(expected_all$avg_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(expected_all$brier), linetype = "dashed") +
  labs(x = "Brier Score",
       y = "EPA/Play",
       title = "How Predictability Affects a Team's Offensive EPA/Play",
       subtitle = "Data from the Big Data Bowl, Week 1-6") +
  scale_y_continuous(breaks = pretty_breaks(n = 10)) +
  scale_x_reverse(breaks = pretty_breaks(n = 10)) 

