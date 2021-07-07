library(tidyverse)
library(tidymodels)
library(vip)
library(data.table)
library(ggthemes)
library(caret)
library(mlbench)
library(DescTools)
library(randomForest)

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
  select(gameId, playId,week, quarter, GameClock, down, yardsToGo, possessionTeam, 
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
  summarise(linemen_width = max(y)- min(y), 
            linemen_height = max(x) - min(x))

plays_select <- plays_select %>%
  left_join(linemen_width, by = c("gameId", "playId"))

plays_select <- plays_select %>%
  mutate(prev_play = lag(pass_or_run)) %>%
  mutate(prev_pass = ifelse(prev_play == "Pass", 1, 0)) %>%
  select(-prev_play)

plays_select$prev_pass[is.na(plays_select$prev_pass)] <- 0

plays_select %>%
  filter(linemen_height < 2) %>%
  ggplot(aes(x = pass_or_run, y = linemen_height)) +
  geom_jitter(color = "black", alpha = 0.05) +
  geom_boxplot(aes(fill = pass_or_run)) +
  scale_fill_brewer(palette = "Set2") +
  theme_reach() +
  labs(y = "Height of Offensive Line",
       x = "",
       title = "How Height of Offensive Line Affected Runs and Passes in 2017")
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

str(plays_select)

simple_data_model <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, down, yardsToGo, yardlineNumber,
         is_shotgun, half_seconds_remaining, score_differential)

simple_data_model$pass_or_run <- as.factor(simple_data_model$pass_or_run)

plays_model_data <- plays_select %>%
  filter(!is.na(width)) %>%
  select(pass_or_run, quarter, week, down, yardsToGo, yardlineNumber, num_rb, num_wr, num_te,
         is_shotgun, is_under_center, is_pistol, is_wildcat, defendersInTheBox,
         half_seconds_remaining, score_differential, width, linemen_width, prev_pass,
         linemen_height, rb_deep, is_fullback)

plays_model_data$pass_or_run <- as.factor(plays_model_data$pass_or_run)

colSums(is.na(plays_model_data))

############################ Using Caret Package ##############################

set.seed(123)

training.samples <- simple_data_model$pass_or_run %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data  <- simple_data_model[training.samples, ]
test.data <- simple_data_model[-training.samples, ]

model <- glm(pass_or_run ~., data = train.data, family = binomial)

summary(model)

probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Run", "Pass")

BrierScore(model)
#Simple model's brier score is 0.198

mean(predicted.classes == test.data$pass_or_run)
#Simple model's prediction rate is 72.6%

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

tune_res

simple_rf <- randomForest(pass_or_run ~ ., data = rf_train, ntree = 1000, importance= TRUE)

importance <- as.data.frame(simple_rf$importance)

rf_test_probs <- predict(simple_rf, rf_test, type = "prob")
rf_test_preds <- predict(simple_rf, rf_test, type = "response")

confusionMatrix(rf_test_preds, rf_test$pass_or_run)

BrierScore(rf_test_probs, as.numeric(rf_test$pass_or_run) - 1)


