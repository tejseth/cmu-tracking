frame_games <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/games.csv"))
frame_players <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/players.csv"))
frame_plays <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/plays.csv"))
frame_game1 <- read_csv(url("https://raw.githubusercontent.com/tejseth/Big-Data-Bowl-1/master/Data/tracking_gameId_2017090700.csv"))

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

frame_tracking <- rbindlist(df_tracking)

frame_games_select <- frame_games %>%
  select(gameId, week)

frame_plays <- frame_plays %>%
  left_join(games_select, by = c("gameId"))

frame_plays_select <- frame_plays %>%
  filter(isSTPlay == "FALSE") %>%
  mutate(is_pass = ifelse(is.na(PassLength), 0, 1)) %>%
  select(gameId, playId, playDescription, is_pass)

frame_plays_select <- frame_plays_select %>%
  mutate(sacked = ifelse(grepl("sacked", frame_plays_select$playDescription), TRUE, FALSE))

frame_plays_select <- frame_plays_select %>%
  mutate(is_pass = ifelse(sacked == TRUE, 1, is_pass)) %>%
  select(-sacked, -playDescription)

ball_snaps <- frame_tracking %>%
  filter(event == "ball_snap")

min(ball_snaps$frame.id)

frame_tracking <- frame_tracking %>%
  left_join(frame_plays_select, by = c("gameId", "playId")) %>%
  filter(!is.na(is_pass))

probs_select <- probs_and_preds %>%
  select(gameId, playId, xpass)

frame_tracking <- frame_tracking %>%
  left_join(probs_select, by = c("gameId", "playId"))



