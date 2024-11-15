library(tidyverse)
library(caTools)
library(dplyr)
library(stringr)
library(xgboost)
library(tidyr)
library(caret)
future::plan("multisession")

data <- read.csv('/Users/cadgo/OneDrive/Documents/CFB4thDown/cfb_playbyplay.csv', header = TRUE)

elodata <- read.csv('/Users/cadgo/OneDrive/Documents/CFB4thDown/cfb_elo_ratings.csv', header = TRUE)

data <- merge(data, elodata[, c("year", "team", "elo")], by.x = c("season", "home"), by.y = c("year", "team"), all.x = TRUE)

data <- data %>%
  rename(home_elo = elo)

data <- merge(data, elodata[, c("year", "team", "elo")], by.x = c("season", "away"), by.y = c("year", "team"), all.x = TRUE)

data <- data %>%
  rename(away_elo = elo)

data <-  data %>%
  mutate(home_elo = replace_na(home_elo, 0),
         away_elo = replace_na(away_elo, 0))

data <- data %>%
  mutate(home_elo_diff = home_elo - away_elo,
         home_favored = ifelse(home_elo_diff > 0, 1, 0))

data <- data %>%
  mutate(original_play_call = case_when(
    play_type %in% c("Pass reception", "Pass incompletion", "Passing Touchdown", "Sack", "Interception Return", "Interception Return Touchdown") ~ "Pass",
    play_type %in% c("Blocked Punt", "Blocked Punt (Safety)", "Blocked Punt Touchdown", "Punt", "Punt (Safety)", "Punt Return Touchdown", "Punt Team Fumble Recovery", "Punt Team Fumble Recovery Touchdown") ~ "Punt",
    play_type %in% c("Rush", "Rushing Touchdown") ~ "Run",
    play_type %in% c("Blocked Field Goal", "Blocked Field Goal Touchdown", "Field Goal Good", "Field Goal Missed", "Missed Field Goal Return", "Missed Field Goal Return") ~ "Field Goal",
    play_type %in% c("Kickoff", "Kickoff Team Fumble Recovery", "Kickoff Return (Offense)", "Kickoff Return Touchdown", "End of Regulation", "Fumble Recovery (Opponent)", "Fumble Recovery (Opponent) Touchdown", "Fumble Recovery (Own)", "Fumble Return Touchdown", "Penalty", "Safety", "Timeout", "Uncategorized") ~ "Misc",
  ))

data <- data %>%
  arrange(game_id, game_play_number) %>%
  mutate(
    team_week_year = paste(pos_team, 'Week', wk, season),
    
  )

data <- data %>%
  mutate(
    home_or_away = ifelse(pos_team == home, 1, 0),
    converted = ifelse(yards_gained >= distance | first_by_penalty == 1 | first_by_yards == 1, 1, 0),
    yards_gained = ifelse(yards_gained < -10, -10, yards_gained),
    yards_gained = ifelse(yards_gained > 65, 65, yards_gained),
  )

data <- data %>%
  group_by(pos_team) %>%
  mutate(
    original_play_call = replace_na(original_play_call, "Misc"),     # Handle any missing values in play call
    yards_gained = replace_na(yards_gained, 0),
    
    s_fourth_down_attempts = lag(cumsum(ifelse(down == 4 & original_play_call %in% c("Pass","Run"), 1, 0)), 1),
    s_fourth_down_success = lag(cumsum(ifelse(down == 4 & original_play_call %in% c("Pass","Run") & converted == 1, 1, 0)), 1),
    Season_Fourth_Down_Conversion_Rate = lag(ifelse(s_fourth_down_attempts > 0, s_fourth_down_success / s_fourth_down_attempts, 0), 1)
    # added a lag to the conversion rate so that the rate represents the historical rate leading up to that play
    
  ) %>%
  ungroup()


data <- data %>%
  group_by(team_week_year) %>%
  mutate(
    
    
    total_rush_yards = cumsum(ifelse(original_play_call == "Run", yards_gained, 0)),  
    rush_attempts = cumsum(ifelse(original_play_call == "Run", 1, 0)),               
    rush_yards_per_attempt = ifelse(rush_attempts > 0, total_rush_yards / rush_attempts, 0),
    total_pass_yards = cumsum(ifelse(original_play_call == "Pass", yards_gained, 0)),  
    pass_attempts = cumsum(ifelse(original_play_call == "Pass", 1, 0)),             
    pass_yards_per_attempt = ifelse(pass_attempts > 0, total_pass_yards/pass_attempts, 0),
    
    fourth_down_attempts = lag(cumsum(ifelse(down == 4 & original_play_call %in% c("Pass","Run"), 1, 0)), 1),
    fourth_down_success = lag(cumsum(ifelse(down == 4 & original_play_call %in% c("Pass","Run") & converted == 1, 1, 0)), 1),
    Fourth_Down_Conversion_Rate_Game = lag(ifelse(fourth_down_attempts > 0, fourth_down_success / fourth_down_attempts, 0), 1)
    # added a lag to the conversion rate so that the rate represents the historical rate leading up to that play
  
  ) %>%
  ungroup()


# new_df <- data[1:50, ]

# write.csv(new_df, "/Users/cadgo/OneDrive/Documents/cfb_sample_updated2.csv", row.names = FALSE)

### Model Creation

datas1 <- data %>%
  filter(down == 4)

#################### Use only go for it plays

datas1 <- datas1 %>% filter(original_play_call %in% c("Pass", "Run"))
# 10654 4th down conversion attempts

data_m <- datas1 %>%
  select(distance, yards_to_goal, original_play_call, pos_score_diff, 
         rush_yards_per_attempt, pass_yards_per_attempt, period, 
         Season_Fourth_Down_Conversion_Rate, home_or_away, home_favored, converted)

data_m$original_play_call <- as.factor(data_m$original_play_call)
data_m$home_or_away <- as.factor(data_m$home_or_away)


data_m$original_play_call <- as.numeric(data_m$original_play_call)
data_m$home_or_away <- as.numeric(data_m$home_or_away)

# pass is 1, run is 2

# Middle 8 False is 1, True is 2 # Middle 8 is insignificant

data_m <- na.omit(data_m)

set.seed(31) 
trainIndex <- createDataPartition(data_m$converted, p = 0.8, list = FALSE)
train_data <- data_m[trainIndex,]
test_data <- data_m[-trainIndex,]

train_matrix <- xgb.DMatrix(data = as.matrix(train_data[, -ncol(data_m)]), label = train_data$converted)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data[, -ncol(data_m)]), label = test_data$converted)


params <- list(
  booster = "gbtree",
  objective = "binary:logistic",  # Did or did not convert
  eval_metric = "logloss",  
  eta = 0.1,  # Learning rate # Higher learning rate leads to overfitting on training data
  max_depth = 6,  # Maximum depth of a tree
  gamma = 1,  # Minimum loss reduction required to make a further partition
  subsample = 0.8,  # Subsample ratio of the training instance
  colsample_bytree = 0.8  # Subsample ratio of columns when constructing each tree
)

xgb_model <- xgb.train(
  params = params,
  data = train_matrix,
  nrounds = 1000,  
  watchlist = list(train = train_matrix, test = test_matrix),
  early_stopping_rounds = 100,  
  verbose = 1
)

pred_prob <- predict(xgb_model, test_matrix)

# Convert probabilities to converted (1) or not converted (0)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

accuracy <- mean(pred_class == test_data$converted)
print(paste("Accuracy: ", accuracy))

confusionMatrix(as.factor(pred_class), as.factor(test_data$converted))

importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance)


saveRDS(xgb_model, "GoForIt_model.rds")

## Season 4th down attempts and success group_by(Pos_team)
# in shiny app, clairfy home_favored
# add model narrative, explain xgboost and the params
# Find data for team injuries, weather, crowd size 
# (crowd size to be removed due to lack of reliability of numbers team report)
# no injury data in play by play data
# Season 4th Down Success Rate more important than game 4th down success
# More variable without home or away, and home favored
# model 

View(data)

misc_data <- na.omit(data$play_text)
which(data$play_text %in% c("pass"), arr.ind = TRUE)
sum(str_detect(df$column_name, 'partial_string'))
sum(str_detect(misc_data, 'hurt'))

table(data$play_type)
data$first_by_penalty
data$first_by_yards
