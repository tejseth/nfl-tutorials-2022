# Tutorial 1: https://www.youtube.com/watch?v=r9SelUodNas&t=1371s
# Tutorial 2: https://www.youtube.com/watch?v=vVOagBhHo9A&t=15s
# Tutorial 3: https://www.youtube.com/watch?v=uJNjCxuBTn8&t=1546s

# Load Packages
library(tidyverse)
library(nflfastR)
library(ggthemes)
library(ranger)
library(vip)
library(caret)
library(xgboost)
library(ggimage)
options(scipen = 9999)

# Load in play-by-play data
pbp <- load_pbp(2015:2022)

# Find out when field goals happen
pbp |> 
  filter(!is.na(down), !is.na(field_goal_attempt)) |> 
  group_by(down) |> 
  summarize(field_goal_attempts = sum(field_goal_attempt))

# Get data for field goals
field_goals <- pbp |> 
  filter(field_goal_attempt == 1) |> 
  mutate(field_goal_made = ifelse(field_goal_result == "made", 1, 0)) |> 
  select(yardline_100, half_seconds_remaining, 
         goal_to_go, score_differential, roof, surface,
         temp, wind, field_goal_made, kicker_player_name) 

# Check the NA values
colSums(is.na(field_goals))

# Remove the roof NA rows since there's a small number
field_goals <- field_goals |> 
  filter(!is.na(roof))

# Check where the NA's are coming from
field_goals |> 
  filter(is.na(temp)) |> 
  group_by(roof) |> 
  tally()

field_goals |> 
  filter(is.na(wind)) |> 
  group_by(roof) |> 
  tally()

field_goals |> 
  group_by(roof) |> 
  summarize(median_wind = median(wind, na.rm = T))

# Put in room temparature and no wind for NA's
field_goals$temp[is.na(field_goals$temp)] <- 68
field_goals$wind[is.na(field_goals$wind)] <- 0

# Check what is a factor and not
str(field_goals)

# Chance goal_to_go to a factor
field_goals$goal_to_go <- as.factor(field_goals$goal_to_go)

# Build the Random Forest
field_goal_rf <- randomForest(as.factor(field_goal_made) ~ yardline_100 +
                                half_seconds_remaining +
                                score_differential + roof + surface + temp + wind,
                              data = field_goals)

# Check the forest
field_goal_rf

# Feature importance
vip(field_goal_rf)

# Get predictions
fg_preds <- predict(field_goal_rf, type="prob")

# Combine with original dataset
fg_preds_joined <- cbind(field_goals, fg_preds)

# See the leaderboard
fg_preds_joined |> 
  mutate(fg_oe = field_goal_made - `1`) |> 
  group_by(kicker_player_name) |> 
  summarize(kicks_oe_made = sum(fg_oe)) |> 
  arrange(-kicks_oe_made)

##################### Let's make rushing yards over expected ################

# Getting just rushes
rush_attempts <- pbp |> 
  filter(rush_attempt == 1, qb_scramble == 0, 
         qb_dropback == 0, !is.na(yards_gained))

# Making defensive yards per carry
def_ypc <- rush_attempts |> 
  filter(!is.na(defteam)) |> 
  group_by(season, defteam) |> 
  summarize(def_ypc = mean(yards_gained))

rush_attempts <- rush_attempts |> 
  left_join(def_ypc, by = c("season", "defteam"))

# Getting the join dataset ready
rushing_data_join <- rush_attempts |> 
  select(label = yards_gained, yardline_100, quarter_seconds_remaining, half_seconds_remaining,
         game_seconds_remaining, qtr, down, ydstogo, shotgun, no_huddle,
         ep, wp, def_ypc, rusher_player_name, posteam, defteam) |> 
  filter(!is.na(label)) |> 
  filter(!is.na(down))

rushes <- rushing_data_join |> 
  select(-rusher_player_name, -posteam, -defteam)

str(rushes)

# Creating factors
rushes$down <- as.factor(rushes$down)
rushes$shotgun <- as.factor(rushes$shotgun)
rushes$no_huddle <- as.factor(rushes$no_huddle)

# One hot encoding
dmy <- dummyVars(" ~ .", data = rushes)
rushing_model_data <- data.frame(predict(dmy, newdata = rushes))

colSums(is.na(rushing_model_data))

# Making train and test datasets
smp_size <- floor(0.50 * nrow(rushing_model_data))
set.seed(2011) #go lions
ind <- sample(seq_len(nrow(rushing_model_data)), size = smp_size)
train <- as.matrix(rushing_model_data[ind, ])
test <- as.matrix(rushing_model_data[-ind, ])

dim(train)
colnames(train)

# Making the model
ryoe_model <-
  xgboost(
    data = train[, 2:18],
    label = train[, 1],
    nrounds = 100,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

vip(ryoe_model)

xgb.plot.tree(model = ryoe_model, trees = 1)

pred_xgb <- predict(ryoe_model, test[, 2:18])

# Seeing our RMSE
yhat <- pred_xgb
y <- test[, 1]
postResample(yhat, y)

rushing_preds <- as.data.frame(
  matrix(predict(ryoe_model, as.matrix(rushing_model_data %>% select(-label))))
) %>%
  dplyr::rename(exp_yards = V1)

ryoe_projs <- cbind(rushing_data_join, rushing_preds)

ryoe_projs |> 
  mutate(ryoe = label - exp_yards) |> 
  group_by(rusher_player_name) |> 
  summarize(rushes = n(),
            total_ryoe = sum(ryoe)) |> 
  arrange(-total_ryoe)



