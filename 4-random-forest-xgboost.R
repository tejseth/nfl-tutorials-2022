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



