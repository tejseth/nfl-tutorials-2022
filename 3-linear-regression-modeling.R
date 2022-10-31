# Tutorial 1: M-FANS: Introduction to NFL Data with R (2022): https://www.youtube.com/watch?v=r9SelUodNas&t=1359s&ab_channel=MFANS
# Tutorial 2: M-FANS: Data Viz with NFL Data (2022): https://www.youtube.com/watch?v=vVOagBhHo9A&t=14s&ab_channel=MFANS
# Tutorial 3: 

# Load packages we've already installed
library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)

# Load in play-by-play data
pbp <- load_pbp(2018:2022)

# Getting just 4th downs
fourth_downs <- pbp |> 
  filter(down == 4, !is.na(play_type))

# See what usually happens on 4th down
fourth_downs |> 
  group_by(play_type) |> 
  tally(sort = T) |> 
  print(n = 10)

# Creating an indicator variable
fourth_downs <- fourth_downs |> 
  mutate(went_for_it = ifelse(play_type %in% c("pass", "run"), 1, 0))

# Seeing which variables correlate with going for it
fourth_downs |> 
  group_by(ydstogo) |> 
  summarize(count = n(), 
            went_for_it_rate = mean(went_for_it)) |> 
  filter(count >= 5) |> 
  ggplot(aes(x = ydstogo, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

fourth_downs |> 
  group_by(yardline_100) |> 
  summarize(count = n(), 
            went_for_it_rate = mean(went_for_it)) |> 
  filter(count >= 5) |> 
  ggplot(aes(x = yardline_100, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

fourth_downs |> 
  group_by(ydstogo) |> 
  summarize(went_for_it_rate = mean(went_for_it)) |> 
  ggplot(aes(x = ydstogo, y = went_for_it_rate)) +
  geom_bar(stat = "identity") +
  theme_minimal()

fourth_downs |> 
  mutate(wp_rounded = round(wp, 2)) |> 
  group_by(wp_rounded) |> 
  summarize(count = n(), 
            went_for_it_rate = mean(went_for_it)) |> 
  ggplot(aes(x = wp_rounded, y = went_for_it_rate)) +
  geom_bar(aes(fill = went_for_it_rate), stat = "identity") +
  theme_minimal()

# Making a logistic regression model
log_fourth <- glm(went_for_it ~ yardline_100 + ydstogo + wp, 
                  data = fourth_downs)

# Getting the summary
summary(log_fourth)

# Getting the variable importance
vip(log_fourth)

# Accounting for interaction effects
log_fourth_co <- glm(went_for_it ~ (yardline_100 + ydstogo + wp)^2, 
                  data = fourth_downs)

summary(log_fourth_co)

# Checking the prediction probabilities
fourth_downs |> 
  mutate(pred_prob = log_fourth$fitted.values) %>%
  ggplot(aes(x = ydstogo)) +
  geom_line(aes(y = pred_prob), color = "black", size = 2) +
  geom_point(aes(y = went_for_it, color = ifelse(went_for_it == 1, "darkgreen", "darkred")), 
             alpha = 0.3) +
  scale_color_identity() +
  theme_minimal() +
  labs(x = "Yards to Go",
       y = "Chance Offense Will Go For It (0-1)")

# Getting fourth down go's over expected
fourth_downs <- fourth_downs %>%
  mutate(pred_prob = log_fourth$fitted.values) %>%
  mutate(fourth_oe = went_for_it - pred_prob)

# Team stats for 2022
team_fourth_22 <- fourth_downs |> 
  filter(season == 2022) |> 
  group_by(posteam) |> 
  summarize(count = n(),
            exp_fourths = sum(pred_prob),
            actual_fourths = sum(went_for_it),
            fourths_oe = sum(fourth_oe)) |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Plotting Fourth Down stats
team_fourth_22 |> 
  ggplot(aes(x = exp_fourths, y = actual_fourths)) +
  geom_hline(yintercept = mean(team_fourth_22$actual_fourths), linetype = "dashed") +
  geom_vline(xintercept = mean(team_fourth_22$exp_fourths), linetype = "dashed") +
  geom_smooth(method = "lm", color = "black", size = 1.5, alpha = 0.5, se = FALSE) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.04) +
  theme_minimal() +
  labs(x = "Expected 4th Down Go's",
       y = "Actual 4th Down Go's",
       title = "Team 4th Down Actual and Expected Go's",
       subtitle = "Based on logistic regression model") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8))

###################### Making Linear Regression Model ########################

pass_plays <- pbp |> 
  filter(pass == 1) %>%
  filter(!is.na(air_yards), !is.na(down), !is.na(wp),
         !is.na(ydstogo), !is.na(half_seconds_remaining)) 

# Select just the columns we want
pass_play_model <- pass_plays |> 
  select(air_yards, down, wp, ydstogo, half_seconds_remaining, season) |> 
  mutate(down = as.factor(down))

str(pass_play_model)
colSums(is.na(pass_play_model))

# Make the linear model
air_yards_lm <- lm(air_yards ~ down + wp + ydstogo + 
                     half_seconds_remaining + as.factor(season), 
                   data = pass_play_model)

summary(air_yards_lm)

vip(air_yards_lm, num_features = 12)

# Getting predictions
air_yard_preds <- data.frame(predict.lm(air_yards_lm, newdata = pass_play_model)) |> 
  rename(exp_air_yards = predict.lm.air_yards_lm..newdata...pass_play_model.)

# Binding it to data frame
air_yards_projs <- cbind(pass_plays, air_yard_preds)

# Leaders in air yards over expected in 2021
ayoe_22 <- air_yards_projs |> 
  mutate(ayoe = air_yards - exp_air_yards) |> 
  filter(season == 2022) |> 
  group_by(passer) |> 
  summarize(passes = n(),
            exp_air_yards = mean(exp_air_yards),
            adot = mean(air_yards),
            avg_ayoe = mean(ayoe),
            team = last(posteam)) |> 
  filter(passes >= 140) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Making a bar graph
ayoe_22 |> 
  ggplot(aes(x = avg_ayoe, y = fct_reorder(passer, avg_ayoe))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.6) +
  scale_fill_identity(aesthetics = c("fill", "color")) +
  geom_text(aes(label = paste0("n=", passes)), x = min(ayoe_22$avg_ayoe)-0.05, size = 3.5) +
  labs(x = "Average Air Yards Over Expected",
       y = "",
       title = "Average Air Yards Over Expected, 2022",
       subtitle = "Minimum of 140 passes on the season") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) 
 

