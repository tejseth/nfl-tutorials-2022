# download r and r studio here: http://mathcenter.oxford.emory.edu/site/math117/installRStudio/

# remove the '#' if you haven't installed these packages already

# install.packages("tidyverse")
# install.packages("nflfastR")
# install.packages("ggimage")
# install.packages("gt")
# install.packages("gtExtras")

# Load the packages we installed
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

# Load multiple years of pbp data
pbp <- load_pbp(2021:2022)

# Check how many rows are in the data
nrow(pbp)

# Take a look at the first 5 rows of the data
# If you want to change your pipe to |> go to "Global Options" and then "Code"
pbp |> head()

# Take a look at all the columns
names(pbp)

# Select just a couple columns to look at 
pbp |> select(posteam, defteam, down, ydstogo, play_type, yards_gained)

# Let's just get runs and passes
pbp_rp <- pbp |> 
  filter(pass == 1 | rush == 1) |> 
  # and get plays that have epa
  filter(!is.na(epa))

# Now we can see that our dataset got smaller
nrow(pbp_rp)

# Who was the Lions best rusher last season?
pbp_rp |> 
  filter(posteam == "DET", rush == 1, !is.na(rusher_player_name)) |> 
  group_by(rusher_player_name) |> 
  summarize(rushes = n(),
            epa_rush = mean(epa)) |> 
  filter(rushes >= 10) |> 
  arrange(-epa_rush)

# Who was the Dolphins best quarterback last season?
pbp_rp |> 
  filter(posteam == "CAR", !is.na(id)) |> 
  group_by(id) |> 
  summarize(name = first(name),
            plays = n(),
            epa_per_play = mean(epa),
            pass_attempts = sum(complete_pass + incomplete_pass, na.rm = T)) |> 
  filter(plays >= 50, pass_attempts >= 10) |> 
  arrange(-epa_per_play)

# Compare pass efficiency vs. rush efficiency last season
pass_efficiency_21 <- pbp |> 
  filter(season == 2021, pass == 1) |> 
  group_by(posteam) |> 
  summarize(passes = n(),
            pass_epa = mean(epa))

rush_efficiency_21 <- pbp |> 
  filter(season == 2021, rush == 1) |> 
  group_by(posteam) |> 
  summarize(rushes = n(),
            rush_epa = mean(epa))

# Join the two datasets together
total_eff <- pass_efficiency_21 |> 
  left_join(rush_efficiency_21, by = "posteam")

# Look at the teams_colors_logos dataset
View(teams_colors_logos)

# Join the team logos with total_eff
total_eff <- total_eff |> 
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

# Make the plot
total_eff |> 
  ggplot(aes(x = pass_epa, y = rush_epa)) +
  geom_hline(yintercept = mean(total_eff$rush_epa), linetype = "dashed") +
  geom_vline(xintercept = mean(total_eff$pass_epa), linetype = "dashed") +
  geom_smooth(method = "lm") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "EPA/pass",
       y = "EPA/rush",
       title = "EPA/pass and EPA/Rush in 2021",
       subtitle = "Regular season and playoffs included",
       caption = "By Tej Seth | @tejfbanalytics | M-FANS") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #put the title and subtitle in the middle

# Saving the plot
ggsave('pass-rush-epa-21.png', width = 14, height = 10, dpi = "retina")

# Now let's make a table to see how quarterback aggressiveness has changed
agg_21 <- pbp |> 
  mutate(yards_past_sticks = air_yards - ydstogo) |> 
  filter(season == 2021, down %in% c(3, 4), !is.na(passer_player_id)) |>
  group_by(passer_player_id) |> 
  summarize(name = first(name),
            passes_21 = n(),
            agg_21 = mean(yards_past_sticks, na.rm = T))

# Do the same thing for 2022
agg_22 <- pbp |> 
  mutate(yards_past_sticks = air_yards - ydstogo) |> 
  filter(season == 2022, down %in% c(3, 4), !is.na(passer_player_id)) |>
  group_by(passer_player_id) |> 
  summarize(passes_22 = n(),
            team = last(posteam), 
            agg_22 = mean(yards_past_sticks, na.rm = T))

# Combine 2021 and 2022 together
agg_21_22 <- agg_21 |> 
  left_join(agg_22, by = "passer_player_id") |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr")) |> 
  filter(passes_21 >= 100)

# Make the gt table
agg_21_22 |> 
  mutate(agg_21 = round(agg_21, 1),
         agg_22 = round(agg_22, 1),
         diff = agg_22 - agg_21) |> 
  select(name, team_wordmark, agg_21, agg_22, diff) |> 
  arrange(-diff) |> 
  gt() |>
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(name = "Quarterback",
             team_wordmark = "",
             agg_21 = "Late-Down YPS, 2021",
             agg_22 = "Late-Down YPS, 2022",
             diff = "Difference") |> 
  gtExtras::gt_theme_espn()

# Message me on twitter @tejfbanalytics if you have any problems!
  
  
  
  