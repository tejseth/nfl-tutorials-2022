# install.packages("ggthemes")
# install.packages("ggrepel")

library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)

# Note to tej: hold command + to zoom in so it's easier to see the code

# load in the play-by-play data
pbp <- load_pbp(2019:2022)

# get QB EPA/play, create a pass rate column and join teams_colors_logos
qb_epa_play <- pbp |> 
  filter(pass == 1 | rush == 1, !is.na(epa)) |> 
  group_by(id) |> 
  summarize(name = first(name),
            team = last(posteam),
            plays = n(),
            epa_play = mean(epa),
            pass_attempts = sum(incomplete_pass + complete_pass, na.rm = T)) |> 
  filter(plays >= 1000, pass_attempts >= 150) |> 
  mutate(pass_rate = pass_attempts / plays) |> 
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# make a scatter plot with qb_epa_play
qb_epa_play |> 
  ggplot(aes(x = pass_rate, y = epa_play)) +
  geom_point(aes(fill = team_color, color = team_color2, size = plays), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = name)) +
  theme_bw() +
  geom_hline(yintercept = mean(qb_epa_play$epa_play), linetype = "dashed") +
  geom_vline(xintercept = mean(qb_epa_play$pass_rate), linetype = "dashed") +
  labs(x = "Pass Rate",
       y = "EPA/Play",
       title = "EPA/Play and Pass Rate, 2019-2022",
       subtitle = "Minimum of 600 plays and 150 pass attempts to be included") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))
# save the plot
ggsave('epa-pass-rate.png', width = 14, height = 10, dpi = "retina")

# make a bar graph with the same data
qb_epa_play |> 
  ggplot(aes(x = epa_play, y = fct_reorder(name, epa_play))) +
  geom_bar(aes(fill = team_color, color = team_color2), 
           stat = "identity", alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_minimal() +
  geom_image(aes(image = team_logo_espn, x = ifelse(epa_play > 0, epa_play + 0.01, epa_play - 0.01)), 
             asp = 16/9, size = 0.035) +
  labs(x = "EPA/Play",
       y = "Quarterback",
       title = "Each Quarterback's EPA/Play, 2019-2022",
       subtitle = "Minimum of 1000 plays and 150 pass attempts") +
  theme(panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5)) 
ggsave('bar-epa.png', width = 14, height = 10, dpi = "retina")

# Make a nice gt table with the same information
qb_gt <- qb_epa_play |> 
  arrange(-epa_play) |>
  mutate(rank = row_number()) |> 
  dplyr::select(rank, name, team_wordmark, pass_attempts, plays, pass_rate, epa_play) |> 
  mutate(pass_rate = 100*round(pass_rate, 3),
         epa_play = round(epa_play, 2)) |> 
  gt() |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_wordmark) |> 
  cols_label(rank = "Rank",
             name = "Quarterback",
             team_wordmark = "",
             pass_attempts = "Pass Attempts",
             plays = "Plays",
             pass_rate = "Pass Rate",
             epa_play = "EPA Per Play") |> 
  gtExtras::gt_theme_espn() |> 
  gtExtras::gt_hulk_col_numeric(epa_play)
#save the gt table
gtsave(qb_gt, "qb_gt.png")

