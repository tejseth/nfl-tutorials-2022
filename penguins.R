# Install these packages by removing the '#' before the line

# install.packages("tidyverse")
# install.pacakges("gt")
# install.packages("gtExtras")
# install.packages("palmerpenguins")

# Load in the packages we just installed
library(tidyverse)
library(gt)
library(palmerpenguins)

# Assign our dataset to a variable 
penguins <- penguins

# Check how many rows are in the dataset
nrow(penguins)

# Display the first 5 rows of the dataset
head(penguins)

# Print the types of each column
str(penguins)

# Check select columns
penguins %>% select(species, bill_length_mm, bill_depth_mm)

# See where the NA's are occuring before going further
colSums(is.na(penguins))

# Filter out the NA's
penguins_clean <- penguins %>%
  filter(!is.na(sex))

colSums(is.na(penguins_clean))

# Get stats about each type of island
penguins_clean %>%
  group_by(island) %>%
  summarize(count = n(), 
            avg_bill_len = mean(bill_length_mm),
            avg_bill_depth = mean(bill_depth_mm),
            median_body_mass = median(body_mass_g))

# Making a graph using this data
penguins_clean %>%
ggplot(aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species), size = 3, alpha = 0.8) +
  geom_smooth(aes(color = species), method = "lm", se = FALSE) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "How Flipper Length and Body Mass Correlate",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)") +
  theme_minimal()

# Making another graph
penguins_clean %>%
  count(sex, species) %>%
  ggplot() + 
  geom_col(aes(x = species, y = n, fill = species)) +
  geom_label(aes(x = species, y = n, label = n)) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  facet_wrap(~sex) +
  theme_minimal() +
  labs(title = 'Penguins Species and Gender')

# Converting body weight to points to make it more understandable
penguins_clean <- penguins_clean %>%
  mutate(body_weight_pounds = body_mass_g / 453.6)

# Making a table
species_gender <- penguins_clean %>%
  group_by(species, sex) %>%
  summarize(count = n(), 
            avg_bill_len = mean(bill_length_mm),
            avg_bill_depth = mean(bill_depth_mm),
            median_body_mass = median(body_weight_pounds))

species_gender %>% 
  ungroup() %>%
  mutate(avg_bill_len = round(avg_bill_len, 1),
         avg_bill_depth = round(avg_bill_depth, 1),
         median_body_mass = round(median_body_mass, 1)) %>%
  arrange(-median_body_mass) %>%
  gt() %>%
  cols_align(align = "center") %>%
  gtExtras::gt_hulk_col_numeric(median_body_mass) %>%
  gtExtras::gt_theme_538() %>%
  cols_label(species = "Species",
             sex = "Gender",
             count = "Count",
             avg_bill_len = "Avg. Bill Length",
             avg_bill_depth = "Avg. Bill Depth",
             median_body_mass = "Median Body Pass") %>%
  tab_header("Descritpive Stats for Palmer Penguins")







