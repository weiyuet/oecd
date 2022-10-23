# Setup
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

# Load data
renewable_energy <- read_csv("data/renewable-energy.csv")

# Column names to lower case
renewable_energy <- renewable_energy %>% 
  clean_names()

# Visualize
selected_locations <- c("WLD", "AUS", "BRA", "CHN", "DEU", "IDN", "ISL", "JPN", "USA")

renewable_energy %>% 
  filter(location %in% selected_locations) %>% 
  filter(measure == "KTOE") %>%
  mutate(value = value / 1000) %>% 
  ggplot(aes(x = time, y = value, colour = location)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  theme_bw() +
  labs(x = "", y = "",
       colour = "",
       title = "Renewable Energy Production",
       subtitle = "million toe (y-axis log scale)",
       caption = "Data: OECD (2022), Renewable energy (indicator). doi: 10.1787/aac7c3f1-en | Graphic: @weiyuet")

# Save image
ggsave("figures/renewable-energy.png", width = 7, height = 7)