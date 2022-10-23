# Setup
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

# Load data
primary_energy_supply <- read_csv("data/primary-energy-supply.csv")

# Wrangle
# Column names to lower case
primary_energy_supply <- primary_energy_supply %>% 
  clean_names()

# Visualize
selected_locations <- c("G7", "G20", "EU28", "OECD", "AUS", "CHN", "IDN", "JPN", "DEU", "USA")

primary_energy_supply %>% 
  filter(location %in% selected_locations) %>%
  filter(measure == "MLN_TOE") %>% 
  ggplot(aes(x = time, y = value, colour = location)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1960, 2025, 5)) +
  scale_y_log10(labels = label_number(big.mark = ",")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  theme_bw() +
  labs(x = "", y = "",
       colour = "",
       title = "Primary Energy Supply",
       subtitle = "million toe (y-axis log scale)",
       caption = "Data: OECD (2022), Primary energy supply (indicator). doi: 10.1787/1b33c15a-en | Graphic: @weiyuet")

# Save image
ggsave("figures/primary-energy-supply.png", width = 7, height = 7)