####################
# Renewable Energy #
####################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

#### Load data ####
renewable_energy <- read_csv("data/renewable-energy.csv")

#### Wrangle ####
# Column names to lower case
renewable_energy <- renewable_energy %>% 
  clean_names()

#### Visualize ####
# What is the gross production of renewable energy?
selected_locations <- c("WLD", "AUS", "BRA", "CHN", "DEU", "FRA", "IDN", "ISL", "JPN", "USA")

renewable_energy %>% 
  filter(location %in% selected_locations) %>% 
  filter(measure == "KTOE") %>%
  mutate(value = value / 1000) %>% 
  ggplot(aes(x = time,
             y = value,
             colour = location)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1960, 2020, 5)) +
  scale_y_log10(labels = label_number(big.mark = "")) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       colour = "",
       title = "Renewable Energy Production",
       subtitle = "million toe (y-axis log scale)",
       caption = "Data: OECD (2022), Renewable energy (indicator). doi: 10.1787/aac7c3f1-en | Graphic: @weiyuet") +
  theme_bw()

#### Save image ####
ggsave("figures/renewable-energy.png", width = 7, height = 7)

#### Visualize ####
# What is the percentage of renewable energy per primary energy supply?
selected_locations <- c("ISL", "JPN", "NOR")

renewable_energy %>% 
  filter(location %in% selected_locations) %>% 
  filter(measure == "PC_PRYENRGSUPPLY") %>% 
  ggplot(aes(x = time,
             y = value,
             colour = location)) +
  geom_step() +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 100, 10),
                     labels = label_number(suffix = "%")) +
  scale_color_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       colour = "",
       title = "Pecentage of Primary Energy Supply from Renewable Energy",
       caption = "Data: OECD (2022), Renewable energy (indicator). doi: 10.1787/aac7c3f1-en | Graphic: @weiyuet") +
  theme_bw()

#### Save image ####
ggsave("figures/percentage-renewable-energy.png", width = 7, height = 7)