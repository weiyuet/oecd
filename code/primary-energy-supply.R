# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)

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
  scale_y_log10(labels = label_number(big.mark = ",")) +
  scale_colour_jco()