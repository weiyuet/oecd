# Setup
library(tidyverse)
library(janitor)
library(scales)

# Load data
electricity_generation <- read_csv("data/electricity-generation.csv")

# Wrangle
# Column names to lower case
electricity_generation <- electricity_generation %>% 
  clean_names()

# Change column name
electricity_generation <- electricity_generation %>% 
  rename(type = "subject")

# Visualize
electricity_generation %>% 
  filter(location == "JPN") %>% 
  filter(measure == "GWH") %>% 
  ggplot(aes(x = time, y = value, colour = type)) +
  geom_step() +
  scale_x_continuous(expand = c(0.01, 0),
                     breaks = seq(1960, 2022, 5)) +
  scale_y_continuous(expand = c(0.01, 0),
                     labels = label_number(suffix = " GWh",
                                           big.mark = ",")) +
  theme_bw() +
  theme(legend.position = c(0.2, 0.85)) +
  labs(x = "", y = "",
       colour = "",
       title = "Electricity Generation in Japan",
       subtitle = "Nuclear power generation practically stopped after the 2011 earthquake",
       caption = "Data: OECD (2022), Electricity generation (indicator). doi: 10.1787/c6e6caa2-en")

# Save image
ggsave("figures/electricity-generation-japan.png", width = 7, height = 5)