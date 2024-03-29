########################
# Crude Oil Production #
########################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

#### Load data ####
crude_oil_production_annual <- read_csv("data/crude-oil-production.csv")

#### Wrangle ####
# Column names to lower case
crude_oil_production_annual <- crude_oil_production_annual %>%
  clean_names()

#### Visualize ####
# Plot Crude oil production
selected_locations <- c("EU28", "G20", "BRN", "CHN", "NOR", "RUS", "SAU", "USA", "VEN")

crude_oil_production_annual %>% 
  filter(location %in% selected_locations) %>%
  mutate(value = value / 1000) %>% 
  ggplot(aes(x = time,
             y = value,
             colour = location)) +
  geom_line() +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(1960, 2020, 5),
                     limits = c(1970, 2017)) +
  scale_y_log10(labels = label_number(big.mark = "",
                                      accuracy = 0.1)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  labs(x = "",
       y = "",
       colour = "",
       title = "Crude Oil Production (Total Annual)",
       subtitle = "million toe (y-axis log scale)",
       caption = "Data: OECD (2022), Crude oil production (indicator). doi: 10.1787/4747b431-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(legend.position = "right", 
        legend.background = element_blank())
  
#### Save image ####
ggsave("figures/crude-oil-production.png", width = 7, height = 7)