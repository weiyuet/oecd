##################
# Road Accidents #
##################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)

#### Load Data ####
# Road accidents data
road_accidents <- read_csv("data/road-accidents.csv")

#### Wrangle ####
# Column names to lower case
road_accidents <- road_accidents %>% 
  clean_names()

#### Visualize ####
# Are the number of road accidents increasing or decreasing?
selected_locations <- c("FIN", "FRA", "DEU", "EST", "GBR", "MEX", "USA", "JPN", "KOR", "AUS", "NZL", "CHN")
accident_type <- c("DEATH")
measure_type <- c("NBR")

road_accidents %>% 
  filter(location %in% selected_locations) %>% 
  filter(subject %in% accident_type) %>% 
  filter(measure %in% measure_type) %>% 
  ggplot(aes(x = time,
             y = value)) +
  geom_line() +
  facet_wrap(vars(location),
             scales = "free") +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(x = "",
       y = "",
       title = "Number of Road Accidents",
       subtitle = "Trending downwards for most regions in the world",
       caption = "Data: OECD (2023), Road accidents (indicator). doi: 10.1787/2fe1b899-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

#### Save Image ####
ggsave("figures/road-accidents.png", width = 7, height = 7)