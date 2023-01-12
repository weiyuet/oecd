#############################
# Consumer Confidence Index #
#############################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

#### Load data ####
consumer_confidence_index <- read_csv("data/consumer-confidence-index.csv")

#### Wrangle ####
# Column names to lower case
consumer_confidence_index <- consumer_confidence_index %>% 
  clean_names()

# Separate month and year
consumer_confidence_index <- consumer_confidence_index %>% 
  separate(time, c("year", "month"))

# Convert month and year into numeric
consumer_confidence_index <- consumer_confidence_index %>% 
  mutate(across(6:7, as.numeric))

#### Plot CCI ####
# Select locations for plot
selected_locations <- c("USA")

consumer_confidence_index %>% 
  filter(location %in% selected_locations) %>% 
  ggplot(aes(x = month,
             y = value)) +
  geom_point(size = 0.5) +
  geom_line() +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = seq(1, 12, 2),
                     labels = month.abb[seq(1, 12, 2)]) +
  labs(x = "",
       y = "",
       title = "Consumer Confidence Index (CCI) in the USA",
       caption = "Data: OECD (2023), Consumer confidence index (CCI) (indicator). doi: 10.1787/46434d78-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

#### Save image ####
ggsave("figures/consumer-confidence-index.png", width = 7, height = 7)