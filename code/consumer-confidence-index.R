#############################
# Consumer Confidence Index #
#############################

#### Setup ####
library(tidyverse)
library(janitor)

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

#### Explore ####
consumer_confidence_index %>% 
  count(location,
        sort = TRUE) # 49 locations

#### Plot CCI ####
# Select locations for plot
selected_locations <- c("USA")

consumer_confidence_index %>% 
  filter(location %in% selected_locations) %>% 
  ggplot(aes(x = month,
             y = value)) +
  geom_point(size = 0.5) +
  geom_line() +
  geom_hline(yintercept = 100,
             colour = "red",
             linetype = "dashed") +
  facet_wrap(vars(year)) +
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb[1:12]) +
  labs(x = "",
       y = "",
       title = "Consumer Confidence Index (CCI) in the USA",
       subtitle = "Above 100 indicates an optimistic attitude; below 100 indicates a pessimistic attitude",
       caption = "Data: OECD (2023), Consumer confidence index (CCI) (indicator). doi: 10.1787/46434d78-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size = 7))

#### Save image ####
ggsave("figures/consumer-confidence-index.png",
       width = 8.5,
       height = 7)