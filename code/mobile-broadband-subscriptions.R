##################################
# Mobile Broadband Subscriptions #
##################################

#### Setup ####
library(tidyverse)
library(janitor)
library(skimr)
library(scales)

#### Load Data ####
mobile_broadband_subscriptions <- read_csv("data/mobile-broadband-subscriptions.csv")

#### Wrangle ####
# Column names to lower case
mobile_broadband_subscriptions <- mobile_broadband_subscriptions %>% 
  clean_names()

#### Explore Data ####
mobile_broadband_subscriptions %>% 
  count(location, sort = TRUE) # 39 countries

range(mobile_broadband_subscriptions$time) # 2010 to 2022

mobile_broadband_subscriptions %>% 
  skim() %>% 
  summary() # 503 rows, 8 columns

#### Visualize ####
# What is the trend for mobile broadband subscriptions?
selected_locations <- c("OECD",
                        "AUS",
                        "EST",
                        "JPN",
                        "KOR",
                        "USA",
                        "MEX")

mobile_broadband_subscriptions %>% 
  filter(location %in% selected_locations) %>% 
  ggplot(aes(x = time,
             y = value,
             colour = location)) +
  geom_line() +
  scale_x_continuous(labels = label_number(big.mark = "",
                                           accuracy = 1)) +
  scale_colour_brewer() +
  labs(x = NULL,
       y = NULL,
       colour = NULL,
       title = "Mobile Broadband Subscriptions per 100 Inhabitants",
       caption = "Data: OECD (2023), Mobile broadband subscriptions (indicator). doi: 10.1787/1277ddc6-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(legend.position = c(0.2,
                            0.8))

#### Save Image ####
ggsave("figures/mobile-broadband-subscriptions.png",
       width = 8,
       height = 6)