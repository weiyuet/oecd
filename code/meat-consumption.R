####################
# Meat Consumption #
####################

#### Setup ####
library(tidyverse)
library(janitor)
library(scales)
library(paletteer)

#### Load data ####
meat_consumption <- read_csv("data/meat-consumption.csv")

#### Column names to lower case ####
meat_consumption <- meat_consumption %>% 
  clean_names()

#### Plot meat consumption WLD ####
selected_locations <- c("WLD")

meat_consumption %>% 
  filter(measure == "KG_CAP") %>% 
  filter(location %in% selected_locations) %>% 
  ggplot(aes(x = time,
             y = value,
             colour = subject)) +
  geom_line() +
  scale_x_continuous(limits = c(1990, 2022),
                     breaks = seq(1990, 2022, 5)) +
  scale_y_continuous(breaks = seq(0, 16, 2),
                     labels = label_number(accuracy = 0.1)) +
  scale_colour_paletteer_d("ggsci::default_jco") +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.85),
        legend.background = element_blank()) +
  labs(x ="",
       y = "",
       title = "Global Meat Consumption",
       subtitle = "y-axis: kg per capita",
       caption = "Data: OECD (2022), Meat consumption (indicator). doi: 10.1787/fa290fd0-en | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/meat-consumption.png", width = 7, height = 5)