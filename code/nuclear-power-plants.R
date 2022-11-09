#### Setup ####
library(tidyverse)
library(janitor)
library(scales)

#### Load data ####
nuclear_plants <- read_csv("data/nuclear-plants.csv")

#### Column names to lower case ####
nuclear_plants <- nuclear_plants %>% 
  clean_names()

#### Visualize ####
nuclear_plants %>% 
  filter(location != "OECD" & location != "OECDE") %>% 
  mutate(location = fct_reorder(location, value)) %>% 
  ggplot(aes(x = value, y = location)) +
  geom_col() +
  scale_x_continuous(expand = c(0.01, 0),
                     limits = c(0, 100),
                     breaks = seq(0, 100, 10)) +
  theme_bw() +
  labs(x = "", y = "",
       title = "Number of Nuclear Power Plants",
       caption = "Data: OECD (2022), Nuclear power plants (indicator). doi: 10.1787/3cc1191d-en | Graphic: @weiyuet")

#### Save image ####
ggsave("figures/nuclear-power-plants.png", width = 6, height = 6)