# Setup
library(tidyverse)
library(janitor)
library(scales)
library(ggsci)

# Load data 
crude_oil_production_annual <- read_csv("data/crude_oil_production_annual.csv")

# Column names to lower case
crude_oil_production_annual <- crude_oil_production_annual %>%
  clean_names()

# Plot Crude oil production in Russia, compared to Ukraine, Saudi Arabia, Norway, the USA, China, and the EU
selected_countries <- c("EU28", "G20", "BRN", "CHN", "NOR", "RUS", "SAU", "USA", "VEN")

crude_oil_production_annual %>% 
  filter(location %in% selected_countries) %>%
  ggplot(aes(x = time, y = value, colour = location)) +
  geom_line() +
  geom_point(size = 0.7) +
  scale_x_continuous(breaks = seq(1970, 2020, 5),
                     limits = c(1970, 2020)) +
  scale_y_log10(labels = label_number(suffix = " ktoe", big.mark = ","),
                limits = c(100, 1000000)) +
  scale_colour_jco() +
  theme_bw() +
  theme(legend.position = c(0.5, 0.1), 
        legend.background = element_blank(),
        axis.text.y = element_text(angle = 90)) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "", y = "",
       colour = "",
       title = "Crude Oil Production (Total Annual)",
       subtitle = "Measured in thousand tonne of oil equivalent (ktoe)",
       caption = "Data: OECD (2022), Crude oil production (indicator). doi: 10.1787/4747b431-en | Graphic: @weiyuet")
  
# Save image
ggsave("figures/crude-oil-production.png", width = 8, height = 6)