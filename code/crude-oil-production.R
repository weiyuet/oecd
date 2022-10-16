# Setup
library(tidyverse)
library(scales)
library(NatParksPalettes)

# Load data 
crude_oil_production_annual <- read_csv("data/crude_oil_production_annual.csv")

# Count distinct number of countries/areas in data set
crude_oil_production_annual %>%
  distinct(crude_oil_production_annual$LOCATION) %>% count()

# Plot Crude oil production in Russia, compared to Ukraine, Saudi Arabia, Norway, the USA, and the EU
selected_countries <- c("RUS", "UKR", "SAU", "NOR", "USA", "EU28")

crude_oil_production_annual %>% 
  filter(LOCATION %in% selected_countries) %>%
  ggplot(aes(x = TIME, y = Value, colour = LOCATION)) +
  geom_line() +
  geom_point(size = 0.7) +
  scale_x_continuous(breaks = seq(1970, 2020, 5),
                     limits = c(1970, 2020),
                     expand = c(0.01, 0)) +
  scale_y_log10(labels = label_number(suffix = " toe", big.mark = ","),
                limits = c(1000, 1000000),
                expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.35), 
        legend.title = element_blank(),
        axis.text.y = element_text(angle = 90)) +
  scale_colour_manual(values = natparks.pals("Yellowstone", 6)) +
  labs(x = "", y = "",
       title = "Crude Oil Production (Total Annual)",
       subtitle = "Measured in thousand tonne of oil equivalent (toe) | y-axis log scale",
       caption = "Data: OECD (2022), Crude oil production (indicator). doi: 10.1787/4747b431-en | Graphic: @weiyuet")
  
# Save image
ggsave("figures/russia-crude-oil-production.png", width = 8, height = 6)