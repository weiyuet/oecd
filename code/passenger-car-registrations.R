###############################
# Passenger Car Registrations #
###############################

#### Setup ####
library(tidyverse)
library(janitor)

#### Load Data ####
# Passenger car registrations data
passenger_car_registrations <- read_csv("data/passenger-car-registrations.csv")

#### Wrangle ####
# Column names to lower case
passenger_car_registrations <- passenger_car_registrations %>%
  clean_names()

# Filter out annual data
passenger_car_registrations <- passenger_car_registrations %>% 
  filter(frequency == "M")

# Separate year and month
passenger_car_registrations <- passenger_car_registrations %>% 
  separate(time,
           c("year", "month"))

# Convert year and month into numeric
passenger_car_registrations <- passenger_car_registrations %>% 
  mutate(year = as.numeric(year),
         month = as.numeric(month))

#### Explore Data ####
passenger_car_registrations %>% 
  count(location,
        sort = TRUE) %>% 
  print(n = 40)

#### Visualize ####
# Are the number of passenger car registrations increasing or decreasing?
selected_locations <- c("CAN",
                        "GBR",
                        "DEU",
                        "FIN",
                        "ITA",
                        "JPN",
                        "USA",
                        "KOR",
                        "BRA",
                        "AUS",
                        "EU27_2020",
                        "CHN")

passenger_car_registrations %>% 
  group_by(location, year) %>% 
  summarize(year_avg = mean(value, na.rm = TRUE)) %>% 
  filter(location %in% selected_locations) %>% 
  ggplot(aes(x = year,
             y = year_avg)) +
  geom_line() +
  facet_wrap(vars(location),
             scales = "free_y") +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       title = "New passenger car registrations have started to decline in some countries",
       subtitle = "Base year 2015 = 100",
       caption = "Data: OECD (2023), Passenger car registrations (indicator). doi: 10.1787/c58fcf22-en | Graphic: @weiyuet")

#### Save Image ####
ggsave("figures/passenger-car-registrations.png",
       width = 7,
       height = 7)