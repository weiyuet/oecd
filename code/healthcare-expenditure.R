##########################
# Healthcare Expenditure #
##########################

#### Setup ####
library(tidyverse)
library(janitor)
library(skimr)
library(scales)

#### Load Data ####
healthcare_expenditure <- read_csv("data/healthcare-expenditure.csv")

#### Wrangle ####
# Column names to lower case
healthcare_expenditure <- healthcare_expenditure %>% 
  clean_names()

#### Explore Data ####
healthcare_expenditure %>% 
  skim() %>% 
  summary() # 17777 rows of data, 6 character, 2 numeric

range(healthcare_expenditure$time) # Data from 1970 to 2021

healthcare_expenditure %>% 
  count(frequency,
        sort = TRUE) # Only annual data

healthcare_expenditure %>% 
  count(measure,
        sort = TRUE) # 3 different measures

healthcare_expenditure %>% 
  count(subject,
        sort = TRUE) # 4 different

healthcare_expenditure %>% 
  count(location,
        sort = TRUE) %>% 
  print(n = 53) # 53 countries in data set

#### Visualize ####
selected_locations <- c("KOR", "USA", "DEU", "AUS", "GBR", "NOR", "JPN", "DNK", "FRA", "CHN", "CAN", "IND")

selected_measures <- c("PC_GDP")

selected_subjects <- c("TOT")

healthcare_expenditure %>%
  filter(location %in% selected_locations) %>% 
  filter(measure %in% selected_measures) %>% 
  filter(subject %in% selected_subjects) %>% 
  ggplot(aes(x = time,
             y = value)) +
  facet_wrap(vars(location),
             scales = "free") +
  geom_line() +
  scale_y_continuous(labels = label_number(accuracy = 0.1,
                                           decimal.mark = ".")) +
  labs(x = "",
       y = "",
       title = "Healthcare Expenditures as a % of GDP",
       caption = "Data: OECD (2023), OECD (2023), Health spending (indicator). doi: 10.1787/8643de7e-en | Graphic: @weiyuet") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5))

#### Save Image ####
ggsave("figures/healthcare-expenditures.png",
       width = 7,
       height = 7)