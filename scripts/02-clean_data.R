#### Preamble ####
# Purpose: Cleans the raw data to filter data for Donald Trump specifically.
# Author: Yiyi Yao
# Date: 01 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT
# Pre-requisites: data/02-raw_data/president_polls.csv


#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(arrow)
library(janitor)

#### Clean data ####
### Prepare dataset ###
# Read in the data and clean variable names
raw_data <- read_csv("data/02-raw_data/president_polls.csv") %>%
  clean_names()

# Filter data to Trump estimates based on high-quality polls after he declared
just_trump_high_quality <- raw_data %>%
  filter(
    candidate_name == "Donald Trump",
    numeric_grade >= 3 # Use high-quality polls only
  ) %>%
  mutate(
    state = if_else(is.na(state), "National", state), # Fix for national polls
    end_date = mdy(end_date),
    end_date = if_else(end_date < as.Date("2024-07-21"), as.Date("2024-07-21"), end_date) # When Trump declared
  ) %>%
  mutate(
    num_trump = round((pct / 100) * sample_size, 0) # Calculate Trump support numbers
  )


#### Plot data ####
# Base plot
base_plot <- ggplot(just_trump_high_quality, aes(x = end_date, y = pct)) +
  theme_classic() +
  labs(y = "Trump Percent", x = "Date")

# Plot poll estimates and overall smoothing
base_plot +
  geom_point() +
  geom_smooth()

# Color by pollster
base_plot +
  geom_point(aes(color = pollster)) +
  geom_smooth() +
  theme(legend.position = "bottom")

# Facet by pollster
base_plot +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(pollster))

# Color by pollscore
base_plot +
  geom_point(aes(color = factor(pollscore))) +
  geom_smooth() +
  theme(legend.position = "bottom")


#### Save data ####
write_parquet(just_trump_high_quality, "data/03-analysis_data/trump.parquet")
