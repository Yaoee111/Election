#### Preamble ####
# Purpose: Models the analysis data to get predictions for Donald Trump.
# Author: Yiyi Yao
# Date: 02 November 2024
# Contact: ee.yao@mail.utoronto.ca
# License: MIT
# Pre-requisites: data/03-analysis_data/trump.parquet


#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(rstanarm)

#### Read data ####
trump_data <- read_parquet("data/03-analysis_data/trump.parquet") %>%
  mutate(end_date = as.Date(end_date, format="%Y-%m-%d"))

### Model data ####
# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = trump_data)

# Model 2: pct as a function of end_date and pollster
model_date_pollster <- lm(pct ~ end_date + pollster, data = trump_data)

# Augment data with model predictions
trump_data <- trump_data %>%
  mutate(
    fitted_date = predict(model_date),
    fitted_date_pollster = predict(model_date_pollster)
  )

### Plot model predictions ###

# Plot for Model 1: Trump's Support Percentage Over Time with Confidence Intervals
#| echo: false
#| warning: false
#| message: false
#| label: fig-trump-model1-improved
#| fig-cap: Linear Model of Trump’s Support Over Time with Confidence Interval

ggplot(trump_data, aes(x = end_date, y = pct)) +
  geom_point(color = "darkgray", size = 2, alpha = 0.6) +
  geom_smooth(aes(y = fitted_date), method = "lm", color = "blue", se = TRUE, linetype = "solid") +
  labs(title = "Trump's Support Percentage Over Time",
       x = "Date",
       y = "Support Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed")
  )

# Plot for Model 2: Trump’s Support Percentage Over Time by Pollster
#| echo: false
#| warning: false
#| message: false
#| label: fig-trump-model2-improved
#| fig-cap: Linear Model of Trump’s Support Over Time by Pollster with Confidence Intervals

ggplot(trump_data, aes(x = end_date, y = pct)) +
  geom_point(color = "darkgray", size = 2, alpha = 0.6) +
  geom_smooth(aes(y = fitted_date_pollster), method = "lm", color = "blue", se = TRUE, linetype = "solid") +
  labs(title = "Trump's Support Percentage Over Time by Pollster",
       x = "Date",
       y = "Support Percentage (%)") +
  theme_minimal(base_size = 14) +
  facet_wrap(vars(pollster)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "lightgray", linetype = "dashed"),
    strip.text = element_text(face = "bold")
  )


# Save models if needed
saveRDS(model_date, file = "models/model_date_trump.rds")
saveRDS(model_date_pollster, file = "models/model_date_pollster_trump.rds")
