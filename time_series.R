### time series
# Load required packages
library(ggplot2)
library(scales)

ALL_DATA_SUMMARY

all_poland_summary

all_poland_summary %>%
  mutate(select(., T01:T52))

# Create a sample weekly time series data
set.seed(123)
dates <- seq(as.Date("2000/1/1"), as.Date("2023/4/13"), by = "week")
values <- rnorm(length(dates), mean = 100, sd = 20)
data <- data.frame(date = dates, value = values)

# Create line plot
ggplot(data, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Year", y = "Value", title = "Weekly Time Series Data") +
  theme_minimal()

3235.5-3313

ALL_DATA %>%
  select(wiek) %>%
  distinct()

#### SZEREG CZASOWY DLA POLSKI
library(tidyr)

ALL_DATA_TS <- ALL_DATA_AGE_EUROSTAT %>%
  # filter(powiat_nazwa == 'Polska') %>%
  # filter(wiek == 'Ogółem') %>%
  # select(-powiat_numer) %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

library(viridis)

ALL_DATA_TS %>%
  filter(powiat_nazwa == 'Polska') %>%
  filter(wiek == 'Ogółem') %>%
  # filter(YEAR == 2000) %>%
  mutate(week_number = str_remove(week, 'T')) %>%
  ggplot(aes(x = week_number, y = count, group = YEAR, color = YEAR)) +
  geom_line() +
  scale_color_viridis(option = "D", direction = -1) +
  theme_ipsum()


