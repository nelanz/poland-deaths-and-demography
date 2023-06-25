### time series
# Load required packages
library(ggplot2)
library(scales)
library(tidyr)
library(viridis)
library(dplyr)


#### SZEREG CZASOWY DLA POLSKI

ALL_DATA_TS <- ALL_DATA_AGE_EUROSTAT %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

ALL_DATA_TS %>%
  filter((YEAR == 2010))


ALL_DATA_TS_FEMALE <- ALL_DATA_AGE_EUROSTAT_FEMALE %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

write.csv(ALL_DATA_TS, 'ALL_DATA_TS.csv', row.names = F)
ts_test = read.csv('ALL_DATA_TS.csv')
ts_test%>%
  select(YEAR) %>%
  distinct()

write.csv(ALL_DATA_TS_FEMALE, 'ALL_DATA_TS_FEMALE.csv', row.names = F)

ALL_DATA_TS_MALE <- ALL_DATA_AGE_EUROSTAT_MALE %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

write.csv(ALL_DATA_TS_MALE, 'ALL_DATA_TS_MALE.csv', row.names = F)
dates <- seq(as.Date("2000/1/03"), as.Date("2021/12/31"), by = "week")

summary_ts_PL <- ALL_DATA_TS %>%
  filter(powiat_nazwa == 'Polska') %>%
  filter(wiek == 'Ogółem') %>%
  # filter(YEAR == 2000) %>%
  mutate(week_number = str_remove(week, 'T')) %>%
  mutate(date = dates) 

# weekly time series
  # ggplot(subset(summary_ts_PL, !(YEAR %in% c(2020, 2021))), aes(x = week_number, y = count, group = YEAR, color = YEAR)) +
  # geom_line() +
  # geom_line(data = subset(summary_ts_PL, YEAR == 2020), 
  #           size = 1, linetype = 'dashed', color = 'red') +
  # geom_line(data = subset(summary_ts_PL, YEAR == 2021), 
  #             size = 1, linetype = 'dotted', color = 'orange') +
  # scale_color_viridis(option = "D", direction = -1) +
  # labs(title = "Weekly deaths in Poland", subtitle = "Years 2000-2021") + 
  # theme_ipsum()

  length(dates)
  
  break.vec <- c(min(dates), dates, max(dates))

  summary_ts_PL %>%
    ggplot(aes(x = date, y = count)) +
    geom_line(color = "#005377") +
    scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y") +
    labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland", subtitle = 'Years 2000-2021') +
    geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
    ylim(c(3000,17000)) + 
    theme_ipsum()
  
  
  summary_ts_PL %>%
    filter(YEAR == 2021)
  
  model <- lm(count ~ as.numeric(date), data=summary_ts_PL)
  summary(model)
  
  
### Time series for females
  
get_summary_ts <- function(all_data_ts, wiek_eurostat_name) {
  summary_ts <- all_data_ts %>%
    filter(powiat_nazwa == 'Polska') %>%
    filter(wiek_eurostat == wiek_eurostat_name) %>%
    mutate(week_number = str_remove(week, 'T')) %>%
    mutate(date = dates) 
  
  return(summary_ts)
}

get_summary_ts(ALL_DATA_TS_FEMALE, 'TOTAL') %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#005377") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y") +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland, females", subtitle = 'Years 2000-2021') +
  geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  ylim(c(2000,10000)) +
  theme_ipsum()

## time series for males
get_summary_ts(ALL_DATA_TS_MALE, 'TOTAL') %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#005377") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y") +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland, males", subtitle = 'Years 2000-2021') +
  geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  ylim(c(2000,10000)) +
  theme_ipsum()

# time series for age groups

ALL_DATA_TS %>%
  filter(powiat_nazwa == 'Polska') %>%  
  filter(wiek_eurostat != 'TOTAL') %>%
  group_by(wiek_eurostat) %>%
  mutate(date = ifelse(wiek_eurostat == 'Y_GE85', rep(dates, 2), dates)) %>%
  mutate(date = as.Date.numeric(date, origin = "1970-01-01")) %>%
  ggplot(aes(x = date, y = count)) +
  geom_line(color = "#005377") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", date_labels = "%Y") +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland", subtitle = 'Years 2000-2021') +
  geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  # ylim(c(2000,10000)) +
  theme_ipsum() +
  facet_wrap(~wiek_eurostat, ncol = 4)

###### SEASONAL PLOT

# ggplot(subset(summary_ts_PL, !(YEAR %in% c(2020, 2021))), aes(x = date, y = count, group = YEAR, color = YEAR)) +
# geom_smooth() +
# geom_line(data = subset(summary_ts_PL, YEAR == 2020),
#           size = 1, linetype = 'dashed', color = 'red') +
# geom_line(data = subset(summary_ts_PL, YEAR == 2021),
#             size = 1, linetype = 'dotted', color = 'orange') +
#   
# scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
# scale_color_viridis(option = "D", direction = -1) +
# labs(title = "Weekly deaths in Poland", subtitle = "Years 2000-2021") +
# theme_ipsum()

summary_ts_PL_single_year <- summary_ts_PL %>% 
  mutate(
    date = update(date, year = 1)  # use a constant year for the x-axis
  ) 


  ggplot(subset(summary_ts_PL_single_year, !(YEAR %in% c(2020, 2021))), aes(x = date, y = count, color = as.factor(YEAR))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  geom_smooth(se=F) + 
  geom_smooth(se = F, data = subset(summary_ts_PL_single_year, YEAR == 2020), color = 'red', linetype = 'dashed')+
  geom_smooth(se = F, data = subset(summary_ts_PL_single_year, YEAR == 2021), color = 'red')+
  scale_color_grey(start = 0.8, end = 0.2)+
  labs(title = "Seasonality of deaths in Poland", subtitle = "Years 2000-2021", x = "Month", y = "Death count") +
  theme_ipsum() +
    theme(legend.position = "none") 

library(xts)

summary_xts_PL <-  xts(x = summary_ts_PL$count, order.by = summary_ts_PL$date, frequency = 52)
acf(summary_xts_PL, lag.max = 104, plot = T, main = "30 days")

summary_xts_PL['2000']

plot(summary_xts_PL)

ggplot(as.data.frame(summary_xts_PL), aes(x = index(summary_xts_PL), y = summary_xts_PL)) +
  geom_line()
