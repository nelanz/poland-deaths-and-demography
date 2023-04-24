library(hrbrthemes)
library(RColorBrewer)

# Tabelki statystyczne dla całej Polski, summary.
# customowa funkcja do mody
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# statystyki: suma, srednia, mediana, moda, max, min, sd, kwantyle .25 i .75
get_primary_stats <- function(summary_df, result_file_name) {
  all_primary_statistics <- summary_df %>%
    mutate(sum = select(., T01:T53) %>% apply(1, sum, na.rm=TRUE)) %>%
    mutate(mean = select(., T01:T53) %>% apply(1, mean, na.rm=TRUE)) %>%
    mutate(mean = round(mean, 1)) %>%
    mutate(median = select(., T01:T53) %>% apply(1, median, na.rm=TRUE)) %>%
    mutate(mode = select(., T01:T53) %>% apply(1, getmode)) %>%
    mutate(max = select(., T01:T53) %>% apply(1, max, na.rm=TRUE)) %>%
    mutate(min = select(., T01:T53) %>% apply(1, min, na.rm=TRUE)) %>%
    # mutate(var = select(., T01:T53) %>% apply(1, var, na.rm=TRUE)) %>%
    mutate(sd = select(., T01:T53) %>% apply(1, sd, na.rm=TRUE)) %>%
    mutate(sd = round(sd, 1)) %>%
    mutate(q.05 = select(., T01:T53) %>% apply(1, quantile, probs = .05, na.rm=TRUE)) %>%
    mutate(q.25 = select(., T01:T53) %>% apply(1, quantile, probs = .25, na.rm=TRUE)) %>%
    mutate(q.75 = select(., T01:T53) %>% apply(1, quantile, probs = .75, na.rm=TRUE)) %>%
    mutate(q.95 = select(., T01:T53) %>% apply(1, quantile, probs = .95, na.rm=TRUE)) %>%
    select(YEAR, sum, mean, min, q.05, q.25, median, q.75, q.95, max, mode, sd, T01, T52, T53)
  
  write.csv(all_primary_statistics, result_file_name)
  
  return(all_primary_statistics)
}
all_PL_statistics <- get_primary_stats(all_poland_summary, "PL_summary_stats.csv")
all_PL_statistics

all_PL_statistics_female <- get_primary_stats(all_poland_female, "PL_summary_stats_female.csv")
all_PL_statistics_female

all_PL_statistics_male <- get_primary_stats(all_poland_male, "PL_summary_stats_male.csv")
all_PL_statistics_male

ggplot(all_PL_statistics, aes(YEAR, sum)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Sum",
       title = "Total number of all-cause deaths in Poland",
       subtitle = "Years 2000 - 2021") +
  theme_ipsum()

mean_female <- all_PL_statistics_female %>%
  select('YEAR', 'mean') %>%
  mutate(gender = 'female')

mean_male <- all_PL_statistics_male %>%
  select('YEAR', 'mean') %>%
  mutate(gender = 'male')

bind_rows(mean_female, mean_male) %>%
  ggplot(aes(x=YEAR, y=mean, color=gender)) +
  geom_line() +
  labs(x="Year",
       y="Mean",
       title = "Mean number of all-cause deaths in Poland",
       subtitle = "Years 2000 - 2021, by gender",
       color=NULL) +
  theme_ipsum() +
  theme(legend.position = "top") 

sum_female <- all_PL_statistics_female %>%
  select('YEAR', 'sum') %>%
  mutate(gender = 'female')

sum_male <- all_PL_statistics_male %>%
  select('YEAR', 'sum') %>%
  mutate(gender = 'male')

bind_rows(sum_female, sum_male) %>%
  ggplot(aes(x=YEAR, y=sum, color=gender)) +
  geom_line() +
  labs(x="Year",
       y="Sum",
       title = "Total number of all-cause deaths in Poland",
       subtitle = "Years 2000 - 2021, by gender",
       color=NULL) +
  theme_ipsum() +
  theme(legend.position = "top") 
# data %>%
#   gather(var, val, 2:ncol(data)) %>%
#   spread(Series.Description, val)


#### calcuklations on poviats
all_poviats_stats <-
  ALL_DATA_AGE_EUROSTAT %>%
  mutate(sum = select(., T01:T53) %>% apply(1, sum, na.rm=TRUE)) %>%
  select(-starts_with('T'))

all_poviats_stats_females <- ALL_DATA_AGE_EUROSTAT_FEMALE %>%
  mutate(sum = select(., T01:T53) %>% apply(1, sum, na.rm=TRUE)) %>%
  select(-starts_with('T'))

all_poviats_stats_males <- ALL_DATA_AGE_EUROSTAT_MALE %>%
  mutate(sum = select(., T01:T53) %>% apply(1, sum, na.rm=TRUE)) %>%
  select(-starts_with('T'))
