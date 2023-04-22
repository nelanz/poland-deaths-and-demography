# death rate, summary

death_rate_PL_yearly <- all_PL_statistics%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = roczna_ludnosc_PL$Amount) %>%
  mutate(death_rate_per_1000 = sum/number_of_people * 1000)

max_death_rate <- max(death_rate_PL_yearly$death_rate_per_1000)
max_death_rate_year <- death_rate_PL_yearly$YEAR[which(death_rate_PL_yearly$death_rate_per_1000 == max_death_rate)]

ggplot(death_rate_PL_yearly, aes(YEAR, death_rate_per_1000)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Death rate per 1000",
       title = "Death rate per 1000 for Poland",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year,
           y = max_death_rate, color = "red") +
  geom_text(label=round(max_death_rate, 1), x = max_death_rate_year-0.85,
            y = max_death_rate, colour='red') +
theme_ipsum()

# death rate, males
years
males_population <- c(18547799,18537339,18525163,18506749,18486430,	18470253,	
                      18453855,	18426775,	18411501,	18414926,	18412237,	18429700,
                      18427296,	18426093,	18403964,	18397163,	18377040,	18377837,
                      18380299,	18380376,	18373381,	18307488)
all_population_males <- tibble(Year = years, Amount = males_population)

max_death_rate_males <- max(death_rate_PL_yearly_males$death_rate_per_1000)
max_death_rate_year_males <- death_rate_PL_yearly_males$YEAR[which(death_rate_PL_yearly_males$death_rate_per_1000 == max_death_rate_males)]

death_rate_PL_yearly_males <- all_PL_statistics_male%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = all_population_males$Amount) %>%
  mutate(death_rate_per_1000 = sum/number_of_people * 1000)

ggplot(death_rate_PL_yearly_males, aes(YEAR, death_rate_per_1000)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Death rate per 1000",
       title = "Death rate per 1000 for Poland, males",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year_males,
           y = max_death_rate_males, color = "red") +
  geom_text(label=round(max_death_rate_males, 1), x = max_death_rate_year_males-0.85,
            y = max_death_rate_males, colour='red') +
  theme_ipsum()

# death rate, females
females_population <- c(19715504,	19716616,	19717034,	19711782,	19704178,19703582,
                        19703200,	19698704,	19704140,	19720950,	19610632,	19633018,
                        19636496,	19636442,	19613892,	19608451,	19590169,	19595127,
                        19596388,	19592436,	19584757,	19532513)

all_population_females <- tibble(Year=years, Amount=females_population)

max_death_rate_females <- max(death_rate_PL_yearly_females$death_rate_per_1000)
max_death_rate_year_females <- death_rate_PL_yearly_females$YEAR[which(death_rate_PL_yearly_females$death_rate_per_1000 == max_death_rate_females)]

death_rate_PL_yearly_females <- all_PL_statistics_female%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = all_population_females$Amount) %>%
  mutate(death_rate_per_1000 = sum/number_of_people * 1000)

ggplot(death_rate_PL_yearly_females, aes(YEAR, death_rate_per_1000)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Death rate per 1000",
       title = "Death rate per 1000 for Poland, females",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year_females,
           y = max_death_rate_females, color = "red") +
  geom_text(label=round(max_death_rate_females, 1), x = max_death_rate_year_females-0.85,
            y = max_death_rate_females, colour='red') +
  theme_ipsum()

###########################
## DEATH RATE IN AGE GROUPS
library(stringr)
library(eurostat)

# tmp <- get_eurostat_toc()
# head(tmp)
# dataset_code <- "DEMO_PJANGROUP"
# filters <- list(time = years, geo = "PL")
# 
# population_data <- get_eurostat(dataset_code, filters = filters, update_cache=TRUE)

population_data %>%
  select(age) %>%
  distinct() %>%
  print(n=23)

wiek_eurostat_v <-  all_poviats_stats %>% select(wiek_eurostat) %>% distinct()

# populacja w kazdej grupie wiekowej
population_data_summary <- population_data %>%
  filter(sex == 'T') %>%
  filter(!age %in% c('Y_GE75', 'Y_GE80', 'UNK')) %>%
  mutate(YEAR = format(time, "%Y")) %>%
  mutate(wiek_eurostat = age) %>%
  select(sex, wiek_eurostat, values, YEAR)%>%
  mutate(YEAR = as.numeric(YEAR))

poland_age_groups_summarized <-  all_poviats_stats %>%
  filter(powiat_nazwa == 'Polska') %>%
  group_by(YEAR, wiek_eurostat) %>%
  summarize(age_group_sum = sum(sum)) 

age_groups_PL_death_rate_summary <- inner_join(population_data_summary,
           poland_age_groups_summarized,
           by = c('wiek_eurostat', 'YEAR')) %>%
  mutate(death_rate_per_1000 = age_group_sum/values*1000)

ggplot(age_groups_PL_death_rate_summary, aes(YEAR, death_rate_per_1000, color=wiek_eurostat)) +
  geom_line() +
  theme_ipsum()

all_poviats_stats %>%
  filter(powiat_nazwa == 'Polska') %>%
  group_by(YEAR, wiek_eurostat) %>%
  filter(wiek_eurostat == 'Y_GE85')


age_groups_PL_death_rate_summary %>%
  filter(wiek_eurostat == 'Y_GE85') %>%
  print(n=22)
age_groups_PL_death_rate_summary %>%
  arrange(wiek_eurostat)



