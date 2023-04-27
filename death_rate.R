# death rate, summary
# n in d/p * 10^n for crude death rate
factor = 100000

death_rate_PL_yearly <- all_PL_statistics%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = roczna_ludnosc_PL$Amount) %>%
  mutate(death_rate = sum/number_of_people * factor)

max_death_rate <- max(death_rate_PL_yearly$death_rate)
max_death_rate_year <- death_rate_PL_yearly$YEAR[which(death_rate_PL_yearly$death_rate == max_death_rate)]

ggplot(death_rate_PL_yearly, aes(YEAR, death_rate)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Crude mortality rate per 100 000",
       title = "Crude mortality rate per 100 000 for Poland",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year,
           y = max_death_rate, color = "red") +
  geom_text(label=round(max_death_rate, 1), x = max_death_rate_year-1.2,
            y = max_death_rate, colour='red') +
theme_ipsum()

# death rate, males
years
males_population <- c(18547799,18537339,18525163,18506749,18486430,	18470253,	
                      18453855,	18426775,	18411501,	18414926,	18412237,	18429700,
                      18427296,	18426093,	18403964,	18397163,	18377040,	18377837,
                      18380299,	18380376,	18373381,	18307488)
all_population_males <- tibble(Year = years, Amount = males_population)

death_rate_PL_yearly_males <- all_PL_statistics_male%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = all_population_males$Amount) %>%
  mutate(death_rate = sum/number_of_people * factor)

max_death_rate_males <- max(death_rate_PL_yearly_males$death_rate)
max_death_rate_year_males <- death_rate_PL_yearly_males$YEAR[which(death_rate_PL_yearly_males$death_rate == max_death_rate_males)]

ggplot(death_rate_PL_yearly_males, aes(YEAR, death_rate)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Crude mortality rate per 100 000",
       title = "Crude mortality rate per 100 000 for Poland, males",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year_males,
           y = max_death_rate_males, color = "red") +
  geom_text(label=round(max_death_rate_males, 1), x = max_death_rate_year_males-1.2,
            y = max_death_rate_males, colour='red') +
  theme_ipsum()

# death rate, females
females_population <- c(19715504,	19716616,	19717034,	19711782,	19704178,19703582,
                        19703200,	19698704,	19704140,	19720950,	19610632,	19633018,
                        19636496,	19636442,	19613892,	19608451,	19590169,	19595127,
                        19596388,	19592436,	19584757,	19532513)

all_population_females <- tibble(Year=years, Amount=females_population)

death_rate_PL_yearly_females <- all_PL_statistics_female%>%
  select(YEAR, sum) %>%
  mutate(number_of_people = all_population_females$Amount) %>%
  mutate(death_rate = sum/number_of_people * factor)

max_death_rate_females <- max(death_rate_PL_yearly_females$death_rate)
max_death_rate_year_females <- death_rate_PL_yearly_females$YEAR[which(death_rate_PL_yearly_females$death_rate == max_death_rate_females)]

ggplot(death_rate_PL_yearly_females, aes(YEAR, death_rate)) + 
  geom_line(color="red") +
  labs(x="Year",
       y="Crude mortality rate per 100 000",
       title = "Crude mortality rate per 100 000 for Poland, females",
       subtitle = "Years 2000 - 2021") +
  annotate("point", 
           x = max_death_rate_year_females,
           y = max_death_rate_females, color = "red") +
  geom_text(label=round(max_death_rate_females, 1), x = max_death_rate_year_females-1.2,
            y = max_death_rate_females, colour='red') +
  theme_ipsum()


### wykres polaczony

death_rate_PL_yearly_males <- death_rate_PL_yearly_males %>%
  mutate(sex = 'Males')

death_rate_PL_yearly_females <- death_rate_PL_yearly_females %>%
  mutate(sex = 'Females')

death_rate_PL_combined <- rbind(death_rate_PL_yearly_males, death_rate_PL_yearly_females)

ggplot(death_rate_PL_combined, aes(YEAR, death_rate, color = sex)) + 
  geom_line() +
  labs(x="Year",
       y="Crude mortality rate per 100 000",
       title = "Crude mortality rate per 100 000 for Poland, by gender",
       subtitle = "Years 2000 - 2021",
       color = "") +
  theme_ipsum() +
  theme(legend.position = "top")

death_rate_PL_combined %>%
  mutate()

# Fit a logistic regression model
model <- glm(death_rate ~ sex, data = death_rate_PL_combined, family = binomial(link = logit))

# Print the model summary
summary(model)

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
  filter(!age %in% c('Y_GE75', 'Y_GE80', 'UNK', 'TOTAL')) %>%
  mutate(YEAR = format(time, "%Y")) %>%
  mutate(wiek_eurostat = age) %>%
  select(sex, wiek_eurostat, values, YEAR)%>%
  mutate(YEAR = as.numeric(YEAR))

poland_age_groups_summarized <-  all_poviats_stats %>%
  filter(powiat_nazwa == 'Polska') %>%
  group_by(YEAR, wiek_eurostat) %>%
  summarize(age_group_sum = sum(sum)) 

population_deaths_joined <- inner_join(population_data_summary,
                                       poland_age_groups_summarized,
                                       by = c('wiek_eurostat', 'YEAR')) %>%
  mutate(age_for_plotting = str_replace_all(wiek_eurostat, 'Y', '')) %>%
  mutate(age_for_plotting = ifelse(age_for_plotting == '_LT5', '0-4', 
                                   ifelse(age_for_plotting == '_GE85', '85+', age_for_plotting))) 


get_population_and_deaths_joined <- function(sex_value, poviats_data) {
  population_data_summary <- population_data %>%
    filter(sex == sex_value) %>%
    filter(!age %in% c('Y_GE75', 'Y_GE80', 'UNK', 'TOTAL')) %>%
    mutate(YEAR = format(time, "%Y")) %>%
    mutate(wiek_eurostat = age) %>%
    select(sex, wiek_eurostat, values, YEAR)%>%
    mutate(YEAR = as.numeric(YEAR))
  
  poland_age_groups_summarized <-  poviats_data %>%
    filter(powiat_nazwa == 'Polska') %>%
    group_by(YEAR, wiek_eurostat) %>%
    summarize(age_group_sum = sum(sum)) 
  
  population_deaths_joined <- inner_join(population_data_summary,
                                         poland_age_groups_summarized,
                                         by = c('wiek_eurostat', 'YEAR')) %>%
    mutate(age_for_plotting = str_replace_all(wiek_eurostat, 'Y', '')) %>%
    mutate(age_for_plotting = ifelse(age_for_plotting == '_LT5', '0-4', 
                                     ifelse(age_for_plotting == '_GE85', '85+', age_for_plotting))) 
  return(population_deaths_joined)
}

population_deaths_joined %>%
  mutate(death_rate_per_1000 = age_group_sum/values*1000) %>%
  mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', '2020-2021'))%>%
  group_by(wiek_eurostat, is_pre_2020) %>%
  summarize(death_rate_per_1000_mean = mean(death_rate_per_1000), death_rate_per_1000_median = median(death_rate_per_1000)) 

ggplot(age_groups_PL_death_rate_summary, aes(YEAR, death_rate_per_1000, color=wiek_eurostat)) +
  geom_line() +
  theme_ipsum()

### pre-2020 and post-2020 death rate comparison

age_groups_comparison_summary <- population_deaths_joined %>%
  mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', '2020-2021')) %>%
  group_by(age_for_plotting, is_pre_2020) %>%
  summarize(death_rate = sum(age_group_sum)/sum(values) * factor) 

## this is not the right way to calculate this according to CDC
# population_deaths_joined %>%
#   mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', '2020-2021')) %>%
#   group_by(wiek_eurostat, is_pre_2020) %>%
#   summarize(death_rate_per_1000 = mean(age_group_sum)/mean(values) * 1000) 

ggplot(age_groups_comparison_summary, aes(reorder(age_for_plotting, -death_rate), death_rate, fill=(is_pre_2020))) +
  geom_bar(stat='identity', position=position_dodge()) +
  labs(x = 'Age group', y = 'Crude mortality rate per 100 000', 
       title = 'Crude mortality rate per 100 000 for Poland, by age group', 
       subtitle = 'Comparison of years 2000-2019 and 2020-2021',
       fill = '') +
  scale_fill_manual(values = c('#005377', '#E86A92')) +
  # coord_flip() + 
  theme_ipsum() +
  theme(legend.position = "top")

get_age_groups_summary <- function(joined_data) {
  age_groups_comparison_summary <- joined_data %>%
    mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', '2020-2021')) %>%
    group_by(sex, age_for_plotting, is_pre_2020) %>%
    summarize(death_rate= sum(age_group_sum)/sum(values) * factor) 
  
  return(age_groups_comparison_summary)
}

get_age_groups_summary2 <- function(joined_data) {
  age_groups_comparison_summary <- joined_data %>%
    mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', ifelse(YEAR == 2020, '2020', '2021'))) %>%
    filter(is_pre_2020 != '2021') %>%
    group_by(sex, age_for_plotting, is_pre_2020) %>%
    summarize(death_rate= sum(age_group_sum)/sum(values) * factor) 
  
  return(age_groups_comparison_summary)
}

get_age_groups_summary3 <- function(joined_data) {
  age_groups_comparison_summary <- joined_data %>%
    mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', ifelse(YEAR == 2020, '2020', '2021'))) %>%
    filter(is_pre_2020 != '2020') %>%
    group_by(sex, age_for_plotting, is_pre_2020) %>%
    summarize(death_rate= sum(age_group_sum)/sum(values) * factor) 
  
  return(age_groups_comparison_summary)
}

get_age_groups_summary4 <- function(joined_data) {
  age_groups_comparison_summary <- joined_data %>%
    mutate(is_pre_2020 = ifelse(YEAR < 2020, '2000-2019', ifelse(YEAR == 2020, '2020', '2021'))) %>%
    filter(is_pre_2020 != '2000-2019') %>%
    group_by(sex, age_for_plotting, is_pre_2020) %>%
    summarize(death_rate= sum(age_group_sum)/sum(values) * factor) 
  
  return(age_groups_comparison_summary)
}

##################### 
####OLD PLOT
# get_death_rate_plot <- function(joined_data, title_value) {
#   age_groups_comparison_summary <- get_age_groups_summary(joined_data)
#   
#   ggplot(age_groups_comparison_summary, aes(reorder(age_for_plotting, -death_rate), death_rate, fill=(is_pre_2020))) +
#     geom_bar(stat='identity', position=position_dodge()) +
#     labs(x = 'Age group', y = 'Crude mortality rate per 100 000', 
#          # title = paste0('Crude mortality rate per 100 000 for Poland, ', title_value), 
#          subtitle = title_value,
#          fill = '') +
#     scale_fill_manual(values = c('#005377', '#E86A92')) +
#     coord_flip() +
#     theme_ipsum() +
#     theme(legend.position = "top")
# }
# 
# 
##########
population_deaths_joined_females <- get_population_and_deaths_joined("F", poviats_data = all_poviats_stats_females)
# get_age_groups_summary(population_deaths_joined_females)
# death_rate_plot_females <- get_death_rate_plot(population_deaths_joined_females, "females")
# 
population_deaths_joined_males <- get_population_and_deaths_joined("M", poviats_data = all_poviats_stats_males)

######
# death_rate_plot_males <- get_death_rate_plot(population_death_joined_males, "males")
# 
# library("patchwork")
# 
# death_rate_plot_females + death_rate_plot_males +    # Create grid of plots with title
#   plot_annotation(title = "Comparison of death rate per 1000 people in age groups before and after 2020 in Poland") & 
#   theme(plot.title = element_text(hjust = 0.5))

#########################################
###### death rate plots as age pyramid plots

population_deaths_joined_MF <- rbind(population_deaths_joined_females, population_deaths_joined_males)
population_deaths_joined_MF_summary <- get_age_groups_summary(population_deaths_joined_MF)

population_deaths_joined_MF_summary
population_deaths_joined_MF_summary_1 <- tidyr::unite(population_deaths_joined_MF_summary,"sex_year",sex,is_pre_2020,remove = F)

# age_for_plotting_factor <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29',
#                              '30-34', '35-39', '40-44', '45-49', '50-54', '55-59',
#                              '60-64', '65-69', '70-74', '75-79', '80-84', '85+')

population_deaths_joined_MF_summary_2 <- population_deaths_joined_MF_summary_1 %>%
  mutate(sex_year = ifelse(sex_year == 'F_2000-2019', 'Females: 2000-2019',
                           ifelse(sex_year == 'F_2020-2021', 'Females: 2020-2021',
                                  ifelse(sex_year == 'M_2000-2019', 'Males: 2000-2019',
                                         'Males: 2020-2021')))) %>%
  print(n=72)

# function for 2000-2019 vs 2020
get_summary_for_plotting2 <- function(summary_data) {
  summary_transformed <- tidyr::unite(summary_data,"sex_year",sex,is_pre_2020,remove = F)

  
  summary_transfomred <- summary_transformed %>%
    mutate(sex_year = ifelse(sex_year == 'F_2000-2019', 'Females: 2000-2019',
                             ifelse(sex_year == 'F_2020', 'Females: 2020',
                                    ifelse(sex_year == 'M_2000-2019', 'Males: 2000-2019',
                                           'Males: 2020')))) 
  return(summary_transfomred)
}

# function for 2000-2019 vs 2021
get_summary_for_plotting3 <- function(summary_data) {
  summary_transformed <- tidyr::unite(summary_data,"sex_year",sex,is_pre_2020,remove = F)
  
  
  summary_transfomred <- summary_transformed %>%
    mutate(sex_year = ifelse(sex_year == 'F_2000-2019', 'Females: 2000-2019',
                             ifelse(sex_year == 'F_2021', 'Females: 2021',
                                    ifelse(sex_year == 'M_2000-2019', 'Males: 2000-2019',
                                           'Males: 2021')))) 
  return(summary_transfomred)
}

# function for 2020 vs 2021
get_summary_for_plotting4 <- function(summary_data) {
  summary_transformed <- tidyr::unite(summary_data,"sex_year",sex,is_pre_2020,remove = F)
  
  
  summary_transfomred <- summary_transformed %>%
    mutate(sex_year = ifelse(sex_year == 'F_2020', 'Females: 2020',
                             ifelse(sex_year == 'F_2021', 'Females: 2021',
                                    ifelse(sex_year == 'M_2020', 'Males: 2020',
                                           'Males: 2021')))) 
  return(summary_transfomred)
}


### piramida wieku dla grup wiekowuch
### lata 2000-2019 vs 2021-2022
ggplot(population_deaths_joined_MF_summary_2, aes(x=factor(age_for_plotting, levels = age_for_plotting_factor), y=ifelse(sex=="M", -death_rate, death_rate),
                                                fill=sex_year)) + 
  geom_bar(stat="identity", alpha=0.9, position = position_dodge2(reverse=T)) + 
  coord_flip() + 
  labs(y="Crude mortality rate per 100 000",
       x="Age group",
       fill="",
       title = "Crude mortality rate per 100 000 in Poland, by age group and gender",
       subtitle = "Comparison of years 2000-2019 and 2020-2021") +
  scale_fill_brewer(palette="Paired", breaks = c('Males: 2000-2019', 'Males: 2020-2021', 'Females: 2000-2019', 'Females: 2020-2021'))+
  theme_ipsum() +
  theme(legend.position = 'top')


plot_age_pyramid <- function(summary_data, breaks_list, subtitle) {
  ggplot(summary_data, aes(x=factor(age_for_plotting, levels = age_for_plotting_factor), y=ifelse(sex=="M", -death_rate, death_rate),
                                                    fill=sex_year)) + 
    geom_bar(stat="identity", alpha=0.9, position = position_dodge2(reverse=T)) + 
    coord_flip() + 
    labs(y="Crude mortality rate per 100 000",
         x="Age group",
         fill="",
         title = "Crude mortality rate per 100 000 in Poland, by age group and gender",
         subtitle = subtitle)+
    scale_fill_brewer(palette="Paired", breaks = breaks_list)+
    theme_ipsum() +
    theme(legend.position = 'top')
}

###### lata 2000-2019 vs 2020
population_deaths_joined_MF_summary_comparison_2 <- get_age_groups_summary2(population_deaths_joined_MF)
summary_2000_2019_vs_2020 <- get_summary_for_plotting2(population_deaths_joined_MF_summary_comparison_2)
plot_age_pyramid(summary_2000_2019_vs_2020,
                 c('Males: 2000-2019', 'Males: 2020', 'Females: 2000-2019', 'Females: 2020'),
                 "Comparison of years 2000-2019 and 2020")


##### lata 2000-2019 vs 2021
population_deaths_joined_MF_summary_comparison_3 <- get_age_groups_summary3(population_deaths_joined_MF)
summary_2000_2019_vs_2021 <- get_summary_for_plotting3(population_deaths_joined_MF_summary_comparison_3)
plot_age_pyramid(summary_2000_2019_vs_2021,
                 c('Males: 2000-2019', 'Males: 2021', 'Females: 2000-2019', 'Females: 2021'),
                 "Comparison of years 2000-2019 and 2021")

#### lata 2020 vs 2021
population_deaths_joined_MF_summary_comparison_4 <- get_age_groups_summary4(population_deaths_joined_MF)
summary_2020_vs_2021 <- get_summary_for_plotting4(population_deaths_joined_MF_summary_comparison_4)
plot_age_pyramid(summary_2020_vs_2021,
                 c('Males: 2020', 'Males: 2021', 'Females: 2020', 'Females: 2021'),
                 "Comparison of years 2020 and 2021")



####################
# POST PRODUCTION

population_deaths_joined_MF_summary_2 %>%
  filter(age_for_plotting == '85+')

summary_2000_2019_vs_2020 %>%
  filter(age_for_plotting == '85+')

summary_2000_2019_vs_2021 %>%
  filter(age_for_plotting == '80-84')

summary_2020_vs_2021 %>%
  filter(age_for_plotting == '80-84')
