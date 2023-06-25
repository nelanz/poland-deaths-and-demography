### SPLINES
library(splines)
library(dlnm)
library(mgcv)
library(forecast)
library(tidyverse)

set.seed(42)

get_train_data <- function(age_group_name, poviat_name, gender_name) {
  train_dataset <- pre_pandemic %>% 
    filter(name_normalized_x == poviat_name) %>%
    filter(age_group == age_group_name) %>%
    filter(gender == gender_name) %>%
    select(powiat_numer, year, week, date, age_group, deaths, population, gender) %>%
    mutate(week = str_remove_all(week, 'T')) %>%
    mutate(week = as.numeric(week))
  
  return(train_dataset)
}

get_test_data <- function(age_group_name, poviat_name, gender_name) {
  test_dataset <- post_pandemic %>% 
    filter(name_normalized_x == poviat_name) %>%
    filter(age_group == age_group_name) %>%
    filter(gender == gender_name) %>%
    select(powiat_numer, year, week, date, age_group, deaths, population, gender) %>%
    mutate(week = str_remove_all(week, 'T')) %>%
    mutate(week = as.numeric(week))
  
  return(test_dataset)
}

#### TOTAL
total_train <- get_train_data(age_group_name = "TOTAL", poviat_name="Warszawa", gender_name = 'MALE')
total_test <- get_test_data(age_group_name = "TOTAL", poviat_name="Warszawa", gender_name = 'MALE')

total_plot <- ggplot(total_train, aes(as.Date(date), deaths)) + 
  geom_line(color = 'gray') + 
  geom_line(data=total_test, aes(as.Date(date), deaths), color="gray")

### check if we need to compensate for overdispension in Poisson - answer: we do
disp_dat <- total_train %>% 
  group_by(gender) %>%
  summarize(
    avg_inc_rate = mean(deaths),
    var_inc_rate = var(deaths),
    var_over_avg = var_inc_rate/avg_inc_rate
  )

total_train

### test of spline predictions
## model - first try

model_1 <- gam(deaths ~ s(year, k = length(unique(total_train$year))) +
                      s(week, bs = "cc", k = length(unique(total_train$week))),
                    data = total_train, family = nb(theta = NULL, link = "log"))
summary(model_1)
checkresiduals(model_1$residuals)
model_1$coefficients

pred_1 <- predict(model_1,
                se.fit = TRUE,
                type = "response",
                newdata = data.frame(year = c(rep(2020, 53),
                                              rep(2021, 52)),
                                     week = c(1:53, 1:52)))



total_train['model_fit'] <- model_1$fitted.values
total_test['pred_fit'] <- pred_1$fit

total_plot + 
  geom_line(data=total_train, aes(as.Date(date), model_fit), color = 'red')  +
  geom_line(data=total_test, aes(as.Date(date), pred_fit), color='red')

excess_sum <- total_test %>%
  mutate(excess = deaths-pred_fit) %>%
  group_by(year) %>%
  summarise(excess_sum = sum(excess))

excess_cmr <- inner_join(excess_sum,
           total_test %>% select('year', 'population') 
           %>% distinct()) %>%
  mutate(excess_CMR = excess_sum/population * 100000)

excess_cmr

#### GAM dla Polski
# population_data 

formula <- deaths ~ s(year, k = length(unique(total_train$year))) +
  s(week, bs = "cc", k = length(unique(total_train$week)))

total_pl <- ALL_DATA_TS %>%
  filter(powiat_nazwa == 'Polska') %>%
  rename(age_group = wiek_eurostat) %>%
  rename(year = YEAR) %>%
  rename(deaths = count) %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week)) %>%
  filter(age_group == 'TOTAL') 

total_pl_train <- total_pl %>% filter(year < 2020)
total_pl_test <- total_pl %>% filter(year >= 2020)

model_1_1 <- gam(formula,
               data = total_pl_train, family = nb(theta = NULL, link = "log"))
summary(model_1_1)
checkresiduals(model_1_1$residuals)

pred_1_1 <- predict(model_1_1,
                  se.fit = TRUE,
                  type = "response",
                  newdata = data.frame(year = c(rep(2020, 53),
                                                rep(2021, 52)),
                                       week = c(1:53, 1:52)))

total_pl_test['pred_fit'] <- pred_1_1$fit
excess_sum <- total_pl_test %>%
  mutate(excess = deaths-pred_fit) %>%
  filter(!(year == 2020 & week == 1)) %>%
  group_by(year) %>%
  summarise(excess_sum = sum(excess))

total_pl_viz <- total_pl %>% mutate(time = seq(1, nrow(total_pl)))
total_pl_fit <- total_pl_viz %>%
  mutate(fitted = c(model_1_1$fitted.values, pred_1_1$fit))

ggplot(total_pl_fit, aes(time, deaths)) + 
  geom_line(color = 'gray') + 
  geom_line(data=total_pl_fit, aes(time, fitted), color = 'red') +
  theme_ipsum() +
  labs(title = 'Weekly time series of deaths in Poland with fitted expected counts.',
       x = 'Time',
       y = 'Deaths')

##############

### Run model on all totals on all poviats

## 1. data preprocessing
all_cmr <- read.csv('../poviats_code/results/ALL_CMR_W_DATES.csv')
all_cmr_females <- read.csv('../poviats_code/results/ALL_CMR_FEMALES_W.csv')
all_cmr_males <- read.csv('../poviats_code/results/ALL_CMR_MALES_W.csv')

total_poviats <- all_cmr %>%
  filter(age_group == 'TOTAL') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

total_poviats_train <- total_poviats %>%
  select(powiat_numer, year, week, deaths, population) %>% filter(year < 2020)
total_poviats_test <- total_poviats %>%
  select(powiat_numer, year, week, deaths, population) %>% filter(year >= 2020) 

### powiaty, ktore nie maja danych od 2000 roku
total_poviats_train %>%
  group_by(powiat_numer) %>%
  count() %>%
  filter(n < 1043)

### odflitowujemy 263 i 1431, bo one mają dane tylko do 2002

total_poviats_train <- total_poviats_train %>% filter(!(powiat_numer %in% c(263, 265, 1431))) %>% mutate(number = as.numeric(as.factor(powiat_numer)))

# total_poviats %>%
#   filter(powiat_numer == 1821) 
# 
# total_poviats %>%
#   filter(name_normalized_x == 'wałbrzyski')

poviats_numbers <- unique(sort(total_poviats_train$powiat_numer))

acm_preds <- tibble(powiat_numer = rep(poviats_numbers, each = 105),
                    year = rep(c(rep(2020, 53),
                             rep(2021, 52)), times = length(poviats_numbers)),
                    week = rep(c(1:53, 1:52), times = length(poviats_numbers)),
                    expected_acm = NA,
                    expected_acm_se = NA,
                    # expected_log_acm = NA,
                    # expected_log_acm_se = NA
                    ) %>% mutate(number = as.numeric(as.factor(powiat_numer)))

for (i in 1:max(acm_preds$number)) {
  print(i)
  whichs <- which(total_poviats_train$number == i)
  temp <- total_poviats_train[whichs, ]

  model <- gam(formula,
               data = temp, family = nb(theta = NULL, link = "log"))
  
  pred <- predict(model,
                  se.fit = TRUE,
                  type = "response",
                  newdata = data.frame(year = c(rep(2020, 53),
                                                rep(2021, 52)),
                                       week = c(1:53, 1:52)))
  
  whichs_pred <- which(acm_preds$number == i)
  acm_preds[whichs_pred, "expected_acm"] <- pred$fit
  acm_preds[whichs_pred, "expected_acm_se"] <- pred$se.fit
  
  # pred_log <- predict(model,
  #                 type = "response",
  #                 newdata = data.frame(year = c(rep(2020, 53),
  #                                               rep(2021, 52)),
  # #                                      week = c(1:53, 1:52)))
  # acm_preds[whichs_pred, "expected_log_acm"] <- pred_log$fit
  # acm_preds[whichs_pred, "expected_log_acm_se"] <- pred_log$se.fit
  
}

summary(model)

acm_preds
save(acm_preds, file='acm_preds_males.RData')
total_poviats




excess_df <- left_join(acm_preds, total_poviats %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year')) %>%
  mutate(excess = CMR-expected_acm)

##### SUMA EXCESS DLA POWIATOW, DO POROWNANIA Z SUMA DLA CALEGO KRAJU

excess_df %>%
  filter(age_group == 'TOTAL') %>%
  group_by(year)%>%
  summarise(sum(deaths), sum(expected_acm), sum(excess), sum(deaths) -  sum(expected_acm)) %>%
  print(n = 22)


ALL_DATA_TS%>%
  filter(wiek_eurostat == 'TOTAL') %>%
  filter(powiat_numer > 32) %>%
  group_by(YEAR)%>%
  summarise(sum(count))



##### ZALADOWANIE ACM
load(file = "../../acm_preds.RData")
all_acm <- acm_preds
load(file = "acm_preds_males.RData")
all_acm_males <- acm_preds
load(file = "acm_preds_females.RData")
all_acm_females <- acm_preds

### policzenie CMR
all_excess_df <- left_join(all_acm, total_poviats %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000) %>% 
  mutate(gender = "TOTAL", age_group = "TOTAL")

all_excess_df %>%
  print(n=50)

all_excess_males_df <- left_join(all_acm_males, total_poviats_males %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000) %>%
  mutate(age_group = "TOTAL", gender = "MALE")

all_excess_females_df <- left_join(all_acm_females, total_poviats_females %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000) %>%
  mutate(age_group = "TOTAL", gender = "FEMALE")

save(all_excess_df, all_excess_males_df, all_excess_females_df, file='excess_total.RData')

##################################
#### running model on age groups

# all_cmr <- read.csv('../poviats_code/results/ALL_CMR_W_DATES.csv')
all_cmr_females <- read.csv('../poviats_code/results/ALL_CMR_FEMALES_W.csv')
# all_cmr_males <- read.csv('../poviats_code/results/ALL_CMR_MALES_W.csv')

total_poviats <- all_cmr_females %>%
  filter(age_group != 'TOTAL') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week)) %>%
  filter(!(powiat_numer %in% c(263, 265, 1431))) %>% 
  mutate(number = as.numeric(as.factor(powiat_numer)))

current_gender <- 'MALE'

age_groups <- unique(total_poviats$age_group)

total_poviats_train <- total_poviats %>%
  select(powiat_numer, age_group, year, week, deaths, population, number) %>% filter(year < 2020)
total_poviats_test <- total_poviats %>%
  select(powiat_numer, age_group, year, week, deaths, population, number) %>% filter(year >= 2020) 

acm_preds_parent <- tibble() 

for (group in age_groups) {
  print(group)
  
  current_train <- total_poviats_train %>% filter(age_group == group)
  
  poviats_numbers <- unique(sort(total_poviats_train$powiat_numer))
  
  current_acm_preds <- tibble(powiat_numer = rep(poviats_numbers, each = 105),
                      year = rep(c(rep(2020, 53),
                                   rep(2021, 52)), times = length(poviats_numbers)),
                      week = rep(c(1:53, 1:52), times = length(poviats_numbers)),
                      expected_acm = NA,
                      expected_acm_se = NA,
                      age_group = group,
                      gender = current_gender,
                      # expected_log_acm = NA,
                      # expected_log_acm_se = NA
  ) %>% mutate(number = as.numeric(as.factor(powiat_numer)))
  
  for (i in 1:max(acm_preds$number)) {
    print(i)
    whichs <- which(current_train$number == i)
    temp <- current_train[whichs, ]
    
    model <- gam(formula,
                 data = temp, family = nb(theta = NULL, link = "log"))
    
    pred <- predict(model,
                    se.fit = TRUE,
                    type = "response",
                    newdata = data.frame(year = c(rep(2020, 53),
                                                  rep(2021, 52)),
                                         week = c(1:53, 1:52)))
    
    whichs_pred <- which(current_acm_preds$number == i)
    current_acm_preds[whichs_pred, "expected_acm"] <- pred$fit
    current_acm_preds[whichs_pred, "expected_acm_se"] <- pred$se.fit
    
  }
  
  acm_preds_parent <- rbind(acm_preds_parent, current_acm_preds)
}

summary(model)

acm_preds_parent
checkresiduals(model$residuals)
# save(acm_preds_parent, file='acm_preds_age_groups_total.RData')
# save(acm_preds_parent, file='acm_preds_age_groups_total_female.RData')
save(acm_preds_parent, file='acm_preds_age_groups_total_male.RData')
# total_poviats %>%
#   filter(powiat_numer == 1821) 
# 
# total_poviats %>%
#   filter(name_normalized_x == 'wałbrzyski')

acm_preds_parent %>%
  filter(age_group == 'Y_GE85')

load('acm_preds_age_groups_total.RData')
all_acm_preds_age_group <- acm_preds_parent
load('acm_preds_age_groups_total_female.RData')
female_acm_preds_age_group <- acm_preds_parent
load('acm_preds_age_groups_total_male.RData')
male_acm_preds_age_group <- acm_preds_parent

all_excess_age_groups <- left_join(all_acm_preds_age_group, total_poviats %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year', 'age_group')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000)

female_excess_age_groups <- left_join(female_acm_preds_age_group, total_poviats %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year', 'age_group')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000)

male_excess_age_groups <- left_join(male_acm_preds_age_group, total_poviats %>% filter(year >= 2020), by = c('powiat_numer', 'week', 'year', 'age_group')) %>%
  mutate(excess = deaths-expected_acm, excess_CMR = excess/population * 100000)

save(all_excess_age_groups, female_excess_age_groups, male_excess_age_groups, file="excess_age_groups.RData")

load(file="excess_age_groups.RData")

all_excess_age_groups %>%
  filter(age_group == 'Y_GE85') %>%
  filter(name_normalized_x == 'Wrocław') %>%
  mutate(time = seq(1, 105)) %>%
  ggplot(aes(time, deaths)) +
  geom_line(color='gray') +
  geom_line(data = all_excess_age_groups %>%
              filter(age_group == 'Y_GE85') %>%
              filter(name_normalized_x == 'Wrocław') %>%
              mutate(time = seq(1, 105)), aes(time, expected_acm), color = 'red')

ummary(model)

acm_preds


##### MODEL DLA DZIECI 0-14

# all_cmr <- read.csv('../poviats_code/results/ALL_CMR_W_DATES.csv')
# all_cmr_females <- read.csv('../poviats_code/results/ALL_CMR_FEMALES_W.csv')
all_cmr_males <- read.csv('../poviats_code/results/ALL_CMR_MALES_W.csv')

kids_poviats <- all_cmr_males %>%
  mutate(age_group_normalized = ifelse(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'), 'Y_LT14', age_group)) %>%
  filter(age_group_normalized == 'Y_LT14') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))


kids_poviats_train <- kids_poviats %>%
  filter(year < 2020) %>%
  select(powiat_numer, year, week, deaths) %>%
  group_by(year, week, powiat_numer) %>%
  summarise(deaths = sum(deaths)) 

kids_poviats_test <- kids_poviats %>%
  select(powiat_numer, name_normalized_x, age_group, age_group_normalized, year, week, deaths, population) %>% filter(year >= 2020) 

### powiaty, ktore nie maja danych od 2000 roku
kids_poviats_train %>%
  group_by(powiat_numer) %>%
  count() %>%
  filter(n < 1043)

### odflitowujemy 263 i 1431, bo one mają dane tylko do 2002

kids_poviats_train <- kids_poviats_train %>% filter(!(powiat_numer %in% c(263, 265, 1431))) %>% mutate(number = as.numeric(as.factor(powiat_numer)))

poviats_numbers <- unique(sort(kids_poviats_train$powiat_numer))
length(poviats_numbers)

kids_acm_preds <- tibble(powiat_numer = rep(poviats_numbers, each = 105),
                    year = rep(c(rep(2020, 53),
                                 rep(2021, 52)), times = length(poviats_numbers)),
                    week = rep(c(1:53, 1:52), times = length(poviats_numbers)),
                    expected_acm = NA,
                    expected_acm_se = NA,
                    age_group = 'Y_LT14',
                    gender = 'MALE',
                    # expected_log_acm = NA,
                    # expected_log_acm_se = NA
) %>% mutate(number = as.numeric(as.factor(powiat_numer)))

for (i in 1:max(kids_acm_preds$number)) {
  print(i)
  whichs <- which(kids_poviats_train$number == i)
  temp <- kids_poviats_train[whichs, ]
  
  model <- gam(formula,
               data = temp, family = nb(theta = NULL, link = "log"))
  
  pred <- predict(model,
                  se.fit = TRUE,
                  type = "response",
                  newdata = data.frame(year = c(rep(2020, 53),
                                                rep(2021, 52)),
                                       week = c(1:53, 1:52)))
  
  whichs_pred <- which(kids_acm_preds$number == i)
  kids_acm_preds[whichs_pred, "expected_acm"] <- pred$fit
  kids_acm_preds[whichs_pred, "expected_acm_se"] <- pred$se.fit
  
  # pred_log <- predict(model,
  #                 type = "response",
  #                 newdata = data.frame(year = c(rep(2020, 53),
  #                                               rep(2021, 52)),
  # #                                      week = c(1:53, 1:52)))
  # acm_preds[whichs_pred, "expected_log_acm"] <- pred_log$fit
  # acm_preds[whichs_pred, "expected_log_acm_se"] <- pred_log$se.fit
  
}

summary(model)

kids_poviats_test <- kids_poviats_test %>%
  group_by(year, week, powiat_numer, name_normalized_x, age_group_normalized) %>%
  summarise(deaths = sum(deaths), population = sum(population))

kids_acm_preds

excess_kids <- left_join(kids_acm_preds, kids_poviats_test, by=c('year', 'week', 'powiat_numer')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = expected_acm/population * 10000)

excess_kids %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(excess))

male_excess_kids <- excess_kids

all_excess_kids <- excess_kids
female_excess_kids <- excess_kids


save(all_excess_kids, female_excess_kids, male_excess_kids, file="excess_kids_all.RData")
load(file="excess_kids_all.RData")

########### Walbrzych po prostu zrobiony zostanie od 2013
### Walbrzych

### najpierw totals
walbrzych <- all_cmr%>%
  filter(name_normalized_x == 'Wałbrzych') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

walbrzych_total <- walbrzych %>% filter(age_group == 'TOTAL') %>%
  filter(year >= 2013 & year < 2020)

walbrzych_test <- walbrzych %>% filter(year >= 2020) %>% filter(age_group == 'TOTAL')

model_walbrzych <- gam(deaths ~ s(year, k = length(unique(walbrzych_total$year))) +
                         s(week, bs = "cc", k = length(unique(walbrzych_total$week))),
             data = walbrzych_total, family = nb(theta = NULL, link = "log"))
model_walbrzych
summary(model_walbrzych)
plot(model_walbrzych)

pred_walbrzych <- predict(model_walbrzych,
                    se.fit = TRUE,
                    type = "response",
                    newdata = data.frame(year = c(rep(2020, 53),
                                                  rep(2021, 52)),
                                         week = c(1:53, 1:52)))


walbrzych_excess_total_males <- walbrzych_test %>% 
  mutate(expected_acm = pred_walbrzych$fit, expected_acm_se = pred_walbrzych$se.fit, excess = deaths - expected_acm, excess_cmr = excess/population)

walbrzych_excess_total_males %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(excess), sum(expected_acm))

## total and total
walbrzych_excess_total
## total and females
walbrzych_excess_total_females
## total and males
walbrzych_excess_total_males

save(walbrzych_excess_total, walbrzych_excess_total_females, walbrzych_excess_total_males, file='excess_total_walbrzych.RData')
########
### Walbrzych grupy wiekowe
########
walbrzych <- all_cmr_females%>%
  filter(name_normalized_x == 'Wałbrzych') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

walbrzych_age_groups <- walbrzych %>% filter(age_group != 'TOTAL') %>%
  # filter(year >= 2013 & year < 2020) %>%
mutate(age_group_normalized = ifelse(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'), 'Y_LT14', age_group)) %>%
  group_by(powiat_numer, year, week, age_group_normalized) %>%
  summarize(deaths = sum(deaths))

current_gender <- 'TOTAL'

age_groups <- unique(walbrzych_age_groups$age_group_normalized)

walbrzych_train <- walbrzych_age_groups  %>% filter(year >= 2013 & year < 2020)
# total_poviats_test <- total_poviats %>%
#   select(powiat_numer, age_group, year, week, deaths, population, number) %>% filter(year >= 2020)

acm_preds_parent <- tibble() 

for (group in age_groups) {
  print(group)
  
  current_train <- walbrzych_train %>% filter(age_group_normalized == group)
  
  # unique(current_train$year)
  
  poviats_numbers <- unique(sort(walbrzych_train$powiat_numer))
  
  current_acm_preds <- tibble(powiat_numer = rep(poviats_numbers, each = 105),
                              year = rep(c(rep(2020, 53),
                                           rep(2021, 52)), times = length(poviats_numbers)),
                              week = rep(c(1:53, 1:52), times = length(poviats_numbers)),
                              expected_acm = NA,
                              expected_acm_se = NA,
                              age_group = group,
                              gender = current_gender,
                              # expected_log_acm = NA,
                              # expected_log_acm_se = NA
  ) 
  
    
    model <- gam(deaths ~ s(year, k = length(unique(current_train$year))) +
                   s(week, bs = "cc", k = length(unique(current_train$week))),
                 data = current_train, family = nb(theta = NULL, link = "log"))
    
    pred <- predict(model,
                    se.fit = TRUE,
                    type = "response",
                    newdata = data.frame(year = c(rep(2020, 53),
                                                  rep(2021, 52)),
                                         week = c(1:53, 1:52)))
    
    current_acm_preds["expected_acm"] <- pred$fit
    current_acm_preds["expected_acm_se"] <- pred$se.fit
    
  
  acm_preds_parent <- rbind(acm_preds_parent, current_acm_preds)
}

summary(model)
acm_preds_parent
#### total and age groups
acm_preds_walbrzych_age_groups_total <- acm_preds_parent

#### females and age groups
acm_preds_walbrzych_age_groups_female <- acm_preds_parent

### males and age groups
acm_preds_walbrzych_age_groups_male <- acm_preds_parent

#### excess
walbrzych <- all_cmr_males%>%
  filter(name_normalized_x == 'Wałbrzych') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

walbrzych_age_groups <- walbrzych %>% filter(age_group != 'TOTAL') %>%
  # filter(year >= 2013 & year < 2020) %>%
  mutate(age_group_normalized = ifelse(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'), 'Y_LT14', age_group)) %>%
  group_by(powiat_numer, year, week, age_group_normalized) %>%
  summarize(deaths = sum(deaths), population = sum(population))

walbrzych_test <- walbrzych_age_groups %>% filter(year >= 2020) %>%
  rename(age_group = age_group_normalized)

## total
excess_walbrzych_age_groups_total <- left_join(acm_preds_walbrzych_age_groups_total, walbrzych_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population)

excess_walbrzych_age_groups_total %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))

### females
excess_walbrzych_age_groups_female <- left_join(acm_preds_walbrzych_age_groups_female, walbrzych_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population)

excess_walbrzych_age_groups_female %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))

### males
excess_walbrzych_age_groups_male <- left_join(acm_preds_walbrzych_age_groups_male, walbrzych_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population)

excess_walbrzych_age_groups_male %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))

save(excess_walbrzych_age_groups_total, excess_walbrzych_age_groups_female,
     excess_walbrzych_age_groups_male, file='excess_age_groups_walbrzych.RData')

load(file='excess_age_groups_walbrzych.RData')


##### POWIAT WAŁBRZYSKI - MIAL DOLACZONY WALBRZYCH OD 2002
# TRZEBA GO ZROBIC OD 2013 
walbrzyski <- all_cmr_females%>%
  filter(name_normalized_x == 'wałbrzyski') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

all_cmr %>%
  filter(powiat_numer == 221) %>%
filter(year >= 2013) %>%
  filter(age_group == 'Y20-24')

walbrzyski_age_groups <- walbrzyski %>% filter(age_group != 'TOTAL') %>%
  # filter(year >= 2013 & year < 2020) %>%
  mutate(age_group_normalized = ifelse(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'), 'Y_LT14', age_group)) %>%
  group_by(powiat_numer, year, week, age_group_normalized) %>%
  summarize(deaths = sum(deaths))

current_gender <- 'FEMALE'

age_groups <- unique(walbrzyski_age_groups$age_group_normalized)

walbrzyski_train <- walbrzyski_age_groups  %>% filter(year >= 2013 & year < 2020)
# total_poviats_test <- total_poviats %>%
#   select(powiat_numer, age_group, year, week, deaths, population, number) %>% filter(year >= 2020)

acm_preds_parent <- tibble() 

for (group in age_groups) {
  print(group)
  
  current_train <- walbrzyski_train %>% filter(age_group_normalized == group)
  
  # unique(current_train$year)
  
  poviats_numbers <- unique(sort(walbrzyski_train$powiat_numer))
  
  current_acm_preds <- tibble(powiat_numer = rep(poviats_numbers, each = 105),
                              year = rep(c(rep(2020, 53),
                                           rep(2021, 52)), times = length(poviats_numbers)),
                              week = rep(c(1:53, 1:52), times = length(poviats_numbers)),
                              expected_acm = NA,
                              expected_acm_se = NA,
                              age_group = group,
                              gender = current_gender,
                              # expected_log_acm = NA,
                              # expected_log_acm_se = NA
  ) 
  
  
  model <- gam(deaths ~ s(year, k = length(unique(current_train$year))) +
                 s(week, bs = "cc", k = length(unique(current_train$week))),
               data = current_train, family = nb(theta = NULL, link = "log"))
  
  pred <- predict(model,
                  se.fit = TRUE,
                  type = "response",
                  newdata = data.frame(year = c(rep(2020, 53),
                                                rep(2021, 52)),
                                       week = c(1:53, 1:52)))
  
  current_acm_preds["expected_acm"] <- pred$fit
  current_acm_preds["expected_acm_se"] <- pred$se.fit
  
  
  acm_preds_parent <- rbind(acm_preds_parent, current_acm_preds)
}

summary(model)
acm_preds_parent
#### total and age groups
# acm_preds_walbrzych_age_groups_total <- acm_preds_parent
acm_preds_walbrzyski_age_groups_total <- acm_preds_walbrzych_age_groups_total

#### females and age groups
# acm_preds_walbrzych_age_groups_female <- acm_preds_parent 
acm_preds_walbrzyski_age_groups_female <- acm_preds_walbrzych_age_groups_female

### males and age groups
acm_preds_walbrzyski_age_groups_male <- acm_preds_parent

#### excess
walbrzyski <- all_cmr_males%>%
  filter(name_normalized_x == 'wałbrzyski') %>%
  mutate(week = str_remove_all(week, 'T')) %>%
  mutate(week = as.numeric(week))

walbrzyski_age_groups <- walbrzyski %>% filter(age_group != 'TOTAL') %>%
  # filter(year >= 2013 & year < 2020) %>%
  mutate(age_group_normalized = ifelse(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'), 'Y_LT14', age_group)) %>%
  group_by(powiat_numer, year, week, age_group_normalized) %>%
  summarize(deaths = sum(deaths), population = sum(population))

walbrzyski_test <- walbrzyski_age_groups %>% filter(year >= 2020) %>%
  rename(age_group = age_group_normalized)

## total
excess_walbrzyski_age_groups_total <- left_join(acm_preds_walbrzyski_age_groups_total, walbrzyski_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population)

excess_walbrzyski_age_groups_total %>%
  group_by(year, age_group) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))

### females
excess_walbrzyski_age_groups_female <- left_join(acm_preds_walbrzyski_age_groups_female, walbrzyski_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population) ### cos tu jest nie tak

excess_walbrzyski_age_groups_female %>%
  group_by(year) %>%
  summarise(sum(expected_acm), sum(excess))

### males
excess_walbrzyski_age_groups_male <- left_join(acm_preds_walbrzyski_age_groups_male, walbrzyski_test, by = c('year', 'week', 'age_group')) %>%
  mutate(excess = deaths - expected_acm, excess_cmr = excess/population)

excess_walbrzych_age_groups_male %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))

save(excess_walbrzyski_age_groups_total, excess_walbrzyski_age_groups_female,
     excess_walbrzyski_age_groups_male, file='excess_age_groups_walbrzyski.RData')

load(file='excess_age_groups_walbrzych.RData')



