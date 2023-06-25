#### proby modelo

post_pandemic <- read.csv('../poviats_code/results/POST_PANDEMIC_GENDER.csv')

#### GLM
wro_df <- pre_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Wrocław") %>%
  filter(age_group != 'TOTAL')

wro_df_predict <- post_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Wrocław") %>%
  filter(age_group != 'TOTAL')


m1 <- glm(deaths ~ week + year + age_group + gender, data = wro_df, family = poisson())
summary(m1)
anova(m1)
checkresiduals(m1$residuals)
fcast1 <- forecast(m1)

predict_input <- wro_df_predict %>%
  select(week, year, age_group, gender)


prediction <- predict(m1, predict_input, type = "response", se.fit = T)

wro_df_excess <- wro_df_predict %>%
  mutate(prediction = round(prediction$fit)) %>%
  mutate(excess = deaths - prediction)

wro_df_excess %>%
  group_by(year) %>%
  summarise(sum(excess))

fitted(prediction)


### GLM QUASIPOISSON
m2 <- glm(deaths ~ week + year + age_group + gender, data = wro_df, family = quasipoisson(link = "log"))
summary(m2)

checkresiduals(m2$residuals)

predict_input <- wro_df_predict %>%
  select(week, year, age_group, gender)


prediction <- predict(m1, predict_input, type = "response", se.fit = T)

wro_df_excess <- wro_df_predict %>%
  mutate(prediction = round(prediction$fit)) %>%
  mutate(excess = deaths - prediction)

wro_df_excess %>%
  group_by(year) %>%
  summarise(sum(excess))


### GLM with lagged y

wro_lagged <- wro_df %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2))

wro_lagged <- na.omit(wro_lagged)

m3 <- glm(deaths ~ week + year + age_group + gender + y_lag1 + y_lag2, data = wro_lagged, family = poisson)
summary(m3)

checkresiduals(m3$residuals)

### GLM for total age group -- MOZE BYC TO

wro_df_total <- pre_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Wrocław") %>%
  filter(age_group == 'TOTAL') %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3))

wro_total_lagged <- na.omit(wro_df_total)

wro_df_predict_total <- post_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Wrocław") %>%
  filter(age_group == 'TOTAL') %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3))

m4 <- glm(deaths ~ week + year + gender + y_lag1 + y_lag2 + y_lag3, data = wro_total_lagged, family = poisson())
summary(m4)
checkresiduals(m4)
prediction4 <- predict(m4, wro_df_predict_total, type = "response", se.fit = T)

fitted_values <- c(rep(NA, 3), m4$fitted.values)

total_viz <- wro_df_total %>%
  mutate(fitted_values = fitted_values)

total_viz_male <- total_viz %>%
  filter(gender == "MALE")

excess_viz <- wro_df_predict_total %>%
  mutate(prediction = round(prediction4$fit)) %>%
  mutate(excess = deaths - prediction) 

male_excess <- excess_viz %>%
  filter(gender == "MALE")

ggplot(total_viz_male, aes(date, deaths, group=1)) +
  geom_line() +
  geom_line(data=total_viz_male, aes(date, fitted_values), color = "red") + 
  geom_line(data=male_excess, aes(date, prediction), color = "red") + 
  geom_line(data=male_excess, aes(date, deaths), color="black")
  

### sprawdzenie modelu 4 dla Polski TOTAL
# Load required packages
library(lubridate)
library(dplyr)
library(tidyr)

# Define start and end dates
start_date <- as.Date("2000-01-03")
end_date <- as.Date("2021-12-31")

# Generate weekly dates sequence
weekly_dates <- seq(from = start_date, to = end_date, by = "week")

# Create a data frame with weekly dates
df <- data.frame(date = weekly_dates)

# Calculate week numbers and year
df <- df %>%
  mutate(YEAR = as.numeric(format(date, "%Y")),
         week = as.numeric(format(date, "%V")))

# Create a week number column with the format T01..T53
df <- df %>%
  mutate(week = paste0("T", str_pad(week, 2, pad = "0")))

# Show the data frame
as_tibble(df)

# ALL_DATA_TS_FEMALE <- inner_join(ALL_DATA_TS_FEMALE, as_tibble(df), by=c("week", "YEAR"))

female_total_ts <- ALL_DATA_TS_FEMALE %>%
  filter(wiek_eurostat != 'TOTAL') %>%
  filter(powiat_nazwa == 'Polska') %>%
  mutate(gender = 'FEMALE') %>%
  rename(deaths = count) %>%
  rename(year = YEAR) %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3),
         y_lag4 = lag(deaths, 4))

male_total_ts <- ALL_DATA_TS_MALE %>%
  filter(wiek_eurostat != 'TOTAL') %>%
  filter(powiat_nazwa == "Polska") %>%
  mutate(gender = 'MALE') %>%
  rename(deaths = count) %>%
  rename(year = YEAR) %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3),
         y_lag4 = lag(deaths, 4))
  

total_ts_PL <- bind_rows(female_total_ts, male_total_ts)

mean(total_ts_PL$deaths)
var(total_ts_PL$deaths)

pre_pandemic_PL <- total_ts_PL %>% filter(year < 2020)
post_pandemic_PL <- total_ts_PL %>% filter(year >= 2020)

m4.1 <- glm(deaths ~ week + year + gender + y_lag1 + y_lag2 + y_lag3 + y_lag4, data = pre_pandemic_PL, family = poisson())
summary(m4.1)

checkresiduals(m4.1$residuals)

length(m4.1$fitted.values)


prediction4.1 <- predict(m4.1, post_pandemic_PL, type = "response", se.fit = T)


excess <- post_pandemic_lagged %>%
  mutate(prediction = prediction4.1$fit) %>%
  mutate(excess = deaths-prediction)

pre_pandemic_male <- pre_pandemic_PL %>%
  mutate(fitted = c(rep(NA, 8), m4.1$fitted.values)) %>%
  filter(gender == 'MALE')

excess %>%
  group_by(gender, year) %>%
  summarise(deaths = sum(deaths, na.rm = T), pred = sum(prediction, na.rm = T)) %>%
  mutate(excess = deaths - pred)
  
excess_male <- excess %>% filter(gender == 'MALE')

ggplot(pre_pandemic_male, aes(paste0(week, year), deaths, group=1)) +
  geom_line() +
  geom_line(data=pre_pandemic_male, aes(paste0(week, year), fitted), color = "red") + 
  geom_line(data=excess_male, aes(paste0(week, year), prediction), color = "red") + 
  geom_line(data=excess_male, aes(paste0(week, year), deaths), color="black")


## model, ktory uwzglednia grupe wiekowa -  nie dziala

total_ts_PL_2 <- bind_rows(female_total_ts, male_total_ts)

pre_pandemic_PL_2 <- total_ts_PL_2 %>% filter(year < 2020)
post_pandemic_PL_2 <- total_ts_PL_2 %>% filter(year >= 2020)

m4.2 <- glm(deaths ~ week + year + wiek_eurostat + gender + y_lag1 + y_lag2 + y_lag3, data = pre_pandemic_PL_2, family = poisson())
summary(m4.2)
checkresiduals(m4.2$residuals)


## puszczenie modelu dla Wroclawia na Warszawie

warsaw_df_total <- pre_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Warszawa") %>%
  filter(age_group == 'TOTAL') %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3))

# warsaw_total_lagged <- na.omit(warsaw_df_total)

formula <- deaths ~ week + year + gender + y_lag1 + y_lag2 + y_lag3
warsaw_df_predict_total <- post_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Warszawa") %>%
  filter(age_group == 'TOTAL') %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3))

m4.warsaw <- glm(deaths ~ week + year + gender + y_lag1 + y_lag2 + y_lag3, data = warsaw_df_total, family = poisson())
summary(m4.warsaw)

checkresiduals(m4.warsaw)

prediction4.warsaw <- predict(m4.warsaw, warsaw_df_predict_total, type = "response", se.fit = T)

fitted_values <- c(rep(NA, 3), m4.warsaw$fitted.values)

total_viz <- warsaw_df_total %>%
  mutate(fitted_values = fitted_values)

total_viz_male <- total_viz %>%
  filter(gender == "MALE")

excess_viz <- warsaw_df_predict_total %>%
  mutate(prediction = round(prediction4.warsaw$fit)) %>%
  mutate(excess = deaths - prediction) 

male_excess <- excess_viz %>%
  filter(gender == "MALE")

ggplot(total_viz_male, aes(date, deaths, group=1)) +
  geom_line() +
  geom_line(data=total_viz_male, aes(date, fitted_values), color = "red") + 
  geom_line(data=male_excess, aes(date, prediction), color = "red") + 
  geom_line(data=male_excess, aes(date, deaths), color="black") +
  labs(title = "Male, Warsaw, glm")


#### GE85 dla modelu 4 dla Warszawy - nie dziala dla GE85, za duza korelacja reszt
warsaw_df_GE85 <- pre_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Warszawa") %>%
  filter(age_group == 'Y_GE85') %>%
  mutate(y_lag1 = lag(deaths, 1),
         y_lag2 = lag(deaths, 2),
         y_lag3 = lag(deaths, 3))

warsaw_df_GE85_xts <- xts(warsaw_df_GE85$deaths[warsaw_df_GE85$gender == 'MALE'],
                          order.by = as.Date(warsaw_df_GE85$date[warsaw_df_GE85$gender == "MALE"]), frequency = 52)
ggtsdisplay(diff(warsaw_df_GE85_xts))


m4.warsaw1 <- glm(formula, data = warsaw_df_GE85, family = poisson())
summary(m4.warsaw1)
checkresiduals(m4.warsaw1)

#### Arima dla 85+ latkow z Warszawy
m5 <- auto.arima(warsaw_df_GE85_xts)
summary(m5)
checkresiduals(m5$residuals)
fcast5 <- forecast(m5, h=104)
fcast5
plot(fcast5)

sum(fcast5$mean[52:104])

post_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Warszawa") %>%
  filter(age_group == 'Y_GE85') %>%
  filter(gender == 'MALE') %>%
  filter(year == 2021) %>%
  summarise(sum(deaths))

post_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Warszawa") %>%
  filter(age_group == 'Y_GE85') %>%
  filter(gender == 'MALE')

(6134 - 5206.833)/(32548 + 31810 )  * 10000

warsaw_df_GE85 %>%
  summarise(sum(deaths))

3096 - 2564.615

3038 - 2692.139

warsaw_df_GE85 %>%
  select(population) %>%
  distinct() %>%
  summarise(sum(population))
5206.833/(32548 + 31810 )

88314/1293300
