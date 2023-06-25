library(xts)
library(ggplot2)
library(dplyr)
library(tscount)

pre_pandemic <- read.csv('../poviats_code/results/PRE_PANDEMIC_GENDER.csv')
pre_pandemic <- as_tibble(pre_pandemic)

all_cmr = read.csv('../poviats_code/results/ALL_CMR_W_DATES.csv')

head(all_cmr)

biala_podlaska_df <- pre_pandemic %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Biała Podlaska")


biala_podlaska_df %>%
  filter(age_group == "Y_GE85") %>%
  ggplot(aes(x=CMR, y=after_stat(density))) +
  geom_histogram(bins = 5) + 
  facet_wrap(~gender) + 
  geom_density(color="red")

biala_podlaska_df <- biala_podlaska_df %>%
  mutate(year_week = paste0(year,'_', week))

summary(m.biala_podlaska <- glm(CMR ~ year_week + age_group + gender, 
                        data = biala_podlaska_df, family = 'poisson'))

summary(m.biala_podlaska1 <- glm(CMR ~ age_group + gender, 
                                data = biala_podlaska_df, family = 'poisson'))
pacf(residuals(m.biala_podlaska1))

plot(x=biala_podlaska_df$year_week, y=biala_podlaska_df$CMR)

biala_podlaska_df %>%
  select(year_week)



pre_pandemic <- pre_pandemic %>%
  select(-c('name_normalized_y', 'population', 'deaths')) %>%
  mutate(date = as.Date(date))


ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")
summary(pre_pandemic)

head(pre_pandemic)

summary(m1 <- glm(CMR ~ age_group + gender + powiat_numer, family="poisson", data=pre_pandemic))

summary(m2 <- glm(CMR ~ age_group + gender + powiat_numer + date, family="poisson", data=pre_pandemic))

### nnetar puszczony na CMR
wro_total <- all_cmr %>%
  # filter(age_group == "TOTAL") %>%
  filter(name_normalized_x == "Wrocław") %>%
  filter(age_group == 'TOTAL') %>%
  mutate(date = as.Date(date))

pre_pandemic_wro <- wro_total %>%
  filter(format(date, format="%Y") < 2020)

post_pandemic_wro <- wro_total %>%
  filter(format(date, format="%Y") >= 2020)

wro_total

biala_podlaska_total %>%
  select(date, CMR, age_group)
  

wro_pre_pandemic_xts <- xts(pre_pandemic_wro$CMR, order.by = pre_pandemic_wro$date, frequency = 52)

autoplot(wro_pre_pandemic_xts)
ggtsdisplay(wro_pre_pandemic_xts)

fit_wro <- nnetar(wro_pre_pandemic_xts, lambda = "auto")
fcast_wro <- forecast(fit_wro, PI=TRUE, h=104)
plot(fcast_wro)

ggtsdisplay(fit_wro$residuals)

fcast_wro$mean



