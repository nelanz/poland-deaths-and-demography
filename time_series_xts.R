### XTS time series
library(ggplot2)
library(scales)
library(tidyr)
library(viridis)
library(xts)
library(ggfortify)


# data prep
ALL_DATA_TS <- ALL_DATA_AGE_EUROSTAT %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

ALL_DATA_AGE_EUROSTAT %>%
  distinct() %>%
  arrange(powiat_numer) %>%
  filter(powiat_numer > 32) %>%
  select(powiat_nazwa) %>%
  distinct()
  

ALL_DATA_TS_FEMALE <- ALL_DATA_AGE_EUROSTAT_FEMALE %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

ALL_DATA_TS_MALE <- ALL_DATA_AGE_EUROSTAT_MALE %>%
  pivot_longer(cols = starts_with('T'), names_to = 'week', values_to = 'count', values_drop_na = T) 

dates <- seq(as.Date("2000/1/03"), as.Date("2021/12/31"), by = "week")

summary_ts_PL <- ALL_DATA_TS %>%
  filter(powiat_nazwa == 'Polska') %>%
  filter(wiek_eurostat == 'TOTAL') %>%
  mutate(week_number = str_remove(week, 'T')) %>%
  mutate(date = dates)

summary_ts_PL %>%
  group_by(YEAR) %>%
  summarise(sum = sum(count)) %>%
  print(n=22)

summary_ts_PL %>%
  filter(YEAR == 2020) %>%
  select(date, count) %>%
  print(n=53)

summary_xts_PL <-  xts(x = summary_ts_PL$count, order.by = summary_ts_PL$date, frequency = 52)
summary_xts_PL['2020']

sum(coredata(summary_xts_PL['2020'])[,1])

ggplot(as.data.frame(summary_xts_PL), aes(x = index(summary_xts_PL), y = summary_xts_PL)) +
  geom_line(colour = '#D00000') +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland", subtitle = 'Years 2000-2021') +
  # geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  ylim(c(3000,17000)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
# +
#   theme_ipsum()

summary_xts_PL['2012']
plot(summary_xts_PL, plot.type = "single", col = "blue", main = "Seasonal Plot")



### ACF
library(fpp2)

acf(summary_xts_PL, lag.max = 4, plot = T, main = "4 weeks")
acf(summary_xts_PL, lag.max = 12, plot = T, main = "12 weeks")
acf(summary_xts_PL, lag.max = 12, plot = T, main = "26 weeks")
acf(summary_xts_PL, lag.max = 208, plot = T, main = "208 weeks")

start.date <- as.Date("2000-01-01")
end.date <- as.Date("2019-12-31")
pre_pandemic_xts <- summary_xts_PL[paste(start.date, end.date, sep = "/")]

par(mfrow=c(3,2))
acf(pre_pandemic_xts, lag.max = 4, plot = T, main = "4 weeks")
acf(pre_pandemic_xts, lag.max = 12, plot = T, main = "12 weeks")
acf(pre_pandemic_xts, lag.max = 16, plot = T, main = "16 weeks")
acf(pre_pandemic_xts, lag.max = 32, plot = T, main = "32 weeks")
acf(pre_pandemic_xts, lag.max = 52, plot = T, main = "52 weeks")
acf(pre_pandemic_xts, lag.max = 104, plot = T, main = "104 weeks")

start.date <- as.Date("2020-01-01")
end.date <- as.Date("2021-12-31")
post_pandemic <- summary_xts_PL[paste(start.date, end.date, sep = "/")]

par(mfrow=c(3,2))
acf(post_pandemic, lag.max = 4, plot = T, main = "4 weeks")
acf(post_pandemic, lag.max = 12, plot = T, main = "12 weeks")
acf(post_pandemic, lag.max = 16, plot = T, main = "16 weeks")
acf(post_pandemic, lag.max = 32, plot = T, main = "32 weeks")
acf(post_pandemic, lag.max = 52, plot = T, main = "52 weeks")
acf(post_pandemic, lag.max = 104, plot = T, main = "104 weeks")


# PACF

par(mfrow=c(2,2))
pacf(pre_pandemic_xts, lag.max = 4, plot = T, main = "4 weeks")
pacf(pre_pandemic_xts, lag.max = 12, plot = T, main = "12 weeks")
pacf(pre_pandemic_xts, lag.max = 16, plot = T, main = "16 weeks")
pacf(pre_pandemic_xts, lag.max = 32, plot = T, main = "32 weeks")

par(mfrow=c(2,2))
pacf(post_pandemic, lag.max = 4, plot = T, main = "4 weeks")
pacf(post_pandemic, lag.max = 12, plot = T, main = "12 weeks")
pacf(post_pandemic, lag.max = 16, plot = T, main = "16 weeks")
pacf(post_pandemic, lag.max = 32, plot = T, main = "32 weeks")

par(mfrow=c(1,1))
pre_pandemic_spec <- spectrum(pre_pandemic, method = "pgram", window = "hanning")
plot(pre_pandemic_spec, main = "Power Spectrum of Sales Data")

# check if time series is stationary
library(tseries)
library(forecast)

adf.test(summary_xts_PL)
adf.test(diff(pre_pandemic))
adf.test(post_pandemic)

kpss.test(summary_xts_PL)
kpss.test(diff(pre_pandemic))
kpss.test(post_pandemic)

plot(diff(post_pandemic))

auto.arima(pre_pandemic)

library(urca)
pre_pandemic %>% ur.kpss() %>% summary()
pre_pandemic %>% diff() %>% ur.kpss() %>% summary()

plot(pre_pandemic)
plot(diff(pre_pandemic_xts, lag = 2))
plot()

Box.test(diff(pre_pandemic_xts, lag = 2))

acf(na.remove(diff(pre_pandemic_xts)), lag.max = 16, plot = T)

fit <- auto.arima(pre_pandemic_xts, test="kpss", lambda="auto")
summary(fit)

fcast <- forecast(fit, h=104)

fcast_df = as_tibble(fcast)

fcast_PL <- fcast_df %>%
  mutate(date = index(post_pandemic)) %>%
  mutate(ACM_actual = coredata(post_pandemic)[,1]) %>%
  mutate(excess = ACM_actual - `Point Forecast`) %>%
  mutate(year = format(date, "%Y"))

preds <- data.frame(x = index(summary_xts_PL), fitted = c(fit$fitted, fcast_df$`Point Forecast`), true = summary_xts_PL)

p <- ggplot(as.data.frame(summary_xts_PL), aes(x = index(summary_xts_PL), y = summary_xts_PL)) +
  geom_line(colour = 'black') +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland", subtitle = 'Years 2000-2021') +
  # geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  ylim(c(3000,17000)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 

p + geom_line(data=preds, aes(x = x, y=fitted), color='red')

ggtsdisplay(fit$residuals)


fcast_PL %>%
  group_by(year) %>%
  summarise(sum_excess = sum(excess), sum_acm = sum(ACM_actual),
            sum_fcast = sum(`Point Forecast`), sum_hi = sum(`Hi 95`),
            sum_lo = sum(`Lo 95`)) %>%
  mutate(excess_sum = sum_acm - sum_fcast) %>%
  mutate(excess_sum_hi = sum_acm - sum_hi) %>%
  mutate(excess_sum_lo = sum_acm - sum_lo)


## pre pandemic differentieted
d <- 1
ts(pre_pandemic) %>% diff() %>% diff() %>% ggtsdisplay(main="")
fit <- Arima(pre_pandemic_xts, order = c(4,1,4), seasonal = c(0, 1, 1))
summary(fit)
checkresiduals(fit, plot = T)
ggtsdisplay(fit$residuals)

fcast <- forecast(fit, h=104)
plot(fcast)

fcast

as_tibble(fcast) %>%
  mutate(date = index(post_pandemic)) %>%
  mutate(ACM_actual = coredata(post_pandemic)[,1]) %>%
  mutate(excess = ACM_actual - `Point Forecast`) %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(year) %>%
  summarise(sum(excess))



pre_pandemic_xts %>%
  Arima(order = c(4, 1, 4)) %>%
  residuals() %>% ggtsdisplay()

autoplot(forecast(fit, h=104))

ts(pre_pandemic)

fit2 <- ets(pre_pandemic)
autoplot(fit2)
fit2 %>% forecast(h=100) %>% autoplot()
# plot(ma(pre_pandemic, 16))
# ts(pre_pandemic) %>%
#   stlf() %>%
#   autoplot()
# ggseasonplot(pre_pandemic, polar=TRUE)


create_MA <- function(xts_object, MA_order) {
  MA <- stats::filter(xts_object, sides = 2, filter = rep(1/MA_order, MA_order))
  
  return(MA)
}

MA_4_weeks <- create_MA(pre_pandemic, 4)
MA_12_weeks <- create_MA(pre_pandemic, 12)
ts.plot(pre_pandemic)
lines(MA_4_weeks, col="red")
lines(MA_12_weeks, col="green")

window(pre_pandemic, start = 2000)
fit <- tslm(pre_pandemic <- trend + season)

#### chyba dziala siec neuronowa XDDDD
fit <- nnetar(pre_pandemic_xts, lambda = 0)
autoplot(forecast(fit,h=100))
fcast <- forecast(fit, PI=TRUE, h=104)
plot(fcast)
lines(pre_pandemic_xts)

summary(fit)
fitted.values(fcast)

fit$x
pre_pandemic_xts

fcast_df = as_tibble(fcast)

##### POZOSTALE PROBY
decomp <- decompose(to.period(pre_pandemic_xts, period = "weeks"))

to.period(pre_pandemic_xts, period = "weeks")

p <- ggplot(as.data.frame(summary_xts_PL), aes(x = index(summary_xts_PL), y = summary_xts_PL)) +
  geom_line(colour = 'black') +
  labs(x = "Year", y = "Death count", title = "Weekly deaths in Poland", subtitle = 'Years 2000-2021') +
  # geom_smooth(method="lm", se=F, color = "#E86A92", size = 0.75) +
  ylim(c(3000,17000)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") 

y <- predict(fit)


preds <- data.frame(x = index(summary_xts_PL), fitted = c(fit$fitted, fcast_df$`Point Forecast`), true = summary_xts_PL)

p + geom_line(data=preds, aes(x = x, y=fitted), color='red')

fcast_PL <- fcast_df %>%
  mutate(date = index(post_pandemic)) %>%
  mutate(ACM_actual = coredata(post_pandemic)[,1]) %>%
  mutate(excess = ACM_actual - `Point Forecast`) %>%
  mutate(year = format(date, "%Y"))

fcast_PL %>%
  group_by(year) %>%
  summarise(sum_excess = sum(excess), sum_acm = sum(ACM_actual),
            sum_fcast = sum(`Point Forecast`), sum_hi = sum(`Hi 95`),
            sum_lo = sum(`Lo 95`)) %>%
  mutate(excess_sum = sum_acm - sum_fcast) %>%
  mutate(excess_sum_hi = sum_acm - sum_hi) %>%
  mutate(excess_sum_lo = sum_acm - sum_lo)

coredata(post_pandemic)[,1]

post_pandemic

ggtsdisplay(fcast$residuals)

fit
##### 

fit2 <- nnetar(pre_pandemic_xts, lambda = "auto", repeats = 30)
preds2 <- data.frame(x = index(summary_xts_PL), fitted = c(fit2$fitted, fcast_df$`Point Forecast`), true = summary_xts_PL)
fcast2 <- forecast(fit2, PI=TRUE, h=104)
autoplot(fcast2)
fcast_df2 = as_tibble(fcast2)

preds2 <- data.frame(x = index(summary_xts_PL), fitted = c(fit2$fitted, fcast_df2$`Point Forecast`), true = summary_xts_PL)
p + geom_line(data=preds2, aes(x = x, y=fitted), color='red')

fcast_PL2 <- fcast_df2 %>%
  mutate(date = index(post_pandemic)) %>%
  mutate(ACM_actual = coredata(post_pandemic)[,1]) %>%
  mutate(excess = ACM_actual - `Point Forecast`) %>%
  mutate(year = format(date, "%Y"))

fcast2$mean

fcast_PL2 %>%
  group_by(year) %>%
  summarise(sum_excess = sum(excess), sum_acm = sum(ACM_actual),
            sum_fcast = sum(`Point Forecast`), sum_hi = sum(`Hi 95`),
            sum_lo = sum(`Lo 95`)) %>%
  mutate(excess_sum = sum_acm - sum_fcast) %>%
  mutate(excess_sum_hi = sum_acm - sum_hi) %>%
  mutate(excess_sum_lo = sum_acm - sum_lo)
ggtsdisplay(fit2$residuals)


####


