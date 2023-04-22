library(plotly)
# candle stick plots
all_PL_statistics
write.csv(all_PL_statistics, 'PL_summary_statistics.csv')
all_PL_statistics_female %>%
  print(n=22)
write.csv(all_PL_statistics_female, 'PL_summary_stats_female.csv')
all_PL_statistics_male
write.csv(all_PL_statistics_male, 'PL_summary_stats_male.csv')

get_before_durng_candlestick <- function(data, title) {
  data_00_19 <- data %>%
    filter(YEAR <= 2019) 
  
  data_20_21 <- data %>%
    filter(YEAR >= 2020)  
  
  time <- c('2000-2019', '2020-2021')
  open <- c(data$T01[data$YEAR == 2000], 
            data$T01[data$YEAR == 2020])
  close <- c(data$T52[data$YEAR == 2019], 
             data$T52[data$YEAR == 2021])
  high <- c(max(data_00_19$max), max(data_20_21$max))
  low <- c(min(data_00_19$min), min(data_20_21$min))
  
  candle <- tibble(time, open, close, high, low) 
  
  i <- list(line = list(color = 'red'))
  d <- list(line = list(color = 'green'))
  
  # before and during pandemic candlestick
  plot_ly(x = candle$time,
          data = candle,
          type = "candlestick",
          open = candle$open, close = candle$close, 
          high = candle$high, low = candle$low,
          increasing=i, decreasing=d) %>% 
    layout(title = title,
           xaxis = list(rangeslider = list(visible = F)))
}

get_before_durng_candlestick(all_PL_statistics_female, 
                             'Candlestick plot before and during COVID-19 Pandemic, females')
get_before_durng_candlestick(all_PL_statistics_male, 
                             'Candlestick plot before and during COVID-19 Pandemic, males')
# general candlestick
get_general_candlestick <- function(data, group) {
  i <- list(line = list(color = 'red'))
  d <- list(line = list(color = 'green'))
  
  fig <- plot_ly(x = data$YEAR,
                 data = data,
                 type = "candlestick",
                 open = data$T01, close = data$T52, 
                 high = data$max, low = data$min,
                 increasing=i, decreasing=d) %>% 
    layout(title = paste("Candlestick plot for years 2000-2021,", group),
           xaxis = list(rangeslider = list(visible = F)))
  
  fig <- fig %>% add_lines(x = ~YEAR, y = ~data$T01, 
                           line = list(color = 'black', width = 0.75), inherit = F)
  fig <- fig %>% layout(showlegend = FALSE, yaxis = list(title = "T01"))
  fig
} 
get_general_candlestick(all_PL_statistics_female, 'females')
get_general_candlestick(all_PL_statistics_male, 'males')

# Na nastepny tydzien:

# DEADLINE: 01.05 koniec obliczen

# TODO: liczba kobiet mezczyzn -> jaki jest wskaznik zgonow
# TODO: jak sie beda grupowac zgony total (kobiety, mezczyzni) 
# i zgony wskaznikowe? ()


# TODO: szereg czasowy dla polski tygodniowy -> wyciagamy trend
# TODO: robimy szereg czasowy dla powiatow, porownujemy trendy

