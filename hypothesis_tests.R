# test hipotez - srednia i mediana
get_deaths_vector <- function(dataset) {
  deaths_vector <- c()
  for (i in 1:length(dataset)) {
    curr_col <- colnames(dataset)[i]
    deaths_vector[i] <- dataset[curr_col]
  }
  return(unlist(deaths_vector))
}

# Polska, summary

pre_2020 <- all_poland_summary %>%
  filter(YEAR < 2020) %>%
  select(., T01:T53) 

post_2020 <- all_poland_summary %>%
  filter(YEAR >= 2020)%>%
  select(., T01:T53) 

pre_2020_l <- get_deaths_vector(pre_2020)


get_pre_2020_histogram <- function(deaths_vector, plot_title) {
  pre_2020_t <- as_tibble(deaths_vector)
  
  pre_2020_stats <- pre_2020_t |>
    summarize(mean_value = mean(value, na.rm = T))
  
  
  ggplot(pre_2020_t, aes(x = value)) +
    geom_histogram(color = 'white', fill = '#005377', binwidth = 200) + 
    geom_vline(aes(xintercept = mean_value), 
               pre_2020_stats, 
               color = "red", 
               linewidth = 0.9,
               linetype = 'dashed'
    ) +
    theme_ipsum() + 
    labs(title = plot_title,
         subtitle = 'Years 2000-2019',
         x = 'Number of deaths',
         y = 'Count') + 
    xlim(c(2000, 7050))
}

get_post_2020_histogram <- function(deaths_vector, plot_title) {
  post_2020_t <- as_tibble(deaths_vector)
  
  post_2020_stats <- post_2020_t |>
    summarize(mean_value = mean(value, na.rm = T))

  ggplot(post_2020_t, aes(x = value)) +
    geom_histogram(color = 'white', fill = '#E86A92', binwidth = 200) + 
    geom_vline(aes(xintercept = mean_value), 
               post_2020_stats, 
               color = "blue", 
               linewidth = 0.9,
               linetype = 'dashed'
    ) +
    theme_ipsum() + 
    labs(title = plot_title,
         subtitle = 'Years 2020-2021',
         x = 'Number of deaths',
         y = 'Count') +
    ylim(c(0, 100)) +
    xlim(c(2000, 7050))
}

post_2020_l <- get_deaths_vector(post_2020)

get_post_2020_histogram(deaths_vector = post_2020_l, plot_title = "All-cause weekly deaths in Poland")

wilcox.test(pre_2020_l, post_2020_l)

# Kobiety, summary
pre_2020_female <- all_poland_female %>%
  filter(YEAR < 2020) %>%
  select(., T01:T53) 

post_2020_female <- all_poland_female %>%
  filter(YEAR >= 2020)%>%
  select(., T01:T53) 

pre_2020_l_female <- get_deaths_vector(pre_2020_female)

get_pre_2020_histogram(pre_2020_l_female, "All-cause weekly deaths in Poland, females")

post_2020_l_female <- get_deaths_vector(post_2020_female)

get_post_2020_histogram(post_2020_l_female, "All-cause weekly deaths in Poland, females")

hist(post_2020_l_female)

wilcox.test(pre_2020_l_female, post_2020_l_female)

#Mezczyzni, summary
pre_2020_male <- all_poland_male %>%
  filter(YEAR < 2020) %>%
  select(., T01:T53) 

post_2020_male <- all_poland_male %>%
  filter(YEAR >= 2020)%>%
  select(., T01:T53) 

pre_2020_l_male <- get_deaths_vector(pre_2020_male)

get_pre_2020_histogram(pre_2020_l_male, "All-cause weekly deaths in Poland, males")

hist(pre_2020_l_male)

post_2020_l_male <- get_deaths_vector(post_2020_male)

get_post_2020_histogram(post_2020_l_male, "All-cause weekly deaths in Poland, males")

hist(post_2020_l_male)

wilcox.test(pre_2020_l_male, post_2020_l_male)

length(post_2020_l)
length(pre_2020_l)

# Shapiro- Wilk tets

shapiro.test(pre_2020_l)
shapiro.test(pre_2020_l_female)
shapiro.test(pre_2020_l_male)

shapiro.test(post_2020_l)
shapiro.test(post_2020_l_female)
shapiro.test(post_2020_l_male)
