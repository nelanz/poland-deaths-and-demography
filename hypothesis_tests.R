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

hist(pre_2020_l)

post_2020_l <- get_deaths_vector(post_2020)

hist(post_2020_l)

wilcox.test(pre_2020_l, post_2020_l)

# Kobiety, summary
pre_2020_female <- all_poland_female %>%
  filter(YEAR < 2020) %>%
  select(., T01:T53) 

post_2020_female <- all_poland_female %>%
  filter(YEAR >= 2020)%>%
  select(., T01:T53) 

pre_2020_l_female <- get_deaths_vector(pre_2020_female)

hist(pre_2020_l_female)

post_2020_l_female <- get_deaths_vector(post_2020_female)

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

hist(pre_2020_l_male)

post_2020_l_male <- get_deaths_vector(post_2020_male)

hist(post_2020_l_male)

wilcox.test(pre_2020_l_male, post_2020_l_male)

