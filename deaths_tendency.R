#### tendencja zgonow
POPULATION_AGE_GROUPS <- EXCESS_AG_TOTAL %>%
  select(powiat_numer, year, age_group, population) %>%
  distinct() %>%
  group_by(powiat_numer, age_group, year) %>%
  reframe(population = sum(population))


excess_wro <- EXCESS_AG_TOTAL %>%
  filter(name_normalized == 'Wrocław') %>%
  group_by(powiat_numer, year, age_group) %>%
  reframe(excess_sum = sum(excess_cmr))

age_group_normalized <- tibble(age_group = sort(unique(excess_wro$age_group)),
                                 age_group_normalized = c('85+', '0-14', '15-19', '20-24', 
                                                          '25-29', '30-34', '35-39', '40-44',
                                                          '45-49', '50-54', '55-59', '60-64',
                                                          '65-69', '70-74', '75-79', '80-84')) %>%
  mutate(age_group_id = as.numeric(as.factor(age_group_normalized)))

excess_wro_ag <- inner_join(excess_wro, age_group_normalized, by='age_group')

excess_wro_2020 <- excess_wro_ag %>%
  filter(year == 2020)

wro_plot <- ggplot(excess_wro_ag, aes(age_group_id, excess_sum, color=as.factor(year))) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F) +
  labs(title = 'Wroclaw, mortality tendency, years 2020 and 2021',
       x = 'Age group', y = 'Excess Crude Mortality Rate') +
  scale_x_discrete(labels = age_group_normalized$age_group_normalized)

wro_plot

wro_model <- lm(formula = excess_sum ~ poly(age_group_id, 2, raw=TRUE), data = excess_wro_2020)
wro_model$coefficients
summary(wro_model)
quadratic_roots(8, -98, 199)

wro_model$model

summary(wro_model)

plot(wro_model)

plot(wro_model$residuals)



####### 
# Regresja wielomianowa
# Puszczone na wszystkich powiatach

set.seed(42)

excess_sum <- EXCESS_AG_TOTAL %>%
  group_by(powiat_numer, year, age_group) %>%
  reframe(excess_sum = sum(excess_cmr))  %>% 
  mutate(number = as.numeric(as.factor(powiat_numer)))

excess_sum <- inner_join(excess_sum, age_group_normalized %>% select(-c(start, end)), by='age_group')

excess_sum %>%
  filter(year == 2020) %>%
  filter(powiat_numer == 221)
obs_number <- nrow(age_group_normalized)

poviats_numbers <- unique(sort(excess_sum$powiat_numer))

fitted_values_df <- tibble(powiat_numer = rep(poviats_numbers, each = 32),
                            year = rep(c(rep(2020, obs_number), rep(2021, obs_number)), times = length(poviats_numbers)),
                        fitted.values = NA, age_group_id = NA, excess_sum = NA, age_group = NA)

coeff_matrix_parent <- tibble()
for (i in 1:max(excess_sum$number)) {
  print(i)
  whichs_poviat <- which(excess_sum$number == i)
  temp <- excess_sum[whichs_poviat, ]
  curr_powiat <- unique(temp$powiat_numer)
  
  coeff_matrix <- tibble(year = c(2020, 2021), powiat_numer = curr_powiat, intercept = NA, a1 = NA, a2 = NA)
  
  for (y in c(2020, 2021)) {
    print(year)
    data <- temp %>% filter(year == as.numeric(y))
    model <- lm(formula = excess_sum ~ poly(age_group_id, 2, raw = TRUE), data = data)
    summary(model)
    
    which_preds <- which(fitted_values_df$year == y & fitted_values_df$powiat_numer == curr_powiat)
    # print(wro_model$fitted.values)
    
    which_coeff <- which(coeff_matrix$year == y)
    
    coeff_matrix[which_coeff, 'intercept'] <- model$coefficients[[1]]
    coeff_matrix[which_coeff, 'a1'] <- model$coefficients[[2]]
    coeff_matrix[which_coeff, 'a2'] <- model$coefficients[[3]]
    coeff_matrix[which_coeff, 'powiat_numer'] <- unique(data$powiat_numer)
    
    fitted_values_df[which_preds, 'fitted.values'] <- model$fitted.values
    fitted_values_df[which_preds, 'age_group_id'] <- data$age_group_id
    fitted_values_df[which_preds, 'excess_sum'] <- data$excess_sum
    fitted_values_df[which_preds, 'age_group'] <- data$age_group_normalized
  }
  
  coeff_matrix_parent <- rbind(coeff_matrix_parent, coeff_matrix)
}
summary(model)
fitted_values_df
length(coeff_matrix_parent$powiat_numer)

coeff_matrix_parent %>%
  arrange(powiat_numer)

# save(fitted_values_df, coeff_matrix_parent, file="../../deaths_tendency.RData")
# load(file="deaths_tendency.RData")

save(fitted_values_df, coeff_matrix_parent, file="../../deaths_tendency_raw_poly.RData")

excess_walbrzyski <- EXCESS_AG_TOTAL %>%
  filter(powiat_numer == 221) %>%
  # filter(age_group == 'Y_GE85') %>%
  group_by(age_group, year) %>%
  summarise(sum(deaths), sum(expected_acm))

walbrzyski_ag <- inner_join(excess_walbrzyski, age_group_normalized, by='age_group')

plot(walbrzyski_ag$age_group_id, walbrzyski_ag$`sum(deaths)`)
lines(walbrzyski_ag$age_group_id, walbrzyski_ag$`sum(expected_acm)`)

  ggplot(aes(aes(x = age_group_id, y=sum(deaths), color = as.factor(year)))) +
  geom_line()

hajnowska_test <- fitted_values_df %>%
  filter(powiat_numer == 2005)

wwa_test <- fitted_values_df %>%
  filter(powiat_numer == 1465)

test <- fitted_values_df %>%
  filter(powiat_numer == 226)

plot_excess_tendency <-  function(excess_fitted_df, powiat_name) {
  ggplot(excess_fitted_df, aes(x = age_group_id, y=fitted.values, color = as.factor(year))) +
    geom_line() + 
    geom_point(data = excess_fitted_df, aes(x = age_group_id, y=excess_sum, color = as.factor(year))) +
    labs(title = paste(powiat_name, ", excess crude mortality rate tendency in age groups"), color = "Year", x = 'Age group', y = 'Excess CMR') +
    # scale_x_continuous(breaks = sort(age_group_normalized$age_group_id), labels = sort(age_group_normalized$age_group_normalized)) +
    theme_light()  + 
    theme(axis.text = element_text(size = 12))
}

plot_excess_tendency(wwa_test, 'Warszawa')
plot_excess_tendency(hajnowska_test, 'Powiat hajnowski')
plot_excess_tendency(test, 'Powiat test')

coeff_matrix_parent %>%
  filter(powiat_numer == 2005)

# ggsave('wroclaw_excess_CMR_tend01.png')


### k-means dla wspolczynnika a2 - wspolczynnik przy x^2
CURRENT_YEAR = 2021

coeff_matrix_parent

coeff_year <- coeff_matrix_parent %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(intercept, a1)) %>%
  mutate(powiat_numer = as.character(powiat_numer))

# dolaczenie lozek
# sumaryczna populacja
population_year <- EXCESS_AG_TOTAL %>%
  select(powiat_numer, year, age_group, population) %>%
  distinct() %>%
  group_by(powiat_numer, year) %>%
  reframe(population = sum(population)) %>%
  mutate(powiat_numer = as.character(powiat_numer)) %>%
  filter(year == CURRENT_YEAR)

beds_YEAR <- beds %>% filter(year == CURRENT_YEAR) %>%
  mutate(powiat_numer = as.character(powiat_numer)) %>%
  select(-c(id, name, year)) 

beds_year_per_10000 <- inner_join(beds_YEAR, population_year, by=c("powiat_numer")) %>%
  mutate(beds_per_10000 = value/population * 10000) %>%
  select(-c('value', 'population'))

coeff_beds <- inner_join(coeff_year, beds_year_per_10000, by='powiat_numer') %>%
  rename(year = year.x) %>%
  select(-c(year.y))

###3 dolaczenie lekarzy

doctors_YEAR <- doctors %>% filter(year == CURRENT_YEAR) %>%
  mutate(powiat_numer = as.character(powiat_numer)) %>%
  rename(doctors_per_10000 = value) %>%
  select(-c(id, name, year)) 

#### polaczenie do kupy 
coeff_doc_years <- inner_join(coeff_beds, doctors_YEAR, by='powiat_numer') %>%
  select(-c(year))

coeff_doc_years_2 <- get_excess_df(coeff_doc_years) # TODO: sprawdzic ujemne

set.seed(42)
coeff_scale <- scale(coeff_doc_years_2)

# fviz_nbclust(coeff_doc_years_2, kmeans, method = "wss")

set.seed(42)
cluster <- kmeans(coeff_scale, centers = 3, nstart = 25)
print('Cluster model')
print(cluster)

set.seed(42)
fviz_cluster(cluster, data = coeff_scale, show.clust.cent = T) +
  labs(title = paste0(CURRENT_YEAR, ": a2 + beds per 10 000 + doctors per 10 000"))

ggsave('http://127.0.0.1:23715/graphics/832823e1-db5d-4ccb-aeb2-2c5989fd41c1.png','tendency_clusters_2020.png')

cluster$size

##### zapisanie
tendency_2020_model <- cluster
tendency_2020 <- coeff_doc_years_2 %>%
  mutate(Cluster = tendency_2020_model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)

# tendency_2020_model$size
# tendency_2020_model$cluster

tendency_2021_model <- cluster
tendency_2021 <- coeff_doc_years_2 %>%
  mutate(Cluster = tendency_2021_model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)

tendency_2021_model$size

coeff_doc_years_2 %>%
  mutate(Cluster = cluster$cluster)

comparison <- tibble(powiat_numer = rownames(coeff_doc_years_2),
                     year_2020 = tendency_2020_model$cluster,
                     year_2021 = tendency_2021_model$cluster)
tendency_2020_model
tendency_2021_model

write.csv(comparison, '../../tendency_comparison_2020_2021.csv')

hist(scale(coeff_doc_years_2$a2))

############### ALGORYTM ANETY
#######
#' Mediana wieku w powiatach

#' 1. wczytanie mediany wieku
dir <- '/Users/nelatoma/Documents/icm/magisterka/zgony/MAGISTERKA_OFFICIAL/population_data/other-measures/'
median_df <- read.csv(paste0(dir, 'age_median.csv')) %>%
  select(-c(id, name)) %>%
  rename(median_age = value)

#' Polaczenie mediany wieku z coefficients - JEST TU WALBRZYSKI DEBIL
#' 
coeff_matrix_parent_dist <- coeff_matrix_parent %>%
  distinct() %>%
  filter(!(powiat_numer == 221 & intercept < -5000))

median_coeff_df <- inner_join(coeff_matrix_parent_dist, median_df, by=c('year', 'powiat_numer'))

#' funkcja licząca rozwiązania funkcji kwadratowej
#' 

quadratic_roots <- function(a, b, c) {
  discriminant <- (b^2) - (4*a*c)
  
  if(discriminant < 0) {
    # Complex roots
    root1 <- (-b + sqrt(as.complex(discriminant))) / (2*a)
    root2 <- (-b - sqrt(as.complex(discriminant))) / (2*a)
    
    return(Re(c(discriminant,root1, root2)))
  } else if(discriminant > 0) {
    # Distinct real roots
    root1 <- (-b - sqrt(discriminant)) / (2*a)
    root2 <- (-b + sqrt(discriminant)) / (2*a)
    return(c(discriminant, root1, root2))
  } else {
    # Single real root
    root <- (-b) / (2*a)
    return(root)
  }
}


calculate_roots <- function(row) {
  a <- row["a2"]
  b <- row["a1"]
  c <- row["intercept"]
  roots <- quadratic_roots(a, b, c)
  
  if (length(roots) == 1) {
    return(c(0, roots, NA))
  } else {
    return(roots)
  }
}

roots_matrix <- t(apply(median_coeff_df[, c("intercept", "a1", "a2")], MARGIN = 1, FUN = calculate_roots))
roots_dataframe <- as.data.frame(roots_matrix)
colnames(roots_dataframe) <- c("delta", "root1", "root2")

#### ROOTS
median_coeff_root_df <- cbind(median_coeff_df, roots_dataframe)
median_coeff_root_df
### Jeśli delta < 0 -> znajdujemy Re - przypomniec sobie co to znaczy i znajdujemy minimum 
median_coeff_root_df %>%
filter(delta < 0)

test <- fitted_values_df %>%
  filter(powiat_numer == 207)

plot_excess_tendency(test, 'Powiat test')

find_minimum <- function(a, b, c, x_min = 1, x_max = 16) {
  # Define the quadratic function
  f <- function(x) {
    a * x^2 + b * x + c
  }
  
  # Calculate the x-coordinate of the vertex
  vertex_x <- -b / (2 * a)
  
  # Check if the vertex_x is within the given range
  if (vertex_x >= x_min && vertex_x <= x_max) {
    # Calculate the y-coordinate of the vertex (minimum value)
    vertex_y <- f(vertex_x)
    return(vertex_x)
  } else {
    # If the vertex_x is outside the range, find the minimum value at the endpoints
    min_y <- min(f(x_min), f(x_max))
    x_value <- ifelse(f(x_min) == min_y, x_min, x_max)
    return(x_value)
  }
}

calculate_minimum <- function(row) {
  a <- row["a2"]
  b <- row["a1"]
  c <- row["intercept"]
  delta <- row["delta"]
  
  if (delta < 0) {
    minimum <- find_minimum(a, b, c)
    return (minimum)
  }
  else {
    return(NA)
  }
}

min_matrix <- t(apply(median_coeff_root_df[, c("intercept", "a1", "a2", "delta")], MARGIN = 1, FUN = calculate_minimum))

# minimum dla ujemnej delty, zebu wiedziec odkad rosnie
median_root_min_df <- median_coeff_root_df %>%
  mutate(minimum = as.vector(min_matrix))

### SCENARIUSZ 1 - porównanie grupy wiekowej i mediany
find_increase_point <- function(root1, root2, a2, minimum) {
  max_root <- max(root1, root2)
  
  if (!is.na(minimum)) {
    return(round(minimum))
  }
  
  if (max_root > 16) {
    return(round(min(root1, root2)))
  } 
  if (a2 < 0 && root1 >= 1) {
    return(root1)
  } else {
    return(max_root)
  }
}

calculate_increase_point <- function(row) {
  root1 <- row["root1"]
  root2 <- row["root2"]
  a2 <- row['a2']
  minimum <- row["minimum"]
  point <- find_increase_point(root1, root2, a2, minimum)
  
  return(point)
}

increase_vector <- apply(median_root_min_df[, c("root1", "root2", "a2", "minimum")], MARGIN = 1, FUN = calculate_increase_point)

## punkt wzrostu
df1 <- median_root_min_df %>%
  mutate(increase_point = round(increase_vector))
df1

## przetlumaczenie mediany na grupe wiekowa
get_age_group_scope <- function(age_group) {
  age_grup_scope <- c()
  if (age_group == '85+') {
    age_group_scope <- c(85, NA)
    return (age_group_scope)
  }
  else {
    age_group_scope <- as.numeric(strsplit(age_group, '-')[[1]])
    return (age_group_scope)
  }
}

calculate_age_group_scope <- function(row) {
  age_group <- row['age_group_normalized']
  age_group_scope <- get_age_group_scope(age_group)
  
  return(age_group_scope)
}

age_group_normalized
age_group_scope_matrix <- t(apply(age_group_normalized[, "age_group_normalized"], MARGIN = 1, FUN = calculate_age_group_scope))
age_group_dataframe <- as.data.frame(age_group_scope_matrix)
colnames(age_group_dataframe) <- c("start", "end")

# to chyba bylo niepotrzebne
age_group_normalized <- cbind(age_group_normalized, age_group_dataframe)

get_age_group <- function(age) {
  age <- round(age)
  if (age >= 35 && age <= 39) {
    age_group <- 6
    return (age_group)
  } else if (age >= 40 && age <= 44) {
    age_group <- 7
    return (age_group)
  } else if (age >= 45 && age <= 49) {
    age_group <- 8
    return (age_group)
  }
}


median_age_group <- c()

for (i in df1$median_age) {
  current_median_group <- get_age_group(i)
  median_age_group <- c(median_age_group, current_median_group)
}

### tablica z grupa na podstawie mediany

df2 <- df1 %>%
  mutate(median_age_group = median_age_group)
df2$powiat_numer

### FAZA 1 - okreslenie polozenia mediany i x0
#' increase_point == median_age_group => 1
#' median_age_group > increas_point => 2
#' median_age_group < increase_point => 3
#' 

get_faze_1 <- function(median_age_group, increase_point) {
  if (increase_point == median_age_group) {
    return(1)
  } else if (median_age_group > increase_point) {
    return(2)
  } else if (median_age_group < increase_point) {
    return (3)
  }
}

calculate_faze_1 <- function(row) {
  increase_point <- row['increase_point']
  median_age_group <- row['median_age_group']
  
  faze_1_classification <- get_faze_1(median_age_group, increase_point)
  return(faze_1_classification)
}

faze_1_vector <- apply(df2[, c("increase_point", "median_age_group")], MARGIN = 1, FUN = calculate_faze_1)
table(faze_1_vector)

##### dolaczenie fazy 1 do tabeli
df3 <- df2 %>%
  mutate(faze1 = faze_1_vector)

df3$powiat_numer

#### FAZA 2 - skosnosc paraboli
#' a in [q_25, q_75] => 1
#' a < q_25 => 2
#' a > q_75 => 3
#' 
# WYWALAMY WALBRZYSKI, COS Z NIM NIE TAK
# test <- fitted_values_df %>%
#   filter(powiat_numer == 221)
# 
# plot_excess_tendency(test, 'Powiat test')

df3 <- df3 %>%
  filter(powiat_numer != 221)

df3_2020 <- df3%>%
  filter(year == 2020)

df3_2021 <- df3%>%
  filter(year == 2021)

hist(df3_2020$a2)
hist(df3_2021$a2)

shapiro.test(df3_2021$a2) # wspolczynniki w przyblizeniu normalne

q_25_2020 <- quantile(df3_2020$a2, 0.25)[[1]]
q_75_2020 <- quantile(df3_2020$a2, 0.75)[[1]]

q_25_2021 <- quantile(df3_2021$a2, 0.25)[[1]]
q_75_2021 <- quantile(df3_2021$a2, 0.75)[[1]]

get_steepnes <- function(a, q_25, q_75) {
  if (a >= q_25 && a <= q_75) { # interquantile range
    return(1)
  } else if (a < q_25) {
    return(2)
  } else if (a > q_75) {
    return(3)
  }
}

calculate_steepness <- function(row) {
  a <- row['a2']
  current_year <- row['year']
  
  if (current_year == 2020) {
    steepness <- get_steepnes(a, q_25_2020, q_75_2020)
    return(steepness)
  } else {
    steepness <- get_steepnes(a, q_25_2021, q_75_2021)
    return(steepness)
  }
}

steepness_vector <- apply(df3[, c("a2", "year")], MARGIN = 1, FUN = calculate_steepness)

table(steepness_vector)

###### TABELA Z OBYDWOMA FAZAMI
FINAL <- df3 %>%
  mutate(faze2 = steepness_vector) %>%
  distinct()

FINAL %>%
  filter(powiat_numer == 225)

FINAL %>%
  filter(faze1 == 3 & faze2 == 3)

### TODO: wskaznik zgonow wygenerowanych przez model (oczekiwanych)
all_cmr

#' rozklad oczkiwanych zgonow z modelu - total
#' czy powiaty w ktorych wystapily najwyzsze wskazniki zgonow oczekiwanych, 
#' sa tymi, w ktorych wystąpiły 

#' Model = zgony oczekiwane
#' zgony nadmiarowe = zgony rzeczywiste (GUS) - zgony oczekiwane (Modelu)
#' 
#' 2020 porownanie z 2021:
#' 1. Jesli faza1 == faza1 to patrzymy czy faza 2 jest bardziej stroma (gorzej) czy
#' zostaje taka sama czy jest lagodniejsza (lepiej)
#' 2. Jesli faza1 w 2020 != faza1 w 2021 to patrzymy w ktora strone jest zmiana,
#'  jesli 2 to jest pogorszenie, jesli 3 to polepszenie (nie umieraja mlodzi)
#'  
#' TODO: tabela, ruch w ktora strone jest lepszy
#' 
#' 
#' Porównanie modelu zgonow oczekiwanych total (bez pandemii) 2020, 2021
#' Pytanie 1: jaki jest rozklad wskaznika zgonow total? Bo potrzebujemy miec 3 grupy powiatow:
#' Grupa 1: powiaty, które mają przeciętny wskaźnik zgonow total (gdzies na srodku)
#' Grupa 2: powiaty, ktore maja wysoki wskaznik zgonow (gdzies na prawo)
#' Grupa 3: powiaty, ktore maja niski wskaznik zgonow (gdzies na lewo)
#' 
#' Pytanie: do ktorej grupy z 3 powyzej beda wpadaly te powiaty, w ktorych
#' otrzymalysmy najgorszy scenariusz zgonow nadmiarowych w kroku poprzednim?
#' 
#' Jezeli w pandemii umieraja mlodzi i starzy i jest to intensywne (x0 < mediana,
#' wysokie a) => to itensywny wplyw pandemii na strukture demigraficzna. Przyspieszenie
#' procesu jesli x0 >= medianie i a okresla intensywnosc. Jesli a jest strome
#' to jest przyspieszenie zgonow ludzi starych, jesli a jest wyplaszczone, czy IQR
#' to jest to oczekiwane. 
#' Grupa 3: mediana < x0 i a przecietne albo plaskie to jest to relacja bliska dynamice
#' naturalnej. Jesli a jest jest wysokie, to przyspieszenie w grupach starszych
#' Najlepszy mozliwy scenariusz: mediana < x0 i a w srodku albo nisko - pandemia 
#' ma najmniejszy wplyw na demografie
#' 
#' W jaki sposob do 3 grup wpada 9 scenariuszy?
#' 
#' 
#' Krok 1:
#' Narysowac CMR zgonow oczekiwanych dla total dla powiatow dla lat 2020 w sumie.
#' 
#' Krok 2: 3 grupy na podstawie rozkladu
#' 
#' Krok 2A: tabelka jakie scenraiusze sa lepsze a jakie sa gorsze
#' 
#' Krok 3: Do grup z kroku 2 przyporzadkowujemy scenariusze, ktore juz mamy dla 
#' lat 2020 i 2021.
#' 
#' Krok 4: 
#' 4.1 Rzeczywistosc (2020 i 2021)
#' 4.2 Mapa oczekiwanych wskaznikow
#' 4.3 Mapa wynikowa pokazujaca te powiaty, w ktorych wplyw pandemii jest najwiekszy
#' 4.4 Mapa, w ktorej wplyw pandemii byl najslabszy
#' 






##### klastry z uwzglednieniem faz - BEZ SENSU
# CURRENT_YEAR = 2020
# 
# cluster_data <- FINAL %>%
#   filter(year == CURRENT_YEAR) %>%
#   select(powiat_numer, faze1, faze2) %>%
#   distinct()
# 
# cluster_df <- get_excess_df(cluster_data)
# get_clusters(cluster_df, 'title')


