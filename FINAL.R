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
#' Narysowac CMR zgonow oczekiwanych dla total dla powiatow dla lat 2020 w sumie. DONE
#' 
#' Krok 2: 3 grupy na podstawie rozkladu DONE
#' 
#' Krok 2A: tabelka jakie scenraiusze sa lepsze a jakie sa gorsze
#' 
#' Krok 3: Do grup z kroku 2 przyporzadkowujemy scenariusze, ktore juz mamy dla 
#' lat 2020 i 2021. DONE 
#' 
#' Krok 4: 
#' 4.1 Rzeczywistosc (2020 i 2021)
#' 4.2 Mapa oczekiwanych wskaznikow
#' 4.3 Mapa wynikowa pokazujaca te powiaty, w ktorych wplyw pandemii jest najwiekszy
#' 4.4 Mapa, w ktorej wplyw pandemii byl najslabszy
#' 
#################

# KROK 1 - CMR zgonow oczekiwanych total (nie ma Walbrzycha i walbrzyskiego)
#' 3 grupy na podstawie rozkladu osobno dla 2020 i 2021
POPULATION_TOTAL <- POPULATION_AGE_GROUPS %>%
  group_by(powiat_numer, year) %>%
  summarise(population = sum(population))

expected_cmr <- left_join(expected_acm, POPULATION_TOTAL, by = c('powiat_numer', 'year')) %>%
  mutate(expected_cmr = sum_expected/population * 100000)

# odfiltrowanie walbrzyskiego
expected_cmr <- expected_cmr %>%
  filter(powiat_numer != 221)

# 2020
expected_2020 <- expected_cmr %>%
  filter(year == 2020) 

# 2021
expected_2021 <- expected_cmr %>%
  filter(year == 2021) 

# calosc
hist(expected_2020$expected_cmr)
hist(expected_2021$expected_cmr)

# kwantylowy i gestosc 
library(car)
library("ggpubr")
qqPlot(expected_2020$expected_cmr)
qqPlot(expected_2021$expected_cmr)

ggdensity(expected_2020$expected_cmr, 
          main = "Density plot, 2020",
          xlab = "Expected cmr")
ggdensity(expected_2021$expected_cmr, 
          main = "Density plot, 2021",
          xlab = "Expected cmr")

# Sa z rozkladu normalnego
shapiro.test(expected_2020$expected_cmr)
shapiro.test(expected_2021$expected_cmr)

# kwantyle
q_25_2020 <- quantile(expected_2020$expected_cmr, 0.25)[[1]]
q_75_2020 <- quantile(expected_2020$expected_cmr, 0.75)[[1]]

q_25_2021 <- quantile(expected_2021$expected_cmr, 0.25)[[1]]
q_75_2021 <- quantile(expected_2021$expected_cmr, 0.75)[[1]]

# KROK 2: Pogrupowanie

get_group <- function(expected_cmr, q_25, q_75) {
  if (expected_cmr >= q_25 && expected_cmr <= q_75) { # interquantile range
    return(1)
  } else if (expected_cmr < q_25) {
    return(2)
  } else if (expected_cmr > q_75) {
    return(3)
  }
}

calculate_group <- function(row) {
  expected_cmr <- row['expected_cmr']
  current_year <- row['year']
  
  if (current_year == 2020) {
    group <- get_group(expected_cmr, q_25_2020, q_75_2020)
    return(group)
  } else {
    group <- get_group(expected_cmr, q_25_2021, q_75_2021)
    return(group)
  }
}

group_vector <- apply(expected_cmr[, c("expected_cmr", "year")], 
                      MARGIN = 1, FUN = calculate_group)

group_t <- tibble(group = group_vector)

## GRUPY
expected_group <- cbind(expected_cmr, group_t)

#### Połączenie z final tabelką, żeby mieć scenariusze dla grup
FINAL_GROUPS <- left_join(expected_group, FINAL, by=c('year', 'powiat_numer'))

#### ANALIZA
# Najgorzej
FINAL_GROUPS %>%
  filter(faze1 == 2 & faze2 == 3)

FINAL_GROUPS %>%
  filter(powiat_numer %in% c(464, 604, 605, 615))

all_cmr %>%
  filter(powiat_numer == 604)

# Najlepiej
FINAL_GROUPS %>%
  filter(faze1 == 3 & faze2 == 2) %>%
  select(powiat_numer, woj_numer, year, median_age, increase_point, median_age_group, faze1, faze2) %>%
  filter(year == 2021)

all_cmr %>%
  filter(powiat_numer == 3206)

test <- fitted_values_df %>%
  filter(powiat_numer == 3061)

plot_excess_tendency(test, 'Powiat test')

total_poviats %>%
  filter(powiat_numer == 3061) %>%
  filter(year %in% c(2019, 2020, 2021)) %>%
  group_by(year) %>%
  summarise(sum(deaths))

### GRUPY
#' expected cmr in [Q_25, Q75] => 1
#' expected_cmr < Q25 => 2
#' expected_cmr > Q75 => 3

###FAZA 1 - okreslenie polozenia mediany i x0
#' increase_point == median_age_group => 1
#' median_age_group > increas_point => 2
#' median_age_group < increase_point => 3
#' 
#' #### FAZA 2 - skosnosc paraboli
#' a in [q_25, q_75] => 1
#' a < q_25 => 2
#' a > q_75 => 3
#' 

# Obliczenia do tabelek
FINAL_GROUPS %>%
  group_by(year, group, faze1, faze2) %>%
  count() %>%
  filter(faze1 == 2) %>%
  filter(year == 2021)

### sprawdzenie
groups_desc <- tibble(group = c(1, 2, 3),
                      desc = c('expected_cmr in IQR',
                               'expected_cmr < Q25',
                               'expected_cmr > Q75'))
  
faze1_desc <- tibble(faze1 = c(1, 2, 3),
                     desc = c('median == X0',
                              'median > X0',
                              'median < X0'))
faze2_desc <- tibble(faze2 = c(1, 2, 3),
                     desc = c('a in IQR',
                              'a < Q25',
                              'a > Q75'))

group_count <- FINAL_GROUPS %>%
  group_by(year, group, faze1, faze2) %>%
  count()

group_count1 <- inner_join(group_count, groups_desc, by='group')
group_count2 <- inner_join(group_count1, faze1_desc, by='faze1')
group_count3<- inner_join(group_count2, faze2_desc, by = 'faze2')

group_count_final <- group_count3 %>%
  rename(group_desc = desc.x,
         faze1_desc = desc.y,
         faze2_desc = desc) 

group_count_final %>%
  filter(year == 2021) %>%
  filter(group == 2)
  print(n=25)
  
quantile(expected_2020$expected_cmr)

##### sprawdzenie c.d 
test <- fitted_values_df %>%
  filter(powiat_numer == 2817)

plot_excess_tendency(test, 'Powiat test')

FINAL_GROUPS %>%
filter(powiat_numer == 2817) %>%
select(increase_point)


FINAL_GROUPS %>%
  filter(a2 > 0) %>%
  filter(delta < 0) %>%
  print(n=25)
