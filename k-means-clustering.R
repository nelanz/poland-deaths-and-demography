all_excess_age_groups
female_excess_age_groups
male_excess_age_groups
all_excess_df

check_results <- function(excess_df) {
  excess_df %>%
    group_by(age_group, year) %>%
    summarise(sum(deaths), sum(expected_acm), sum(excess), sum(expected_acm_se)) %>%
    print(n=36)
}

check_results(all_excess_age_groups)
check_results(female_excess_age_groups)

male_excess_age_groups %>%
  filter(name_normalized_x == 'Warszawa') %>%
  filter(age_group == 'Y_GE85') %>%
  group_by(year) %>%
  summarise(sum(deaths), sum(expected_acm), sum(excess))
#######################################
#### data prep
#' 1. change columns so they are easier ot join
#' 2. Add Walbrzych 
#' 2. Replace kids with Y_LT14
excess_walbrzych_age_groups_total %>%
  filter(age_group == 'Y_LT14') %>%
  print(n=105)
 # 1.
excess_all <- all_excess_age_groups %>%
  rename(gender = gender.x,
         number = number.x,
         name_normalized = name_normalized_x,
         excess_cmr = excess_CMR) %>%
  select(-c(gender.y, number.y, name_normalized_y, X)) %>%
  filter(!(age_group %in% c('Y_LT5', 'Y5-9', 'Y10-14'))) %>%
  filter(name_normalized != 'wałbrzyski')

unique(excess_all$X)
# 2.
excess_walbrzych <- excess_walbrzych_age_groups_total %>%
  mutate(number = 380) %>%
  rename(powiat_numer = powiat_numer.x) %>%
  select(-c(powiat_numer.y)) %>%
  mutate(date = rep(unique(excess_all$date), length(unique(excess_walbrzych_age_groups_total$age_group)))) %>%
  mutate(CMR = deaths/population * 100000) %>%
  mutate(name_normalized = 'Wałbrzych') %>%
  mutate(excess_cmr = excess_cmr * 100000)

date_df <- tibble(week = c(seq(1, 53), seq(1, 52)), date = unique(excess_all$date))

# 3.
excess_kids <-  all_excess_kids %>%
  rename(name_normalized = name_normalized_x) %>%
  select(-c(age_group_normalized)) %>%
  group_by(powiat_numer) %>%
  mutate(date = date_df$date) %>%
  mutate(CMR = deaths/population * 100000)

# 4. powiat_wałbrzyski
excess_walbrzyski <- excess_walbrzyski_age_groups_total %>%
  mutate(number = 381) %>%
  rename(powiat_numer = powiat_numer.x) %>%
  select(-c(powiat_numer.y)) %>%
  mutate(date = rep(unique(excess_all$date), length(unique(excess_walbrzyski_age_groups_total$age_group)))) %>%
  mutate(CMR = deaths/population * 100000) %>%
  mutate(name_normalized = 'wałbrzyski') %>%
  mutate(excess_cmr = excess_cmr * 100000)

excess_walbrzyski %>%
  group_by(year) %>%
  summarise(sum(excess))

EXCESS_AG_TOTAL <- bind_rows(excess_all, excess_walbrzych, excess_kids, excess_walbrzyski)

unique(EXCESS_AG_TOTAL$gender)

EXCESS_AG_TOTAL %>%
  group_by(age_group, year) %>%
  summarise(sum(deaths), sum(excess), sum(expected_acm)) %>%
  print(n=32)

## import additional data sources form GUS
dir <- '/Users/nelatoma/Documents/icm/magisterka/zgony/MAGISTERKA_OFFICIAL/population_data/other-measures/'
E0 <- read.csv(paste0(dir, 'E0.csv'))
HLY <- read.csv(paste0(dir, 'HLY.csv'))
beds <- read.csv(paste0(dir, 'beds.csv'))
doctors <- read.csv(paste0(dir, 'doctors_per_1000.csv'))
health_percentage <- read.csv(paste0(dir, 'health_percentage.csv'))
hospitals <- read.csv(paste0(dir, 'hospitals.csv'))


HLY %>%
  group_by(year, gender) %>%
  summarise(mean(value)) %>%
  filter(year %in% c(2020, 2021))

E0 %>%
  group_by(year, gender) %>%
  summarise(mean(value)) %>%
  filter(year %in% c(2020, 2021))

##################################

#### kmeans
library(cluster)
library(factoextra)

### 2020

all_excess_pivot <- EXCESS_AG_TOTAL %>%
  group_by(powiat_numer,  age_group, year) %>%
  reframe(population = first(population), excess_sum_CMR = sum(excess)/first(population) * 100000) %>%
  filter(year == 2020) %>%
  select(-c('year', 'population')) %>%
  spread(key = age_group, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

all_excess_pivot_df <- as.data.frame(all_excess_pivot)

rownames(all_excess_pivot_df) <- all_excess_pivot_df$powiat_numer
all_excess_pivot_df$powiat_numer <- NULL

all_excess_pivot_scale <- scale(all_excess_pivot_df)

distance <- get_dist(all_excess_pivot_scale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

clusters <- kmeans(all_excess_pivot_scale, centers = 3, nstart = 25)
str(clusters)
clusters

all_excess_pivot_df %>%
  mutate(Cluster = clusters$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)


fviz_cluster(clusters, data = all_excess_pivot_scale, show.clust.cent = T, repel = T, ggtheme = theme_ipsum())

fviz_nbclust(all_excess_pivot_df, kmeans, method = "silhouette")

gap_stat <- clusGap(all_excess_pivot_df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

### 2021
all_excess_pivot2 <- EXCESS_AG_TOTAL %>%
  group_by(powiat_numer, age_group, year) %>%
  reframe(population = first(population), excess_sum_CMR = sum(excess)/first(population) * 100000) %>%
  filter(year == 2021) %>%
  select(-c('year', 'population')) %>%
  spread(key = age_group, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

all_excess_pivot_df2 <- as.data.frame(all_excess_pivot2)

rownames(all_excess_pivot_df2) <- all_excess_pivot_df2$powiat_numer
all_excess_pivot_df2$powiat_numer <- NULL

all_excess_pivot_scale2 <- scale(all_excess_pivot_df2)

# distance <- get_dist(all_excess_pivot_scale2)
# fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

clusters2 <- kmeans(all_excess_pivot_scale2, centers = 3, nstart = 25)
str(clusters2)
clusters2

fviz_cluster(clusters2, data = all_excess_pivot_scale2, show.clust.cent = T, repel = T, ggtheme = theme_ipsum())


#### Polepszenie wynikow
#' Pomysl 1: Wybranie grup 60+ (samych)
#' 
#' 

CURRENT_YEAR <-  2021
set.seed(42)

dziadki <- EXCESS_AG_TOTAL %>%
  filter(age_group %in% c('Y75-79', 'Y80-84', 'Y_GE85')) %>%
  group_by(powiat_numer, age_group, year) %>%
  reframe(population = first(population), excess_sum_CMR = sum(excess)/first(population) * 100000) %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c('year', 'population')) %>%
  spread(key = age_group, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

dziadki_df <- as.data.frame(dziadki)
rownames(dziadki_df) <- dziadki_df$powiat_numer
dziadki_df$powiat_numer <- NULL
dziadki_df

fviz_nbclust(dziadki_df, kmeans, method = "wss")

dziadki_scale <- scale(dziadki_df)

dziadki_cluster <- kmeans(dziadki_scale, centers = 3, nstart = 25)
dziadki_cluster

fviz_cluster(dziadki_cluster, data = dziadki_scale, show.clust.cent = T) +
  labs(title = "Total 2020: Y_GE85 + Y75-79 + Y80-84")

dziadki_df %>%
  mutate(Cluster = dziadki_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)

#### Polepszenie wynikow
#' Pomysl 2: zwiekszenie grup do grup 10 letnich poza mlodymy, ktorzy sa od 0-24
#' 

POPULATION_AGE_GROUPS_2 <- EXCESS_AG_TOTAL %>%
  select(powiat_numer, year, age_group, population) %>%
  distinct() %>%
  mutate(age_group_2 = ifelse(age_group %in% c('Y_LT14', 'Y15-19', 'Y20-24'), 'Y0-24',
                              ifelse(age_group %in% c('Y25-29', 'Y30-34'), 'Y25-34',
                                     ifelse(age_group %in% c('Y35-39', 'Y40-44'), 'Y35-44',
                                            ifelse(age_group %in% c('Y45-49', 'Y50-54'), 'Y45-54',
                                                   ifelse(age_group %in% c('Y55-59', 'Y60-64'), 'Y55-64',
                                                          ifelse(age_group %in% c('Y65-69', 'Y70-74'), 'Y65-74',
                                                                 ifelse(age_group %in% c('Y75-79', 'Y80-84'), 'Y75-84', 'Y_85+')))))))) %>%
  group_by(powiat_numer, age_group_2, year) %>%
  reframe(population = sum(population))

# 
# EXCESS_AG_TOTAL %>%
#   select(powiat_numer, year, age_group, population) %>%
#   distinct() %>%
#   filter(powiat_numer == 201) %>%
#   print(n = 32)


temp <- EXCESS_AG_TOTAL %>%
  mutate(age_group_2 = ifelse(age_group %in% c('Y_LT14', 'Y15-19', 'Y20-24'), 'Y0-24',
                              ifelse(age_group %in% c('Y25-29', 'Y30-34'), 'Y25-34',
                                     ifelse(age_group %in% c('Y35-39', 'Y40-44'), 'Y35-44',
                                            ifelse(age_group %in% c('Y45-49', 'Y50-54'), 'Y45-54',
                                                   ifelse(age_group %in% c('Y55-59', 'Y60-64'), 'Y55-64',
                                                          ifelse(age_group %in% c('Y65-69', 'Y70-74'), 'Y65-74',
                                                                 ifelse(age_group %in% c('Y75-79', 'Y80-84'), 'Y75-84', 'Y_85+')))))))) %>%
  group_by(powiat_numer, age_group_2, year) %>%
  reframe(excess = sum(excess))

excess_ag_2 <- inner_join(temp, POPULATION_AGE_GROUPS_2, by=c('powiat_numer', 'year', 'age_group_2')) %>%
  mutate(excess_sum_CMR = excess/population * 100000) 

excess_ag_2_pivot <- excess_ag_2 %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year, excess, population)) %>%
  spread(key = age_group_2, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

excess_ag_2_pivot_df <- as.data.frame(excess_ag_2_pivot)
rownames(excess_ag_2_pivot_df) <- excess_ag_2_pivot_df$powiat_numer
excess_ag_2_pivot_df$powiat_numer <- NULL
excess_ag_2_pivot_df

# fviz_nbclust(excess_ag_2_pivot_df, kmeans, method = "wss")

excess_ag_2_scale <- scale(excess_ag_2_pivot_df)

set.seed(42)

excess_ag_2_cluster <- kmeans(excess_ag_2_scale, centers = 3, nstart = 25)
excess_ag_2_cluster$size

fviz_cluster(excess_ag_2_cluster, data = excess_ag_2_scale, show.clust.cent = T) +
  labs(title = paste("Total", CURRENT_YEAR, ": Y0-24 + 10 year age groups"))

excess_ag_2_pivot_df %>%
  mutate(Cluster = excess_ag_2_cluster$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(mean)


#' Pomysl 3: do tego co wyzej dorzucic oczekiwana dl zycia w zdrowiu
#' 
HLY <- HLY %>%
  mutate(woj_numer = as.character(woj_numer)) %>%
  rename(HLY = value) %>%
  filter(year >= 2020) %>%
  group_by(woj_numer, year) %>%
  reframe(mean_HLY = mean(HLY))

HLY_YEAR <- HLY %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year))

excess_ag_2_woj <- excess_ag_2_pivot %>%
  mutate(woj_numer = ifelse(nchar(powiat_numer) > 3, substr(powiat_numer, 1, 2), substr(powiat_numer, 1, 1)))


excess_HLY <- inner_join(excess_ag_2_woj, HLY_YEAR, by=c('woj_numer'), relationship = "many-to-many") 

get_excess_df <- function(excess_tibble) {
  df <- as.data.frame(excess_tibble)
  rownames(df) <- df$powiat_numer
  df$powiat_numer <- NULL
  return(df)
}

excess_HLY_df <- get_excess_df(excess_HLY) %>%
  select(-c(woj_numer))


get_clusters <- function(excess_df, plot_title) {
  set.seed(42)
  df_scale <- scale(excess_df)
  
  cluster <- kmeans(df_scale, centers = 3, nstart = 25)
  print('Cluster model')
  print(cluster)
  
  print('Cluster groups:')
  group <- excess_df %>%
    mutate(Cluster = cluster$cluster) %>%
    group_by(Cluster) %>%
    summarise_all(mean)
  
  print(group)
  
  return(fviz_cluster(cluster, data = df_scale, show.clust.cent = T, ellipse = T) +
           labs(title = plot_title))
}

get_clusters(excess_HLY_df, plot_title = paste(CURRENT_YEAR, ': excess + HLY (age groups 2)'))


#' Pomysl 4: dolozenie E0 (oczekiwanej dlugosci zycia)

E0 <- E0 %>%
  mutate(woj_numer = as.character(woj_numer)) %>%
  rename(E0 = value) %>%
  filter(year >= 2020) %>%
  group_by(woj_numer, year) %>%
  reframe(mean_E0 = mean(E0))

E0_YEAR <- E0 %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year))

excess_ag_2_pivot <- excess_ag_2 %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year, excess, population)) %>%
  spread(key = age_group_2, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

excess_ag_2_woj <- excess_ag_2_pivot %>%
  mutate(woj_numer = ifelse(nchar(powiat_numer) > 3, substr(powiat_numer, 1, 2), substr(powiat_numer, 1, 1)))

excess_E0 <- inner_join(excess_ag_2_woj, E0_YEAR, by=c('woj_numer'), relationship = "many-to-many") %>% select(-c(woj_numer))

excess_E0_df <- get_excess_df(excess_E0) 

get_clusters(excess_E0_df, plot_title = paste(CURRENT_YEAR, ': excess + E0 (age groups 2)'))

#' Pomysl 5: lozka
#' 
beds <- read.csv(paste0(dir, 'beds.csv'))
beds <- beds %>% select(-c(id, name))

population_year <- POPULATION_AGE_GROUPS_2 %>%
  filter(year == CURRENT_YEAR) %>%
  group_by(powiat_numer) %>%
  reframe(population = sum(population)) %>%
  mutate(powiat_numer = as.character(powiat_numer))

beds_YEAR <- beds %>% filter(year == CURRENT_YEAR) %>%
   mutate(powiat_numer = as.character(powiat_numer)) 

beds_year_per_1000 <- inner_join(beds_YEAR, population_year, by=c("powiat_numer")) %>%
  mutate(beds_per_1000 = value/population * 1000)

excess_ag_2_pivot <- excess_ag_2 %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year, excess, population)) %>%
  spread(key = age_group_2, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

excess_beds <- inner_join(excess_ag_2_pivot, beds_year_per_1000, by=c("powiat_numer")) %>%
  select(-c(year, value, population))

excess_beds_df <- get_excess_df(excess_beds)
get_clusters(excess_beds_df,  plot_title = paste(CURRENT_YEAR, ': excess + beds (age groups 2)'))

#' Pomysl 7: lekarze
CURRENT_YEAR <-  2020
doctors <- read.csv(paste0(dir, 'doctors_per_1000.csv'))
doctors <- doctors %>% select(-c(id, name))

population_year <- POPULATION_AGE_GROUPS_2 %>%
  filter(year == CURRENT_YEAR) %>%
  group_by(powiat_numer) %>%
  reframe(population = sum(population)) %>%
  mutate(powiat_numer = as.character(powiat_numer))

doctors_YEAR <- doctors %>% filter(year == CURRENT_YEAR) %>%
  mutate(powiat_numer = as.character(powiat_numer)) 

excess_ag_2_pivot <- excess_ag_2 %>%
  filter(year == CURRENT_YEAR) %>%
  select(-c(year, excess, population)) %>%
  spread(key = age_group_2, value = excess_sum_CMR) %>%
  mutate(powiat_numer = as.character(powiat_numer))

excess_doctors <- inner_join(excess_ag_2_pivot, doctors_YEAR, by=c("powiat_numer")) %>%
  rename(doctors_per_10000 = value) %>%
  select(-c(year))

excess_doctors_df <- get_excess_df(excess_doctors)
get_clusters(excess_doctors_df,  plot_title = paste(CURRENT_YEAR, ': excess + doctors per 10000 (age groups 2)'))

