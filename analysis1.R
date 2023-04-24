library("tidyverse")
library("stringr")

data_path <- '~/Documents/icm/magisterka/zgony/DK-2189-2022-UW-dane demograficzne/data'

years <- seq(2000, 2021)

file_name <- "OGOLEM.csv"

sub_folders <- list.dirs(data_path)[-1]

file_paths <- file.path(sub_folders, file_name)

ALL_DATA <- dplyr::tibble()
for (file in file_paths) {
  all_yearly <- read.csv(file, header = T, sep = ';')
  all_yearly['year'] <- word(file, -2, sep=fixed("/"))
  ALL_DATA <- dplyr::bind_rows(ALL_DATA, all_yearly)
}

ALL_DATA_SUMMARY <- ALL_DATA %>%
  dplyr::select(-ROK) %>% 
  dplyr::mutate(across(where(is.character), str_trim)) %>%
  dplyr::mutate(across(where(is.character), str_replace_all, pattern = "\\xa0",
                       replacement = "")) 

ALL_DATA_SUMMARY

numeric_cols <- colnames(ALL_DATA_SUMMARY)[4:57]
week_numbers <- numeric_cols[numeric_cols != "year"]

all_poland_summary <- ALL_DATA_SUMMARY %>%
  dplyr::filter(powiat_nazwa == 'Polska', wiek == 'Ogółem') %>% 
  dplyr::mutate(across(4:57, as.numeric)) %>%
  mutate('YEAR' = year) %>%
  select(-year)

all_poland_summary

# added on 18.04.2023 to use for age groups calcs
ALL_DATA_AGE_EUROSTAT <- ALL_DATA_SUMMARY %>%
  dplyr::mutate(across(4:57, as.numeric)) %>%
  mutate('YEAR' = year) %>%
  mutate(wiek_eurostat = paste0('Y',str_replace_all(wiek, " ", ""))) %>%
  mutate(wiek_eurostat = ifelse(wiek_eurostat == "YOgółem", "TOTAL",
                                ifelse(wiek_eurostat == "Y0-4", "Y_LT5",
                                       ifelse(wiek_eurostat %in% c("Y85-89", "Y90iwięcej"),
                                              "Y_GE85", wiek_eurostat)))) %>%

  select(-year)

ALL_DATA_AGE_EUROSTAT_FEMALE <- ALL_DATA_FEMALE %>%
  dplyr::mutate(across(4:57, as.numeric)) %>%
  mutate('YEAR' = year) %>%
  mutate(wiek_eurostat = paste0('Y',str_replace_all(wiek, " ", ""))) %>%
  mutate(wiek_eurostat = ifelse(wiek_eurostat == "YOgółem", "TOTAL",
                                ifelse(wiek_eurostat == "Y0-4", "Y_LT5",
                                       ifelse(wiek_eurostat %in% c("Y85-89", "Y90iwięcej"),
                                              "Y_GE85", wiek_eurostat)))) %>%
  
  select(-year)

ALL_DATA_AGE_EUROSTAT_MALE <- ALL_DATA_MALE %>%
  dplyr::mutate(across(4:57, as.numeric)) %>%
  mutate('YEAR' = year) %>%
  mutate(wiek_eurostat = paste0('Y',str_replace_all(wiek, " ", ""))) %>%
  mutate(wiek_eurostat = ifelse(wiek_eurostat == "YOgółem", "TOTAL",
                                ifelse(wiek_eurostat == "Y0-4", "Y_LT5",
                                       ifelse(wiek_eurostat %in% c("Y85-89", "Y90iwięcej"),
                                              "Y_GE85", wiek_eurostat)))) %>%
  select(-year)
  


