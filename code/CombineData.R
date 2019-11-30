## Load libraries used
library(readxl)
library(skimr)
library(janitor)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr) 
library(lubridate)
library(tidyverse)

path <- 'data/'

# Combine MetaHockey ------------------------------------------------------

mh_files <- paste0(path, list.files(path, pattern = 'MH_'))

## General cleaning function
clean_mh_data <- function(mh_data) {
  
  tidy_mh_data <- 
    mh_data %>%
    clean_names() %>%
    mutate(cap_hit = as.numeric(cap_hit)) %>%
    mutate(dob = date(parse_date_time(dob, orders = c('mdy', 'ymd'))))
  
  if (any(str_detect(names(tidy_mh_data), 'chip'))) {
    tidy_mh_data <- 
      tidy_mh_data %>%
      mutate(chip = as.numeric(chip))
  }
  
  if(any(str_detect(names(tidy_mh_data), 'dist'))) {
    tidy_mh_data <- 
      tidy_mh_data %>%
      mutate(dist = as.character(dist))
  }
  
  return(tidy_mh_data)
}


mh_szns <- paste0(12:17, '-', 13:18)

## Read/clean data and append to list
mh_data_list <- list()
for (i in 1:length(mh_files)) {
  mh_data_list[[i]] <- 
    read_excel(mh_files[i]) %>%
    clean_mh_data() %>%
    mutate(szn = mh_szns[i])
}

## Dataset specific cleaning
mh_data_list[[2]] <- 
  mh_data_list[[2]] %>% 
  rename(`gp` = gp_16, gs = gs_17, sa = sa_21, sv_percent = sv_percent_24)

mh_data_list[[3]] <- 
  mh_data_list[[3]] %>%
  rename(gp = gp_17)

## Combine data into one dataset
new_mh_data <- tibble()
for (i in 1:length(mh_data_list)) {
  new_mh_data <- bind_rows(new_mh_data, mh_data_list[[i]])
}

## Clean dataset
getblanks <- function(blanks) {
  
  blank <- sum(is.na(new_mh_data[blanks]))
  column_name <- colnames(new_mh_data[blanks])
  ans <- tibble(column_id = blanks, column_name = column_name,  NA_amount = blank)
  
  return(ans)
}

## Find number of blanks
response <- tibble()
for (i in 1:ncol(new_mh_data)) {
  response  <- bind_rows(response, getblanks(i))
}

## Remove column if filled with blanks (no match in other datasets)
for (y in 1:nrow(response)) {
  
  if (response[y, "NA_amount"] > 81) {
    
    name_column <- paste((response[y,"column_name"]))
    
    new_mh_data <- 
      new_mh_data %>% 
      select(-name_column)
  }
}

mh_data <- new_mh_data

# Combine HockeyReference -------------------------------------------------

hr_files <- paste0(path, list.files(path, pattern = 'HR_'))

num_of_sheets <- length(excel_sheets(hr_files))

hr_szns <- paste0(12:18, '-', 13:19)

# General cleaning function
clean_hr_data <- function(hr_data) {
  
  hr_data <- 
    hr_data %>%
    clean_names() %>%
    mutate(szn = hr_szns[i]) %>%
    mutate(player = str_remove(player, '\\\\[\\w]+')) %>%
    mutate(player = str_remove(player, '\\*')) %>%
    add_count(player, name = 'num_of_teams') %>%
    mutate(
      num_of_teams = ifelse(num_of_teams > 1, num_of_teams - 1, num_of_teams)
    )
  
  clean_data <-
    bind_rows(
      ## Traded players
      hr_data %>%
        filter(num_of_teams > 1) %>%
        filter(tm == 'TOT'),
      ## Players that played on the same team all szn
      hr_data %>%
        filter(num_of_teams == 1)
    ) %>%
    rename(team = tm) %>%
    arrange(rk)
  
  
  return(clean_data)
}

## Append data from each sheet in excel file to empty list and clean
hr_data_list <- list()
for (i in 1:num_of_sheets) {
  hr_data_list[[i]] <- 
    read_excel(hr_files, sheet = i, skip = 1) %>% 
    clean_hr_data()
}

## Combine data into on dataset
hr_data <- tibble()
for (i in 1:length(hr_data_list)) {
  hr_data <- 
    bind_rows(hr_data, hr_data_list[[i]])
}


# Combine MoneyPuck -------------------------------------------------------

mp_files <- paste0(path, list.files(path, pattern = 'MP_'))

mp_data_list <- list()
for(i in 1:length(mp_files)) {
  mp_data_list[[i]] <- 
    read_csv(mp_files[i]) %>%
      clean_names() %>%
      mutate(szn = hr_szns[i])
}

## Note: MoneyPuck data was perfectly clean (that we can tell) so cleaning was
## not needed before creating dataset

mp_data <- tibble()
for (i in 1:length(mp_data_list)) {
  mp_data <- bind_rows(mp_data, mp_data_list[[i]])
}

# Combine CapFriendly -------------------------------------------------

cf_files <- paste0(path, list.files(path, pattern = 'CF-'))

num_of_sheets <- length(excel_sheets(cf_files))

cf_szns <- excel_sheets(cf_files)

## Append data from each sheet in excel file to empty list
cf_data_list <- list()
for (i in 1:num_of_sheets) {
  cf_data_list[[i]] <- 
    read_excel(cf_files, sheet = i) %>% 
    clean_names() %>%
    mutate(szn = cf_szns[i])
}

## Small tidying
for (i in 1:length(cf_data_list)) {
  cf_data_list[[i]] <-
    cf_data_list[[i]] %>%
    # When a player dies(during the szn?), it denotes "Died" in Age column
    mutate(age = str_extract(age, '\\d+'),
           age = as.numeric(age)) %>%
    # The players column contains a numbered index (i.e. 1. Carey Price)
    mutate(player = str_remove(player, '[0-9]*\\.\\s*'))
}
  
## Combine data into on dataset
cf_data <- tibble()
for (i in 1:length(cf_data_list)) {
  cf_data <- 
    bind_rows(cf_data, cf_data_list[[i]]) 
}

cf_data <-
  cf_data %>%
    separate(drafted, into = c('overall', 'draft_round', 'draft_year'), sep = ' - ') %>%
    separate(draft_year, into = c('draft_year', 'draft_team'), sep = ' ') %>%
    mutate(overall = as.numeric(overall),
           draft_year = as.numeric(draft_year),
           draft_team = str_remove_all(draft_team, '[\\( \\)]'))
