# Multiple Regression -----------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(FSelector)


## Prep and combine data

cf_reg_data <- 
  cf_data %>%
  mutate(
    ## Removes accents off names
    player = stri_trans_general(player, 'latin-ascii'),
    ## Transform all names to lowercase
    player = str_to_lower(player)) %>%
  ## Remove redundant cols
  select(-weight, -height, -pos, -team, -age, -gp, -w, -l, -so, -gaa, -sv_percent)

hr_reg_data <-
  hr_data %>%
  ## Transform all names to lowercase
  mutate(player = str_to_lower(player)) %>%
  # Filter for post lockout szns
  filter(szn != '12-13') %>%
  # Fix nicknames
  mutate(player = case_when(
    str_detect(player, 'cal heeter') ~ str_replace(player, 'cal heeter', 'calvin heeter'),
    str_detect(player, 'matt o\'connor') ~ str_replace(player, 'matt o\'connor', 'matthew o\'connor'),
    str_detect(player, 'eddie pasquale') ~ str_replace(player, 'eddie pasquale', 'edward pasquale'),
    TRUE ~ as.character(player)
  ))

mp_reg_data <- 
  mp_data %>%
  # Filter data for all situations (5v5, player-down, player-up)
  filter(situation == 'all', szn != '12-13') %>%
  # Remove redundant cols
  select(-player_id, -team, -season, -position, -games_played, 
         -penality_minutes, -penalties, -situation, -goals) %>%
  # Transform all names to lowercase
  mutate(name = str_to_lower(name)) %>%
  # Fix nicknames
  mutate(name = case_when(
    str_detect(name, 'tom mccollum') ~ str_replace(name, 'tom mccollum', 'thomas mccollum'),
    str_detect(name, 'j.f. berube') ~ str_replace(name, 'j.f. berube', 'jean-francois berube'),
    str_detect(name, 'j-f berube') ~ str_replace(name, 'j-f berube', 'jean-francois berube'),
    str_detect(name, 'cal petersen') ~ str_replace(name, 'cal petersen', 'calvin petersen'),
    TRUE ~ as.character(name)
  ))

# Join all data together
joined_data <-
  hr_reg_data %>%
  full_join(mp_reg_data, by = c('player' = 'name', 'szn' = 'szn')) %>%
  inner_join(cf_reg_data, by = c('player', 'szn')) %>%
  # Remove extra contract info (since we don't "know" this information yet)
  select(-rk, -cap_hit_percent, -salary, -length, -cap_hit) 

reg_data <- 
  joined_data %>%
  # Code binary variables
  mutate(handed = ifelse(handed == 'Left', 0, 1),
         expiry = ifelse(expiry == 'UFA', 0, 1)) %>%
  # Select numeric predictors
  select_if(is.numeric)

## Feature selection
info_gain <- information.gain(formula = aav ~ ., data = reg_data)
subset_info_gain <- cutoff.k(info_gain, k = 10)

feat_selected <- 
  reg_data %>% 
    select(subset_info_gain, aav)

## Model
mod <- lm(aav~., feat_selected)

## Predictions
new_data1 <- 
  joined_data %>%
  filter(player == 'sergei bobrovsky', szn == '18-19') %>%
  select(subset_info_gain, aav)

predict(mod, new_data1)

new_data2 <-
  joined_data %>%
  filter(player == 'mikko koskinen') %>%
  select(subset_info_gain, aav)

predict(mod, new_data2)