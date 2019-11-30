# Plots -------------------------------------------------------------------

## Contains plots for:
## - Distribution of contract value
## - Top 10 paid goalies in leagues
## - Contracts by annual value and length in years
## - Map of where goalies are from

## Load libraries
library(dplyr)
library(ggplot2)
library(teamcolors)
library(ggsci)

## Set theme for graphics
theme_set(theme_minimal())

## Plot average annual value of contracts over previous 7 szns
cf_data %>%
  filter(gp > 40) %>%
  mutate(szn = paste0(20, szn)) %>%
  ggplot(aes(szn, aav, fill = szn, color = szn)) +
  geom_violin(alpha = 0.8, show.legend = F, trim = T, adjust = 0.75, width = 1) +
  geom_boxplot(width = 0.1, color = 'grey80', fill = 'grey50', alpha = 0.1) +
  labs(x = NULL, y = 'Average annual value ($)') +
  ggtitle('Distribution of goalie average annual value') +
  ggsci::scale_color_futurama() +
  ggsci::scale_fill_futurama() +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_text(size = 12, color = 'grey25'),
        axis.text.y = element_text(size = 7, color = 'grey25'),
        axis.title.y = element_text(size = 9, color = 'grey25', face = 'italic'),
        plot.title = element_text(hjust = 0.5, size = 16, color = 'grey15', face= 'italic'))


## Get top 10 paid players in league over last 7 years
top_10_paid <- 
  cf_data %>%
    group_by(player) %>%
    top_n(1, aav) %>%
    ungroup() %>%
    distinct(player, aav, team) %>%
    top_n(10, aav)

## Plot top 10 paid players
top_10_paid %>%
  mutate(point_lab = paste0('$', as.integer(aav))) %>%
  mutate(team = substr(team, start = str_length(team) - 2, stop = str_length(team))) %>%
  mutate(player = paste0(player, '\n(', team, ')     ')) %>%
  left_join(teamcolors, by = c('team' = 'name')) %>%
  ggplot() +
  geom_segment(aes(x = reorder(player, aav), xend = player, y = 0, yend = aav), color = 'grey50') +
  geom_point(aes(x = player, y = aav, color = team), size = 5, show.legend = F) +
  labs(x = NULL, y = 'AAV($)') +
  ggtitle('Top 10 paid players in the league (as of 2018-19)') +
  coord_flip() +
  scale_color_futurama() +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.y = element_text(size = 10, color = 'grey25'),
        axis.text.x = element_text(size = 7, color = 'grey25'),
        axis.title.x = element_text(size = 9, color = 'grey25', face = 'italic'),
        plot.title = element_text(hjust = 0.5, size = 16, color = 'grey25', face= 'italic'))


## Plot contract by aav and length
cf_data %>%
  mutate(team = substr(team, start = 1, stop = str_length(team) - 3)) %>%
  filter(szn == '18-19', gp > 40) %>%
  left_join(teamcolors, by = c('team' = 'name')) %>%
  ggplot(aes(x = aav, y = length)) +
  geom_label(aes(label = player, fill = team, color = team), show.legend = F, 
             label.padding = unit(0.15, "lines"), size = 2.5, label.r = unit(0.1, "lines"), position = 'jitter') +
  labs(x = 'Average annual value ($)', y = 'Contract length (yrs)') +
  ggtitle('Goalie contracts based on annual value and length in years') +
  scale_fill_teams(which = 2) +
  scale_color_teams(which = 1) +
  theme_classic()

## Plot map of where goalies are from
goalie_countries <- 
  cf_data %>%
    count(country, name = "num_of_goalies") %>%
    na.omit()

world <- 
  ne_countries(
    scale = "medium", 
    returnclass = "sf", 
    continent = c('North America', 'Europe')
  )

world %>%
  left_join(goalie_countries, by = c('name' = 'country')) %>%
  ggplot() +
  geom_sf(aes(fill = num_of_goalies)) +
  ggtitle('Where are NHL goalies from?') +
  scale_fill_viridis(option = 'C', name = 'Number of goalies') +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.height = unit(2, 'mm'),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 8))


