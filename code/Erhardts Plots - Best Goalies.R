# plots for the thing 

#Best Goalies by averages overall 

library(ggrepel)
library(patchwork)


hr_data <- hr_data %>% mutate(w_percent = w/gp)

Salaries <- cf_data %>% group_by(player) %>% 
  summarise(mean_aav = mean(aav)) %>% 
  mutate(player = replace(player,player == "Marc-André Fleury","Marc-Andre Fleury")) %>%
  mutate(player = replace(player,player == "Jaroslav Halák","Jaroslav Halak")) %>% 
  mutate(player = replace(player, player == "Eddie Läck","Eddie Lack")) %>% 
  mutate(player = replace(player, player == "Jacob Markström","Jacob Markstrom")) %>%
  mutate(player = replace(player, player == "Petr Mrázek","Petr Mrazek"))
  





Best_Goalies <- hr_data %>% 
  filter(!(player == "Martin Jones" & szn == "14-15")) %>%
  group_by(player) %>% 
  summarise(gp = sum(gp),
            w = sum(w),
            ga = sum(ga),
            sa = sum(sa),
            sv = sum(sv),
            ) %>%
  filter(gp > 120) %>% 
  mutate(mean_w_percent = w/gp, 
         mean_sv_percent = sv/sa,
         mean_gaa = ga/gp,
         avg_sv = mean(mean_sv_percent),
         mean_gsaa = (sa * (1-avg_sv))- ga,
         )

Best_Goalies <- Best_Goalies %>% left_join(Salaries, by =c("player"= "player"))

Mean_sv_plot <- Best_Goalies %>% 
  top_n(10,mean_sv_percent) %>%
  ggplot(aes(x = mean_sv_percent, y = mean_aav)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Save Percentage", y = "Average Annual Salary")

Mean_gaa_plot <- Best_Goalies %>% 
  top_n(-10,mean_gaa) %>%
  ggplot(aes(x = mean_gaa, y = mean_aav)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Goals Against Average", y = "Average Annual Salary")

mean_w_percent_plot <- Best_Goalies %>% 
  top_n(10, mean_w_percent) %>% 
  ggplot(aes(x = mean_w_percent, y = mean_aav)) + 
  geom_point() + 
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Percentage of Games Won", y = "Average Annual Salary")


mean_gsaa_plot <- Best_Goalies %>% 
  top_n(10, mean_gsaa) %>% 
  ggplot(aes(x = mean_gsaa,y = mean_aav))+ 
  geom_point() + 
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Goals Saved Above Average", y = "Average Annual Salary")


best_average_plots <- (Mean_sv_plot | Mean_gaa_plot) / (mean_w_percent_plot | mean_gsaa_plot)
 
best_average_plots + plot_annotation(
  title = "Top 10 Averages By Goalies Over The Last Six Years"
)


#Worst Goalies by averages overall

worst_mean_sv_plot <- Best_Goalies %>% 
  top_n(-10,mean_sv_percent) %>%
  ggplot(aes(x = mean_sv_percent, y = mean_aav)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Save Percentage", y = "Average Annual Salary")

worst_mean_gaa_plot <- Best_Goalies %>% 
  top_n(10,mean_gaa) %>%
  ggplot(aes(x = mean_gaa, y = mean_aav)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Goals Against Average", y = "Average Annual Salary")

worst_mean_w_percent_plot <- Best_Goalies %>% 
  top_n(-10, mean_w_percent) %>% 
  ggplot(aes(x = mean_w_percent, y = mean_aav)) + 
  geom_point() + 
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Percentage of Games Won", y = "Average Annual Salary")


worst_mean_gsaa_plot <- Best_Goalies %>% 
  top_n(-10, mean_gsaa) %>% 
  ggplot(aes(x = mean_gsaa,y = mean_aav))+ 
  geom_point() + 
  geom_text_repel(aes(label = player), size = 2) +
  labs(x = "Goals Saved Above Average", y = "Average Annual Salary")


Worst_average_plots <- (worst_mean_sv_plot | worst_mean_gaa_plot) / (worst_mean_w_percent_plot | worst_mean_gsaa_plot)

Worst_average_plots + plot_annotation(
  title = "Worst 10 Averages By Goalies Over The Last Six Years"
)


# Finding the best goalies by Season
# creates tables for each stat we are looking at

top_goalie_sv <- function(season){
  top_goalie_stats <- hr_data %>% filter(szn == season, gp > 40) 
  top_goalie_sv_percent <- top_goalie_stats %>% top_n(1,sv_percent) %>%
    select(player, sv_percent, szn)
  return(top_goalie_sv_percent)
}

top_goalie_gaa <- function(season){
  top_goalie_stats <- hr_data %>% filter(szn == season, gp > 40)
  goalie_gaa <- top_goalie_stats %>% top_n(-1,gaa) %>% 
    select(player, gaa, szn)
  return(goalie_gaa)
}


top_goalie_w_percent <- function(season){
  top_goalie_stats <- hr_data %>% filter(szn == season, gp > 40)
  goalie_w_percent <- top_goalie_stats %>% top_n(1,w_percent) %>% 
    select(player, w_percent, szn)
  return(goalie_w_percent)
}


top_goalie_gsaa <- function(season){
  top_goalie_stats <- hr_data %>% filter(szn == season, gp > 40)
  goalie_gsaa <- top_goalie_stats %>% top_n(1,gsaa) %>% 
    select(player, gsaa, szn)
  return(goalie_gsaa)
}

#Loop to fill in the tables

best_sv_year <- tibble()
best_gaa_year <- tibble()
best_win_percent_year <- tibble()
best_gsaa_year <- tibble()
for (i in 1:7){
  best_sv_year <- bind_rows(best_sv_year,top_goalie_sv(hr_szns[i]))
  best_gaa_year <- bind_rows(best_gaa_year,top_goalie_gaa(hr_szns[i]))
  best_win_percent_year <- bind_rows(best_win_percent_year,top_goalie_w_percent(hr_szns[i]))
  best_gsaa_year <- bind_rows(best_gsaa_year,top_goalie_gsaa(hr_szns[i]))
}



#Plots to see best goalies by season 


sv_plot <- best_sv_year %>% ggplot(aes(x = szn, y = sv_percent)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  coord_flip() + 
  labs(x = "Season", y = "Save Percentage")

gaa_plot <- best_gaa_year %>% ggplot(aes(x =szn, y = gaa)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  coord_flip() + 
  labs(x = "Season", y = "Goals Against Average")

w_percent_plot <- best_win_percent_year %>% ggplot(aes(x = szn, y = w_percent)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  coord_flip() + 
  labs(x = "Season", y = "Win Percentage")

gsaa_plot <- best_gsaa_year %>% ggplot(aes(x = szn, y = gsaa)) + 
  geom_point() +
  geom_text_repel(aes(label = player), size = 2) +
  coord_flip() + 
  labs(x = "Season", y = "Goals Saved Above Average")

year_plots <- (sv_plot | gaa_plot) / (w_percent_plot | gsaa_plot)

year_plots + plot_annotation(
  title = "Top Goalies by Season"
)



# Some really bad goalies by year 

missing_salary <- read_xls("data/MH_nhl_goalies_2017-2018.xls") %>% clean_names %>% 
  unite('player', c('first_name', 'last_name'), sep = ' ') %>% 
  filter(player == "Scott Darling") %>% 
  select(player, salary)




worst_goalies <- hr_data %>%
  filter(gp>40 & sv_percent <0.9)

worst_salaries <- cf_data %>% select(player, aav,szn)


worst_goalies <- worst_goalies %>% 
  left_join(worst_salaries,by = c("player" = "player","szn" = "szn")) %>%
  left_join(missing_salary, by = c("player" = "player")) %>% 
  mutate(aav = replace_na(aav,0)) %>% 
  mutate(salary = replace_na(salary,0)) %>% 
  mutate(aav = aav+ salary) %>% 
  unite('player_szn', c('player','szn'), sep = ' | Season:') %>%
  select(-salary)


wg_sv_percent <- worst_goalies %>% 
  ggplot(aes(x = sv_percent, y = aav)) + 
  geom_point()+
  geom_text_repel(aes(label = player_szn), size = 2) +
  labs(x = "Save Percentage", y = "Annual Average Salary")

wg_gsaa <- worst_goalies %>% ggplot(aes(x = gsaa, y = aav)) + 
  geom_point()+
  geom_text_repel(aes(label = player_szn), size = 2) +
  labs(x = "Goals Saved Above Average", y = "Annual Average Salary")

wg_gaa <- worst_goalies %>% ggplot(aes(x = gaa, y = aav)) + 
  geom_point()+
  geom_text_repel(aes(label = player_szn), size = 2) +
  labs(x = "Goals Against Average", y = "Annual Average Salary")

wg_w_percent <- worst_goalies %>% 
  ggplot(aes(x = w_percent, y = aav)) + 
  geom_point() +
  geom_text_repel(aes(label = player_szn), size = 2) +
  labs(x = "Win Percentage", y = "Annual Average Salary")

Worst_goalies_plot <- (wg_sv_percent | wg_gaa) / (wg_w_percent | wg_gsaa)

Worst_goalies_plot + plot_annotation(
  title = "Worst Perfoming goalies"
)

