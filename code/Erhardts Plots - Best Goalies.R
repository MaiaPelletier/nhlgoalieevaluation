# plots for the thing 

#Best Goalies by averages overall 

hr_data <- hr_data %>% mutate(w_percent = w/gp)

Salaries <- cf_data %>% group_by(player) %>% 
  summarise(mean_aav = mean(aav)) %>% 
  mutate(player = replace(player,player == "Marc-André Fleury","Marc-Andre Fleury")) %>%
  mutate(player = replace(player,player == "Jaroslav Halák","Jaroslav Halak"))


help(replace)

Best_Goalies <- hr_data %>% 
  filter(!(player == "Martin Jones" & szn == "14-15")) %>%
  group_by(player) %>% 
  summarise(mean_sv_percent = mean(sv_percent),mean_gp = mean(gp), mean_gaa = mean(gaa), mean_w_percent = mean(w_percent), 
            mean_gsaa = mean(gsaa)) %>% 
  filter(mean_gp > 40)

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


average_plots <- (Mean_sv_plot | Mean_gaa_plot) / (mean_w_percent_plot | mean_gsaa_plot)
 



#Worst Goalies by averages overall




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
  best_sv_year <- bind_rows(best_sv_year,top_goalie(hr_szns[i]))
  best_gaa_year <- bind_rows(best_gaa_year,top_goalie_gaa(hr_szns[i]))
  best_win_percent_year <- bind_rows(best_win_percent_year,top_goalie_w_percent(hr_szns[i]))
  best_gsaa_year <- bind_rows(best_gsaa_year,top_goalie_gsaa(hr_szns[i]))
}



#Plots to see best goalies by season 

install.packages("ggrepel")
library(ggrepel)

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

year_plots





