# Line plot showing avg contract per year (cf_data using aav,)
cf_data_szn_aav <- cf_data %>% select(szn, aav) %>% 
  filter(!is.na(aav))
cf_data_szn_aav_aggr <- aggregate(cf_data_szn_aav$aav,list(szn = cf_data_szn_aav$szn),mean)

cf_data_szn_aav_aggr %>% 
  ggplot(aes(x = szn, y = x, group = 1)) +
  geom_line() + 
  geom_point() + 
  xlab("Season") + 
  ylab("Average AAV")



# Weight/height - scatterplot (cf_data height&weight)

cf_data_player_height_weight <- cf_data %>% select(player, weight, height, age, country) %>%
  clean_names() %>%
  unique() %>% 
  mutate(weight_kg = sub(".*-","", weight) %>% parse_number(), 
         height_cm = sub(".*-","",height) %>% parse_number()) 

cf_data_player_height_weight %>%
  mutate(text= paste("Country: ", country)) %>%
  ggplot(aes(x = weight_kg, y = height_cm, size = age)) + 
  geom_point(alpha=0.5) + 
  scale_size(range = c(.1,5), name = "Player age") +
  labs(
    x = "Player weight in kg",
    y= "Player height in cm",
    title = "Correlation between players' height and weight"
  )
