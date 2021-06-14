library(nflfastR)
library(tidyverse)
library(dplyr)
library(ggplot2)

nfl_passing_plays <- as_tibble(read.csv("C:/Users/Rebas/Downloads/nfl_passing_plays_2020.csv"))

nfl_passing_plays


nfl_passing_plays %>%
  group_by(passer_player_name)
ggplot(aes(x = touchdown)) +
  geom_bar() +
  theme_bw()


nfl_passing_plays %>%
  filter(passer_player_name==c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock")) %>%
  group_by(passer_player_name) %>%
  summarize(total_hit = sum(qb_hit)) %>%
  ungroup() %>%
  ggplot(aes(x=passer_player_name, y=total_hit)) +
  geom_col()+
  #theme(axis.text.x=element_text(angle=90))+
  theme_bw()



nfl_passing_plays %>%
  # filter(passer_player_name==c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock")) %>%
  #group_by(passer_player_name) %>%
  group_by(posteam) %>%
  ungroup() %>%
  ggplot(aes(x = posteam, y = touchdown)) +
  geom_col() +
  theme_bw() 




#2D: if a QB is hit are they more likely to throw an interception ------------------------------
#hypothesis: if QB is hit --> more likely to throw interception
nfl_passing_plays %>%                         #should we change the 0's and 1's to names?
  group_by(qb_hit, interception) %>%
  summarize(count = n(),
            joint_prob = count / nrow(nfl_passing_plays)) %>%
  ungroup() %>%
  ggplot(aes(x=qb_hit, y=interception)) +
  geom_tile(aes(fill=count), color="white")+
  geom_text(aes(label = round(joint_prob, digits=4)), color = "white")+
  scale_fill_viridis_b()+
  theme_bw()+
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm"))



nfl_passing_plays %>%
  filter(!is.na(pass_location)) %>%
  ggplot(aes(x = yards_gained, color = pass_location)) +
  geom_freqpoly() +
  theme_bw() +
  theme(legend.position = "bottom")


# Scatter plot exploring yards gained per play for short and deep passes. Hypothesis: Completed passes further down the field will gain more yards.--------

nfl_passing_plays %>%
  #pivot_longer()
  #filter(play_id %in% pass_length %in% c("deep", "short"))
  ggplot(aes (x = play_id, y = yards_gained)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~ pass_length) 





# Exploring how often passer for each team gets hit. Hypothesis is teams that have passer get hit more often have weaker o-line. ----------------------



nfl_passing_plays %>%
  group_by(posteam) %>%
  summarize(total_hit = sum(qb_hit)+sum(sack)) %>%
  ungroup() %>%
  mutate(posteam = fct_reorder(posteam, total_hit))%>%
  ggplot(aes(x=posteam, y=total_hit)) +
  geom_col()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 90))+
  labs(x= "Possessing Team",
       y= "QB hits + sacks",
       title = "Comparing NFL Team's Offensive Line Strength",
       caption = "Data courtesy of nflfastR")





# Proportion of Interception when passer is hit. Hypothesis is passer would be more likely to throw int when they are hit. ---------------------------



nfl_passing_plays %>%                         
  group_by(qb_hit, interception) %>%
  summarize(count = n(),
            joint_prob = count / nrow(nfl_passing_plays)) %>%
  ungroup() %>%
  mutate(qb_hit_name = case_when(
    qb_hit == 0 ~ "No hit",
    TRUE ~ "Hit"
  ), interception_name = case_when(
    interception == 0 ~ "No interception",
    TRUE ~ "Interception"
  )) %>%
  ggplot(aes(x=qb_hit_name, y=interception_name)) +
  geom_tile(aes(fill=count), color="white")+
  geom_text(aes(label = round(joint_prob, digits=4)), color = "white")+
  scale_fill_viridis_b()+
  theme_bw()+
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm")) +
  labs(
    x = "QB Hit",
    y = "Interception",
    title = "Interception Not More Likely When Hit",
    caption = "Data courtesy of nflfastR"
  )


# ECDF and Density Plot of Yards Gained. My hypothesis: Most passes will be less than 20 yards-----------------------------------
library(patchwork)

nfl_passing_plays_den <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained, color = complete_pass)) +
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards gained", y = "Density")

nfl_passing_plays_ecdf <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained, color = complete_pass)) + 
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained", y = "Proportion of completed passes")


nfl_passing_plays_den + theme(legend.position = "none") +
  nfl_passing_plays_ecdf + plot_layout(guides = "collect")
#nfl_passing_plays_den / nfl_passing_plays_ecdf