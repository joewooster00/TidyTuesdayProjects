library(tidytuesdayR)
library(tidyverse)
library(gganimate)
library(lubridate)
library(viridis)
library(ggrepel)
library(magick)

theme_set(theme_bw())

tuesdata <- tt_load("2020-06-23")

locations <- tuesdata$locations
individuals <- tuesdata$individuals

locations <- locations %>% mutate(date = date(timestamp))
locations <- locations %>% mutate(year = year(timestamp))

color_pal <- c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
               "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
               "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
               "#8A7C64", "#599861")


p <- locations %>% filter(year == "2016") %>% 
  ggplot(aes(x = longitude, y = latitude, color = animal_id, size = 1.5)) +
  geom_point(show.legend = FALSE)+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  transition_states(date)+
  labs(title = "Date: {closest_state}")+
  #  shadow_mark(past = T, future = F, alpha = 0.75)+
  shadow_wake(wake_length = 0.3, alpha = FALSE)+
  scale_color_manual(values = color_pal)


anim <- animate(p, fps = 10, duration = 30)
