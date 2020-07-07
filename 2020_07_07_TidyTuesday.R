library(tidytuesdayR)
library(tidyverse)
library(reshape2)


##loading data from Tidytuesday Github
tuesdata <- tt_load('2020-07-07')
ratings <- tuesdata$coffee_ratings

##clean score categories to remove _
ratings <- ratings %>%
  rename("clean cup" = clean_cup) %>%
  rename("cupper points" = cupper_points)

##melting raw data to get list of countries and species of coffee beans along with the value of each score
ratings_melt <- ratings %>%
  melt(
    id.vars = c("species", "country_of_origin"),
    measure.vars = c("aroma", "flavor", "aftertaste", "acidity","body","balance","uniformity","clean cup","sweetness","cupper points"),
    variable.name ="score_category",
    value.name = "val"
  ) %>%
  group_by(country_of_origin, species, score_category) %>%
  summarise(val = mean(val))

##getting list of top 10 countries based on number of bags of coffee
top_10_countries <- ratings %>%
  melt(
    id.vars = "country_of_origin",
    measure.vars = "number_of_bags",
    variable.name = "num_bags",
    value.name = "val"
  ) %>%
  group_by(country_of_origin) %>%
  summarise(val = sum(val)) %>%
  top_n(10) %>%
  select(country_of_origin)

##creating plot for top 10 countries in list above
ratings_melt %>%
  filter(country_of_origin %in% top_10_countries$country_of_origin) %>%
  ggplot(aes(x=score_category, y=val, group=species, color = species, fill=species, label=score_category))+
  geom_point(size=2, alpha = 1)+
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 33),
    axis.title = element_blank(),
    aspect.ratio = 1/1.25
  )+
  labs(title = "Coffee Ratings by Country", subtitle = "Top 10 Countries by Number of Bags of Coffee",
       caption = "cppp: cupper points, arom: aroma, flvr: flavor, aftr: aftertaste, acdt: acidity, body: body, blnc: balance, unfr: uniformity, clnc: clean cup, swtn: sweetness")+
  geom_polygon(size=1, alpha=0.1)+
  ylim(0,10)+
  scale_x_discrete(labels = abbreviate)+
  coord_polar()+
  facet_wrap(~ country_of_origin, ncol = 5)
