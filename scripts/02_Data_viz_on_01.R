# Script to visualise the results of data wrangling on the spatial dataset

rm(list = ls())

library(dplyr)
library(sf)
library(tmap)
library(patchwork)
load("outputs/2021-09-20_cleaned-data.rda")


# Save each plot as a separate object to use with patchwork

p1 = ggplot(sdat2) + 
  aes(x = dist_to_logging, y = Sum_CB_cover) + 
  geom_point() +
  stat_smooth() +
  theme_minimal()
p1

p2 = ggplot(sdat2) + 
  aes(x = Sum_CB_cover, y = pres.topa) + 
  geom_point() + 
  stat_smooth() +
  theme_minimal()
p2

p3 = ggplot(sdat2) +
  aes(x = dist_to_logging, y = pres.topa) +
  geom_point() +
  stat_smooth() +
  theme_minimal()
p3

plots_all = p1 + p2 + p3
plots_all

ggsave("images/plot1.png", plots_all, width = 8, height = 3)

# Making maps

tland = tm_shape(land) + 
  tm_fill()

tland + 
  tm_shape(sdat2) + 
  tm_symbols(col = "pres.topa", size = 0.15) +
  tm_scale_bar(position = c("right", "top"))


