library(readr)
library(circular)
library(Directional)
library(CircStats)
library(movMF)
library(ggplot2)
Earth_quack = read_csv("Significant Earthquake Dataset 1900-2023.csv")
Earth_quack = data.frame(Earth_quack)
head(Earth_quack)
dim(Earth_quack)
library(maps)
world_coordinates = map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates,map = world_coordinates,
    aes(long, lat , map_id = region),
    color = "black" , fill = "lightblue"
  ) +
  geom_point(
    data = Earth_quack , 
    aes(Earth_quack$Longitude , Earth_quack$Latitude , color = "red",),
    alpha = 1
  ) +
  theme(legend.position = "top")

