---
title: "Geography 176A: Tesselations, Point-in-Polygon"
author: '[Abigail Porter](https://ucsbaporter.github.io/UCSBaporterW1/first-webpage/index.html)'
subtitle: 'Lab 04: exercise 15 (Q1)'
output:
  html_document:
  theme: journal
---

### Libraries

```
# SPDS
library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearth)
library(rmapshaper)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)
library(ggthemes)
```

### Question 1:

#1.1

CONUS = USAboundaries::us_counties() %>%
  filter(!(state_name %in% c("Puerto Rico", "Alaska", "Hawaii"))) %>%
  st_transform(5070)

#1.2

county_centroids = st_centroid(CONUS) %>%
  st_union() %>%
  st_cast("MULTIPOINT")

#1.3

voronoi_grid = st_voronoi(county_centroids) %>%
  st_as_sf() %>%
  st_cast() %>%
  mutate(id = 1:n())

tri_grid = st_triangulate(county_centroids) %>%
  st_as_sf() %>%
  st_cast() %>%
  mutate(id = 1:n())

# Regular Tiles (L15, slide 29)
sq_grid = st_make_grid(CONUS, n = c(70, 50)) %>%
  st_as_sf() %>%
  st_cast() %>%
  mutate(id = 1:n())

***example given (L15, slide 31)

hex_grid = st_make_grid(CONUS, n = c(70, 50), square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

#1.4

#st_voronoi (L15, slide 41)

plot_tess(voronoi_grid, "Voronoi Coverage")

voronoi_grid = st_intersection(voronoi_grid, st_union(CONUS))

plot_tess(voronoi_grid, "Voronoi Coverage") +
  geom_sf(data = county_centroids, col = "darkred", size = .2)


#st_triangulate (L 15, Slide 47)

plot_tess(t_grid, "Square Coverage")

tri_grid = st_intersection(tri_grid, st_union(CONUS))

plot_tess(tri_grid, "Voroni Coverage") +
  geom_sf(data = county_centroids, col = "darkred", size = .3)


#1.6

plot_tess = function(data, title)
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:" nrow(data), "tiles")) +
    theme(plot.title = element_text(hjust = .5, color = "navy", face = "bold"))


***example given
#Tesselation plotting function
plot_tess(data = CONUS, "Counties")

#Grid
plot_tess(sq_grid, "Square Coverage")

#Hex
plot_tess(hex_grid, "Hexegonal Coverage")
