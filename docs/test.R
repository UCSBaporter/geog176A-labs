library(tidyverse)
library(sf)
library(leaflet)

states = USAboundaries::us_states()

-----Visual examples
tmp = states %>%
  filter(grepl("South", state_name))

plot(tmp$geometry, col = "red")

________ could change up
tmp = states %>%
  filter(grepl("I", state_name))

plot(tmp$geometry, col = "red")

tmp = states %>%
  filter(grepl("Ca", state_name))

plot(tmp$geometry, col = "red")

********************
library(tidyverse)
library(sf)
library(leaflet)

states = USAboundaries::us_states() %>%  st_transform(5070)

state.of.interest = "Pennsylvania"

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)

closest = st_make_grid(soi, n = 70, square = FALSE) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

plot(closest)

vor = closest %>%
  st_union() %>%
  st_voronoi()

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircles(data = st_transform(closest, 4326),
             radius = 1, color = ~colorFactor("YlOrRd",
                          state_name)(state_name))

__________ build on

vor = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast %>%
  st_sf() %>%
  st_join(closest) %>%
  group_by(state_name) %>%
  summarise() %>%
  st_intersection(soi)

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = st_transform(vor, 4326),
              fillColor = ~colorFactor("YlOrRd",
                  state_name)(state_name),
              weight = .5, color = "black")

------
leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = st_transform(vor, 4326),
              fillColor = ~colorFactor("YlOrRd",
                                       state_name)(state_name),
              color = NA) %>%
  addPolygons(data = st_transform(soi, 4326), fillColor = "transparent",
              color = "black", group = "SOI") %>%
  addPolygons(data = st_transform(adjoining, 4326), fillColor =
                 ~colorFactor("YlOrRd", state_name)(state_name),
               col = NA) %>%
                addLayersControl(overlayGroups = c("SOI"))
