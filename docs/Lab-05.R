---
  title: "Geography 176A: Rasters & Remote Sensing"
author: '[Abigail Porter](https://ucsbaporter.github.io/UCSBaporterW1/first-webpage/index.html)'
subtitle: 'Lab 05'
output:
  html_document:
  theme: journal
---

### Libraries

#Data Manipulation
library(tidyverse)
#Raster Data Handling
library(raster)
#Keyless Landset Data (2013-2017)
library(getlandsat)
#Rapid Interactive Visualization
library(mapview)
#Vector Data Processing
library(sf)

#
library(USAboundaries)
library(rnaturalearth)
library(rmapshaper)
library(readxl)
library(gghighlight)
library(ggrepel)
library(knitr)
library(ggthemes)
library(leaflet)
library(leafpop)
library(units)
#

install.packages('osmdata')
library(osmdata)

bb = read_csv("data/uscities.csv") %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000)

# To view above in a map (bounding box circle)
mapview(bb)

bb = read_csv("data/uscities.csv") %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc()

# To view above in a map (bounding box square)
mapview(bb)

###

bwgs = st_transform(bb, 4326)

osm = osmdata::opq(bwgs)

mapview(bwgs)

###
bwgs = st_transform(bb, 4326)

osm = osmdata::opq(bwgs) %>%
  osmdata::add_osm_feature("building") %>%
  osmdata::osmdata_sf()

mapview(osm$osm_polygons)

###
bwgs = st_transform(bb, 4326)

osm = osmdata::opq(bwgs) %>%
  osmdata::add_osm_feature("natural") %>%
  osmdata::osmdata_sf()

mapview(osm$osm_polygons)

###
bbwgs = st_bbox(bwgs)

scenes = lsat_scenes()

down = scenes %>%
  filter(min_lat <= bbwgs$ymin, max_lat >=bbwgs$ymax,
         min_lon <= bbwgs$xmin, max_lon >=bbwgs$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood.csv", row.names = FALSE)


# IN RMD NOW  ---------

meta = read_csv("data/palo-flood.csv")

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B", 1:6, ".TIF$", collapse = "|"), file)) %>%
  arrange(file) %>%
  pull(file)
#"paste0" gives space between columns, "paste" will not give a space.
# We only need "6" bands because they are very large
#for (i in 1:6) download each of these bands for us

st = sapply(files, lsat_image)
# "sapply" is returning a list of vectors (instead of repeating the role 6 times)

s = stack(st) %>%
  setNames(paste0("band", 1:6))

cropper = bb %>% st_as_sf() %>%  st_transform(crs(s))

r = crop(s, cropper)

plotRGB(r, r = 4, g = 3, b = 2)
plotRGB(r, r = 5, g = 4, b = 3)
plotRGB(r, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(r, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "lin")
#Visual options (above) to explain variance
#Use this to plot questions (1,2,3?)

par(mfrow = c(2,1))
plotRGB(r, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "hist")
#one above the other plot

par(mfrow = c(2,2))
plotRGB(r, r = 4, g = 3, b = 2, stretch = "hist")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "hist")
#Side by side plot

ndvi = (r$band5 - r$band4) / (r$band5 + r$band4)

palette = colorRampPalette(c("blue", "white", "red"))

plot(ndvi, col = palette(256))




thresholding = function(x){ifelse(x <= 0,1,NA)}

flood = calc(ndvi, thresholding)
flood[flood == 0] = NA

plot(flood)

mapview(flood)

#Troubleshooting issue with map
mapview(bb) + flood

mapview(bb) + ndvi

flood

mapview(flood)

dev.off
plot(flood)

install.packages("mapview")

remotes::install_github("r-spatial/mapview")

thresholding = function(x){ifelse(x <= 0,1,0)}

set.seed

### Question 1:

install.packages('osmdata')
library(osmdata)

bb = read_csv("data/uscities.csv") %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_sf()

# To view above in a map (bounding box square)
mapview(bb)

### Question 2:

#2.1
