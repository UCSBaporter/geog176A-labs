###########################
## Abigail Porter
## 08-23-2020
## Lab 3
#########################

install.packages("ggrepel")
install.packages("gghighlight")

library(tidyverse)
library(sf)
library(units)
library(ggrepel)
library(gghighlight)


region = data.frame(region = state.region, state_name = state.name)


eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

USAboundaries::us_states(resolution = "low") %>%
  filter(!state_name %in% c("Puerto Rico", "Alaska", "Hawaii", "Guam")) %>%
  st_transform(eqdc)

st_as_sf(rnaturalearth::countries110) %>%
  filter(!admin %in% ("Canada", "Mexico", "United States"))

***THIS part "South" will change in lab

conus = USAboundaries::us_states() %>% left_join(region) %>%
  filter(region == "South")

cities = readr::read_csv("data/uscities.csv")




***THIS part is how you make a plot
plot(conus)


