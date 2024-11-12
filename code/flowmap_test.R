# aim: test flowmapblue

# install.packages("flowmapblue")
library(flowmapblue)
library(sf)
library(dplyr)


# load the data
CENTROIDS = st_read("data/Centroid_pop.gpkg")
Coords = as.data.frame(st_coordinates(CENTROIDS))
CENTROIDS = CENTROIDS |> 
  mutate(lon = Coords[,1],
         lat = Coords[,2]) |> 
  st_drop_geometry() |> 
  rename(id = group)
rm(Coords)  

TRIPSmode = readRDS("data/TRIPSmode.Rds")
TRIPS = TRIPSmode |> 
  rename(origin = Origin,
         dest = Destination,
         count = Total) |> 
  select(origin, dest, count)

# plot the flow map
flowmap <- flowmapblue(
  locations = CENTROIDS,
  flows = TRIPS,
  mapboxAccessToken = Sys.getenv('MAPBOX_PUBLIC_TOKEN'),
  clustering = TRUE,
  darkMode = TRUE,
  animation = FALSE
)
flowmap







# using the package data ----------------------------------------------------------------------

locations <- data(ch_locations)
flows <- data(ch_flows)
flowmap <- flowmapblue(
  locations = locations,
  flows = flows,
  mapboxAccessToken = Sys.getenv('MAPBOX_PUBLIC_TOKEN'),
  clustering = TRUE,
  darkMode = TRUE,
  animation = FALSE
)
