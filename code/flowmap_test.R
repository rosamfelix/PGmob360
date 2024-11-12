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
  rename(id = group) |> 
  mutate(name = id) |> 
  select(id, name, lat, lon)

  
CENTROIDS$id[CENTROIDS$id == "Vila Franca de Xira"] = "VFX"
CENTROIDS$id[CENTROIDS$id == "Setúbal"] = "Setubal"
rm(Coords)  

TRIPSmode = readRDS("data/TRIPSmode.Rds")
TRIPS = TRIPSmode |> 
  rename(origin = Origin,
         dest = Destination,
         count = Total) |> 
  select(origin, dest, count)

TRIPS$origin[TRIPS$origin == "Vila Franca de Xira"] = "VFX"
TRIPS$dest[TRIPS$dest == "Setúbal"] = "Setubal"

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
# Oops… Sorry, but something went wrong.







# using the package data ----------------------------------------------------------------------

locations <- flowmapblue::ch_locations
flows <- flowmapblue::ch_flows
flowmap2 <- flowmapblue(
  locations = locations,
  flows = flows,
  mapboxAccessToken = Sys.getenv('MAPBOX_API_TOKEN'),
  clustering = TRUE,
  darkMode = TRUE,
  animation = FALSE
)
