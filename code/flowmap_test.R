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
  select(origin, dest, count) |> 
  mutate(count = as.integer(count))

TRIPS$origin[TRIPS$origin == "Vila Franca de Xira"] = "VFX"
TRIPS$dest[TRIPS$dest == "Vila Franca de Xira"] = "VFX"
TRIPS$origin[TRIPS$origin == "Setúbal"] = "Setubal"
TRIPS$dest[TRIPS$dest == "Setúbal"] = "Setubal"

TRIPS_complete = TRIPS |> 
  fastDummies::dummy_rows()

TRIPS_complete[is.na(TRIPS_complete)] = 0


# PTransit
TRIPStp = TRIPSmode |> 
  rename(origin = Origin,
         dest = Destination,
         count = PTransit) |> 
  select(origin, dest, count) |> 
  mutate(count = as.integer(count))

TRIPStp$origin[TRIPStp$origin == "Vila Franca de Xira"] = "VFX"
TRIPStp$dest[TRIPStp$dest == "Vila Franca de Xira"] = "VFX"
TRIPStp$origin[TRIPStp$origin == "Setúbal"] = "Setubal"
TRIPStp$dest[TRIPStp$dest == "Setúbal"] = "Setubal"

# Car
TRIPScar = TRIPSmode |> 
  rename(origin = Origin,
         dest = Destination,
         count = Car) |> 
  select(origin, dest, count) |> 
  mutate(count = as.integer(count))

TRIPScar$origin[TRIPScar$origin == "Vila Franca de Xira"] = "VFX"
TRIPScar$dest[TRIPScar$dest == "Vila Franca de Xira"] = "VFX"
TRIPScar$origin[TRIPScar$origin == "Setúbal"] = "Setubal"
TRIPScar$dest[TRIPScar$dest == "Setúbal"] = "Setubal"


# export csv
write.csv(TRIPS_complete, "data/flows.csv", row.names = F, quote = F)
write.csv(TRIPStp, "data/flows_tp.csv", row.names = F, quote = F)
write.csv(TRIPScar, "data/flows_car.csv", row.names = F, quote = F)
write.csv(CENTROIDS, "data/locations.csv", row.names = F, quote = F)


# # plot the flow map
# flowmap <- flowmapblue(
#   locations = CENTROIDS,
#   flows = TRIPS_complete,
#   mapboxAccessToken = Sys.getenv('MAPBOX_PUBLIC_TOKEN'),
#   clustering = TRUE,
#   darkMode = TRUE,
#   animation = FALSE
# )
# flowmap
# # Oops… Sorry, but something went wrong.


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
