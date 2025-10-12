library(r5r)
library(sf)
library(tigris)
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(tidytransit)

options(tigris_use_cache = TRUE)
# Increase Java memory for routing (adjust as needed)
options(java.parameters = "-Xmx16G")



# # Paths (kept SIMPLE — use your uploaded files)
# data_dir    <- "data"                 # where your cut.zip and restore.zip live
# osm_pbf     <- file.path(data_dir, "pennsylvania-latest.osm.pbf")
# # GTFS (simple, direct paths)
# gtfs_before <- file.path(data_dir, "restore.zip")  # baseline
# gtfs_after  <- file.path(data_dir, "cut.zip")      # post-cuts
# outputs_dir <- file.path(data_dir, "outputs")
# if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)
# 
# # Auto-download OSM once if missing (simple download)
# if (!file.exists(osm_pbf)) {
#   message("Downloading Pennsylvania OSM extract (once)…")
#   dir.create(dirname(osm_pbf), showWarnings = FALSE, recursive = TRUE)
#   download.file(
#     url      = "https://download.geofabrik.de/north-america/us/pennsylvania-latest.osm.pbf",
#     destfile = osm_pbf,
#     mode     = "wb",
#     quiet    = TRUE
#   )
# }
# 
# # Buffer distance around stops to define service area for filtering tracts
# service_buffer_m <- 2000
# 
# # Auto-download OSM PBF if not present
# ensure_osm_pbf <- function(path){
#   if (!file.exists(path)){
#     message("Downloading Mid-Atlantic OSM PBF from Geofabrik…")
#     url <- "https://download.geofabrik.de/north-america/us/mid-atlantic-latest.osm.pbf"
#     utils::download.file(url, destfile = path, mode = "wb", quiet = FALSE)
#     message("Saved OSM to ", path)
#   } else {
#     message("Using existing OSM: ", path)
#   }
#   invisible(path)
# }
# ensure_osm_pbf(osm_pbf)

# ---------------------------
# 2) Download / Load Census Tracts (sf)
# ---------------------------
# message("Downloading TIGER/Line tracts for 5-County SE PA…")

# pa_state <- "PA"
# pa_counties <- c("Philadelphia", "Delaware", "Montgomery", "Bucks", "Chester")
# tracts <- tracts(state = pa_state, county = pa_counties, year = 2023, cb = TRUE, class = "sf") |>
#   st_transform(4326)
#
# # Keep only GEOID + geometry for simplicity
# tracts <- tracts %>% select(GEOID, NAME, ALAND, AWATER, geometry)
#
# # Create interior point centroids for reliable inside points
# tract_centroids <- st_point_on_surface(tracts) %>%
#   mutate(from_id = GEOID) %>%
#   transmute(from_id, geometry)

# Origins data.frame for r5r (lon/lat columns)
# origins <- tract_centroids %>%
#   st_transform(4326) %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
#   st_drop_geometry() %>%
#   select(id = from_id, lon, lat)

# Destination (City Hall)
cityhall_lon <- -75.1636
cityhall_lat <-  39.9526
destinations <- tibble(id = "CityHall", lon = cityhall_lon, lat = cityhall_lat)
departure_time <- ymd_hms("2025-10-16 11:30:00")
# ---------------------------
# 3) (Optional) Limit tracts to those overlapping GTFS service area
#     by buffering GTFS stops and intersecting (SIMPLE PATHS)
# ---------------------------

# message("Reading GTFS (before) stops and building service buffer…")
# stops_before <- tidytransit::read_gtfs(gtfs_before)$stops %>%
#   sf::st_as_sf(coords = c("stop_lon","stop_lat"), crs = 4326, remove = FALSE) %>%
#   sf::st_transform(3857)
# service_area <- stops_before %>% st_buffer(service_buffer_m) %>% st_union() %>% st_make_valid() %>% st_transform(4326)
#
# tracts_served <- tracts %>% st_filter(service_area)  # only tracts intersecting service area

# Update centroids/origins for routing to limit computations
# tract_centroids_served <- tract_centroids %>% semi_join(tracts_served %>% st_drop_geometry(), by = c("from_id" = "GEOID"))
# origins_served <- tract_centroids_served %>%
#   st_transform(4326) %>%
#   mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
#   st_drop_geometry() %>%
#   select(id = from_id, lon, lat)

# st_write(tracts_served,"data/outputs/tracts_served.gpkg", delete_dsn = TRUE, quiet = TRUE)
# st_write(tract_centroids_served,"data/outputs/tract_centroids_served.gpkg", delete_dsn = TRUE, quiet = TRUE)
tracts_centroids_served <- st_read("data/outputs/tract_centroids_served.gpkg")
origins_served <- tracts_centroids_served %>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id = from_id, lon, lat)


modes          <- c("WALK","TRANSIT")
max_walk_dist  <- 3000     # meters
max_trip_duration <- 180   # minutes; cap long searches
num_itineraries   <- 3     # sample multiple itineraries
walk_speed        <- 1.3   # m/s ~ 10.8 km/h if you want 1.3 m/s typical, change


#Function 1: Setup R5 core and build network
setup_r5_core <-build_network(
    data_path = "data/cut_network_build",
    verbose = FALSE,
    temp_dir = TRUE,
    elevation = "NONE",
    overwrite = FALSE
  )


# Function 2: Compute travel time matrix (with reduced memory parameters)
compute_travel_time_matrix <- function(network, origins_df, destinations_df, departure_time,
                                       modes, max_trip_duration, walk_speed) {


  ttm <- travel_time_matrix(
    r5r_network= network,
    origins      = origins_df,
    destinations = destinations_df,
    mode         = modes,
    departure_datetime = departure_time,
    max_walk_time = 30,
    max_trip_duration = max_trip_duration,
    walk_speed = walk_speed,
    progress = TRUE,
    max_rides = 4,
    max_lts = 2
  )

  return(ttm)
}


# before_cuts<-compute_travel_time_matrix(setup_r5_core, origins_served, destinations, departure_time,
#                                        modes, max_trip_duration, walk_speed)
# write.csv(before_cuts, "data/outputs/before_cuts.csv")

# before_cuts_non_peak<-compute_travel_time_matrix(setup_r5_core, origins_served, destinations, departure_time,
#                                        modes, max_trip_duration, walk_speed)
# write.csv(before_cuts_non_peak, "data/outputs/before_cuts_non_peak.csv")



# after_cuts<-compute_travel_time_matrix(setup_r5_core, origins_served, destinations, departure_time,
#                                        modes, max_trip_duration, walk_speed)
# write.csv(after_cuts, "data/outputs/after_cuts.csv")
# after_cuts_peak<-compute_travel_time_matrix(setup_r5_core, origins_served, destinations, departure_time,
#                                        modes, max_trip_duration, walk_speed)
# write.csv(after_cuts_peak, "data/outputs/after_cuts_peak.csv")









