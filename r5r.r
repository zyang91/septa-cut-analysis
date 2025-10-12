# Philly GTFS Travel Time (r5r) Pipeline
# Author: <your name>
# Purpose: Compute transit travel time from every census tract centroid to Philadelphia City Hall
#          before and after service cuts, then map the differences.
#
# Requirements:
# - Java 11+ installed
# - r5r >= 1.0.0, sf, tigris, tidyverse, readr, lubridate, ggplot2, scales
# - Inputs: OSM .pbf for the region, GTFS zip (before), GTFS zip (after)

# ---------------------------
# 0) Libraries & Options
# ---------------------------


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
options(java.parameters = "-Xmx8G")

# ---------------------------
# 1) User Inputs (EDIT THESE)
# ---------------------------

# Paths (kept SIMPLE — use your uploaded files)
data_dir    <- "data"                 # where your cut.zip and restore.zip live
osm_pbf     <- file.path(data_dir, "pennsylvania-latest.osm.pbf")
# GTFS (simple, direct paths)
gtfs_before <- file.path(data_dir, "restore.zip")  # baseline
gtfs_after  <- file.path(data_dir, "cut.zip")      # post-cuts
outputs_dir <- file.path(data_dir, "outputs")
if (!dir.exists(outputs_dir)) dir.create(outputs_dir, recursive = TRUE)

# Auto-download OSM once if missing (simple download)
if (!file.exists(osm_pbf)) {
  message("Downloading Pennsylvania OSM extract (once)…")
  dir.create(dirname(osm_pbf), showWarnings = FALSE, recursive = TRUE)
  download.file(
    url      = "https://download.geofabrik.de/north-america/us/pennsylvania-latest.osm.pbf",
    destfile = osm_pbf,
    mode     = "wb",
    quiet    = TRUE
  )
}

# Buffer distance around stops to define service area for filtering tracts
service_buffer_m <- 2000

# Auto-download OSM PBF if not present
ensure_osm_pbf <- function(path){
  if (!file.exists(path)){
    message("Downloading Mid-Atlantic OSM PBF from Geofabrik…")
    url <- "https://download.geofabrik.de/north-america/us/mid-atlantic-latest.osm.pbf"
    utils::download.file(url, destfile = path, mode = "wb", quiet = FALSE)
    message("Saved OSM to ", path)
  } else {
    message("Using existing OSM: ", path)
  }
  invisible(path)
}
ensure_osm_pbf(osm_pbf)

# ---------------------------
# 2) Download / Load Census Tracts (sf)
# ---------------------------
# message("Downloading TIGER/Line tracts for 5-County SE PA…")

pa_state <- "PA"
pa_counties <- c("Philadelphia", "Delaware", "Montgomery", "Bucks", "Chester") 
tracts <- tracts(state = pa_state, county = pa_counties, year = 2023, cb = TRUE, class = "sf") |>
  st_transform(4326)

# Keep only GEOID + geometry for simplicity
tracts <- tracts %>% select(GEOID, NAME, ALAND, AWATER, geometry)

# Create interior point centroids for reliable inside points
tract_centroids <- st_point_on_surface(tracts) %>%
  mutate(from_id = GEOID) %>%
  transmute(from_id, geometry)

# Origins data.frame for r5r (lon/lat columns)
origins <- tract_centroids %>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id = from_id, lon, lat)

# Destination (City Hall)
cityhall_lon <- -75.1636
cityhall_lat <-  39.9526
destinations <- tibble(id = "CityHall", lon = cityhall_lon, lat = cityhall_lat)
departure_time <- ymd_hms("2025-10-16 07:30:00")
# ---------------------------
# 3) (Optional) Limit tracts to those overlapping GTFS service area
#     by buffering GTFS stops and intersecting (SIMPLE PATHS)
# ---------------------------

message("Reading GTFS (before) stops and building service buffer…")
stops_before <- tidytransit::read_gtfs(gtfs_before)$stops %>%
  sf::st_as_sf(coords = c("stop_lon","stop_lat"), crs = 4326, remove = FALSE) %>%
  sf::st_transform(3857)
service_area <- stops_before %>% st_buffer(service_buffer_m) %>% st_union() %>% st_make_valid() %>% st_transform(4326)

tracts_served <- tracts %>% st_filter(service_area)  # only tracts intersecting service area

# Update centroids/origins for routing to limit computations
tract_centroids_served <- tract_centroids %>% semi_join(tracts_served %>% st_drop_geometry(), by = c("from_id" = "GEOID"))
origins_served <- tract_centroids_served %>%
  st_transform(4326) %>%
  mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(id = from_id, lon, lat)

# ---------------------------
# 4) Build R5 core and compute travel times (BEFORE cuts)
# ---------------------------

build_core_and_ttm <- function(osm_pbf, gtfs_zip, origins_df, destinations_df, departure_time,
                               modes, max_walk_dist, max_trip_duration, num_itineraries, walk_speed){
  message("Building R5 core: ", gtfs_zip)
  r5r_core <- setup_r5(data_path = dirname(gtfs_zip),
                       temp_dir = TRUE,
                       verbose = FALSE,
                       # Build network with these inputs explicitly
                       # When data_path contains multiple GTFS/OSM, r5r tries to pick up; use the function below to enforce
  )
  
  # Force network build from specific files (ensures only desired GTFS used)
  r5r::build_network(r5r_core, elevation = FALSE, quiet = TRUE,
                     overwrite = TRUE,
                     routing_files = list(osm = osm_pbf, transit = gtfs_zip))
  
  message("Routing…")
  ttm <- travel_time_matrix(
    r5r_core,
    origins      = origins_df,
    destinations = destinations_df,
    mode         = modes,
    departure_datetime = departure_time,
    max_walk_dist = max_walk_dist,
    max_trip_duration = max_trip_duration,
    n_threads = max(1, parallel::detectCores() - 1),
    walk_speed = walk_speed,
    number_of_breaks = 0,
    shortest_path = FALSE,
    progress = TRUE,
    max_rides = 8,
    max_lts = 2,
    fare_cutoff = Inf,
    percentiles = 50,          # median travel time
    draws_per_minute = 5,
    time_window = 60,          # explore departures within 60-min window
    drop_geometry = TRUE,
    keep = c("access_time", "waiting_time", "in_vehicle_time", "transfer_time")
  )
  
  stop_r5(r5r_core)
  ttm
}

modes          <- c("WALK","TRANSIT")
max_walk_dist  <- 3000     # meters
max_trip_duration <- 180   # minutes; cap long searches
num_itineraries   <- 3     # sample multiple itineraries
walk_speed        <- 1.3   # m/s ~ 10.8 km/h if you want 1.3 m/s typical, change


# BEFORE
ttm_before <- build_core_and_ttm(
  osm_pbf, gtfs_before, origins_served, destinations, departure_time,
  modes, max_walk_dist, max_trip_duration, num_itineraries, walk_speed
)

# AFTER
ttm_after <- build_core_and_ttm(
  osm_pbf, gtfs_after, origins_served, destinations, departure_time,
  modes, max_walk_dist, max_trip_duration, num_itineraries, walk_speed
)

# ---------------------------
# 5) Join results and compute differences
# ---------------------------

# r5r returns columns: from_id, to_id, travel_time, etc. (travel_time in minutes)
res <- ttm_before %>%
  select(from_id, travel_time_before = travel_time, access_time_before = access_time,
         waiting_time_before = waiting_time, in_vehicle_time_before = in_vehicle_time,
         transfer_time_before = transfer_time) %>%
  full_join(
    ttm_after %>% select(from_id, travel_time_after = travel_time, access_time_after = access_time,
                         waiting_time_after = waiting_time, in_vehicle_time_after = in_vehicle_time,
                         transfer_time_after = transfer_time),
    by = "from_id"
  ) %>%
  mutate(diff_minutes = travel_time_after - travel_time_before)

# Attach back to tracts for mapping
tracts_diff <- tracts %>% left_join(res, by = c("GEOID" = "from_id"))

# ---------------------------
# 6) Save outputs
# ---------------------------

csv_path <- file.path(outputs_dir, "tract_travel_time_diff.csv")
st_write_path <- file.path(outputs_dir, "tract_travel_time_diff.gpkg")

readr::write_csv(res, csv_path)
try({ st_write(tracts_diff, st_write_path, delete_dsn = TRUE, quiet = TRUE) })

message("Saved:")
message(" - ", csv_path)
message(" - ", st_write_path)

# ---------------------------
# 7) Quick map of travel time differences (ggplot)
# ---------------------------

# Focus plot extent to served tracts only
served_ids <- unique(tract_centroids_served$from_id)
tracts_map <- tracts_diff %>% filter(GEOID %in% served_ids)

p <- ggplot(tracts_map) +
  geom_sf(aes(fill = diff_minutes), linewidth = 0.05, color = alpha("black", 0.2)) +
  scale_fill_gradient2(name = "After - Before (min)",
                       low = "#2166AC", mid = "#FFFFFF", high = "#B2182B",
                       midpoint = 0, na.value = "#DDDDDD", limits = scales::rescale_mid(range(tracts_map$diff_minutes, na.rm = TRUE), mid = 0)) +
  labs(title = "Change in Transit Travel Time to Philadelphia City Hall",
       subtitle = paste0("Departure: ", format(departure_time, "%Y-%m-%d %H:%M"), "  | Modes: ", paste(modes, collapse = "+")),
       caption = "Data: GTFS (SEPTA) before/after cuts, OSM; Analysis: r5r") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

print(p)

gg_path <- file.path(outputs_dir, "travel_time_diff_map.png")
ggsave(gg_path, p, width = 10, height = 8, dpi = 300)

message("Map saved: ", gg_path)

# ---------------------------
# 8) Sanity-check summaries
# ---------------------------
summary_tbl <- tracts_map %>%
  st_drop_geometry() %>%
  summarise(
    n_tracts = n(),
    median_before = median(travel_time_before, na.rm = TRUE),
    median_after  = median(travel_time_after,  na.rm = TRUE),
    median_diff   = median(diff_minutes,       na.rm = TRUE),
    p90_increase  = quantile(diff_minutes, 0.9, na.rm = TRUE),
    share_worse   = mean(diff_minutes > 0, na.rm = TRUE)
  )
print(summary_tbl)

# ---------------------------
# Notes / Tips
# ---------------------------
# 1) Ensure the OSM PBF covers the whole service area; consider a 50–100km buffer beyond the tracts.
# 2) r5r reads time zones from GTFS; pick a typical weekday date when the schedules are valid.
# 3) For more robust results, vary departure_time over a window (e.g., 7–10am) and take median.
# 4) If you want population-weighted centroids, join tract-level population and compute weighted centroids.
# 5) If you serve both PA and NJ tracts (e.g., Camden), append those counties via tigris as needed.
# 6) Consider sensitivity runs for max_walk_dist and number_of_breaks.
