library(tidytransit)
library(tidyverse)
library(sf)
library(lubridate)

# Load GTFS data
cut<- read_gtfs("data/cut.zip")
restore<- read_gtfs("data/restore.zip")

### Peak time analysis
## Cut analysis
gt <- cut
the_date <- as.Date("2025-09-29")  # set the typical day

active_ids <- gt$.$dates_services %>%
  filter(date == the_date) %>%
  distinct(service_id) %>%
  pull(service_id)

hours_freq_peak <- gt %>%
  get_route_frequency(
    start_time  = "07:30:00",
    end_time    = "09:30:00",
    service_ids = active_ids
  )

nrow(hours_freq_peak); length(unique(gt$routes$route_id))
setdiff(gt$routes$route_id, hours_freq_peak$route_id)
# Return headway in seconds.

# rename all the columns to have a _cut suffix
hours_freq_cut <- hours_freq_peak %>%
  rename_with(~ paste0(., "_cut"), -route_id)

## Restore service analysis
active_ids <- restore$.$dates_services %>%
  filter(date == the_date) %>%
  distinct(service_id) %>%
  pull(service_id)

peak_freq_restore <- restore %>%
  get_route_frequency(
    start_time  = "07:30:00",
    end_time    = "09:30:00",
    service_ids = active_ids
  )

nrow(peak_freq_restore); length(unique(restore$routes$route_id))
setdiff(restore$routes$route_id, peak_freq_restore$route_id)

# rename all the columns to have a _restore suffix
hours_freq_restore <- peak_freq_restore %>%
  rename_with(~ paste0(., "_restore"), -route_id)

## Comparison Analysis
# Join the two datasets by route_id
freq_comparison <- hours_freq_cut %>%
  full_join(hours_freq_restore, by = "route_id")
# turn all NA into 0
freq_comparison[is.na(freq_comparison)] <- 0

# Calculate the difference in headway
freq_comparison <- freq_comparison %>%
  mutate(
    median_headways_restore = if_else(
      median_headways_cut == 0,
      -median_headways_restore,
      median_headways_restore
    ),
    headway_diff = (median_headways_restore - median_headways_cut)/60
  )

ggplot(freq_comparison, aes(x = headway_diff)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Peak Hours Headway Differences (Restore - Cut)",
    x = "Headway Difference (minutes)",
    y = "Frequency"
  ) +
  theme_minimal()



### miday analysis
## Cut analysis

midday_freq_cut <- cut %>%
  get_route_frequency(
    start_time  = "11:00:00",
    end_time    = "14:00:00",
    service_ids = active_ids
  )
# rename all the columns to have a _cut suffix
midday_freq_cut <- midday_freq_cut %>%
  rename_with(~ paste0(., "_cut"), -route_id)
## Restore analysis
midday_freq_restore <- restore %>%
  get_route_frequency(
    start_time  = "11:00:00",
    end_time    = "14:00:00",
    service_ids = active_ids
  )
# rename all the columns to have a _restore suffix
midday_freq_restore <- midday_freq_restore %>%
  rename_with(~ paste0(., "_restore"), -route_id)

## Comparison Analysis
# Join the two datasets by route_id
midday_freq_comparison <- midday_freq_cut %>%
  full_join(midday_freq_restore, by = "route_id")
# turn all NA into 0
midday_freq_comparison[is.na(midday_freq_comparison)] <- 0
# Calculate the difference in headway
midday_freq_comparison <- midday_freq_comparison %>%
  mutate(
    median_headways_restore = if_else(
      median_headways_cut == 0,
      -median_headways_restore,
      median_headways_restore
    ),
    headway_diff = (median_headways_restore - median_headways_cut)/60,
  )
ggplot(midday_freq_comparison, aes(x = headway_diff)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Midday Headway Differences (Restore - Cut)",
    x = "Headway Difference (minutes)",
    y = "Frequency"
  ) +
  theme_minimal()


### Trip count comparison during peak hours

library(hms)

# inputs you already have from get_route_frequency()
start_time <- "07:30:00"
end_time   <- "09:30:00"


# assume `gtfs` is your tidytransit GTFS object
cuts <- cut$stop_times %>%
  mutate(dep = hms::as_hms(departure_time)) %>%
  filter(dep >= hms::as_hms(start_time), dep < hms::as_hms(end_time)) %>%
  inner_join(cut$trips,  by = "trip_id") %>%
  filter(service_id %in% active_ids) %>%
  inner_join(cut$routes, by = "route_id")

# count trips by route and direction
trips_by_route_dir <- cuts %>%
  distinct(route_id, direction_id, trip_id) %>%
  count(route_id, direction_id, name = "n_trips")

restore_trips <- restore$stop_times %>%
  mutate(dep = hms::as_hms(departure_time)) %>%
  filter(dep >= hms::as_hms(start_time), dep < hms::as_hms(end_time)) %>%
  inner_join(restore$trips,  by = "trip_id") %>%
  filter(service_id %in% active_ids) %>%
  inner_join(restore$routes, by = "route_id")

# count trips by route and direction
restore_trips_by_route_dir <- restore_trips %>%
  distinct(route_id, direction_id, trip_id) %>%
  count(route_id, direction_id, name = "n_trips")

# join the two datasets
trips_comparison <- trips_by_route_dir %>%
  full_join(restore_trips_by_route_dir, by = c("route_id", "direction_id"), suffix = c("_cut", "_restore"))
# replace NAs with 0
trips_comparison[is.na(trips_comparison)] <- 0

trips_comparison <- trips_comparison %>%
  mutate(trips_diff = n_trips_restore - n_trips_cut)

## nonpeak
start_time <- "11:00:00"
end_time   <- "14:00:00"

cuts_non_peak <- cut$stop_times %>%
  mutate(dep = hms::as_hms(departure_time)) %>%
  filter(dep >= hms::as_hms(start_time), dep < hms::as_hms(end_time)) %>%
  inner_join(cut$trips,  by = "trip_id") %>%
  filter(service_id %in% active_ids) %>%
  inner_join(cut$routes, by = "route_id")

# count trips by route and direction
trips_by_route_dir_non_peak <- cuts_non_peak %>%
  distinct(route_id, direction_id, trip_id) %>%
  count(route_id, direction_id, name = "n_trips")

restore_trips_non_peak <- restore$stop_times %>%
  mutate(dep = hms::as_hms(departure_time)) %>%
  filter(dep >= hms::as_hms(start_time), dep < hms::as_hms(end_time)) %>%
  inner_join(restore$trips,  by = "trip_id") %>%
  filter(service_id %in% active_ids) %>%
  inner_join(restore$routes, by = "route_id")

# count trips by route and direction
restore_trips_by_route_dir_non_peak <- restore_trips_non_peak %>%
  distinct(route_id, direction_id, trip_id) %>%
  count(route_id, direction_id, name = "n_trips")
# join the two datasets
trips_comparison_non_peak <- trips_by_route_dir_non_peak %>%
  full_join(restore_trips_by_route_dir_non_peak, by = c("route_id", "direction_id"), suffix = c("_cut", "_restore"))
# replace NAs with 0
trips_comparison_non_peak[is.na(trips_comparison_non_peak)] <- 0
trips_comparison_non_peak <- trips_comparison_non_peak %>%
  mutate(trips_diff = n_trips_restore - n_trips_cut)

ggplot(trips_comparison, aes(x = trips_diff)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Peak Hours Trip Count Differences (Restore - Cut)",
    x = "Trip Count Difference",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(trips_comparison_non_peak, aes(x = trips_diff)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Non-Peak Hours Trip Count Differences (Restore - Cut)",
    x = "Trip Count Difference",
    y = "Frequency"
  ) +
  theme_minimal()



