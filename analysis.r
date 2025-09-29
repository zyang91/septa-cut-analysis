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
    start_time  = "06:00:00",
    end_time    = "10:00:00",
    service_ids = active_ids
  )

nrow(hours_freq); length(unique(gt$routes$route_id))
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
    start_time  = "06:00:00",
    end_time    = "10:00:00",
    service_ids = active_ids
  )

nrow(hours_freq_restore); length(unique(restore$routes$route_id))
setdiff(restore$routes$route_id, hours_freq_restore$route_id)  

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
    headway_diff = median_headways_restore - median_headways_cut,
    trips_diff   = total_departures_restore - total_departures_cut
  )

ggplot(freq_comparison, aes(x = headway_diff)) +
  geom_histogram(binwidth = 400, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Headway Differences (Restore - Cut)",
    x = "Headway Difference (seconds)",
    y = "Frequency"
  ) +
  theme_minimal()

ggplot(freq_comparison, aes(x = trips_diff)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Trips Differences (Restore - Cut)",
    x = "Trips Difference",
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
    headway_diff = median_headways_restore - median_headways_cut,
    trips_diff   = total_departures_restore - total_departures_cut
  )
ggplot(midday_freq_comparison, aes(x = headway_diff)) +
  geom_histogram(binwidth = 400, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Midday Headway Differences (Restore - Cut)",
    x = "Headway Difference (seconds)",
    y = "Frequency"
  ) +
  theme_minimal()
ggplot(midday_freq_comparison, aes(x = trips_diff)) +
  geom_histogram(binwidth = 70, fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Distribution of Midday Trips Differences (Restore - Cut)",
    x = "Trips Difference",
    y = "Frequency"
  ) +
  theme_minimal()
