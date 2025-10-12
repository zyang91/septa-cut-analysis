library(tidyverse)
library(lubridate)
library(sf)
library(tigris)
pa_state <- "PA"
pa_counties <- c("Philadelphia", "Delaware", "Montgomery", "Bucks", "Chester")
tracts <- tracts(state = pa_state, county = pa_counties, year = 2023, cb = TRUE, class = "sf") |>
  st_transform(4326)

tracts<- tracts |>
  select(GEOID, NAME, geometry)

# Read in the data
after_cut_peaks <- read_csv("data/outputs/after_cuts_peak.csv")%>%
  select(from_id,travel_time_p50)
after_cut_offpeaks <- read_csv("data/outputs/after_cuts.csv")%>%
  select(from_id,travel_time_p50)
before_cut_peaks <- read_csv("data/outputs/before_cuts.csv")%>%
  select(from_id,travel_time_p50)
before_cut_offpeaks <- read_csv("data/outputs/before_cuts_non_peak.csv")%>%
  select(from_id,travel_time_p50)

peaks <- after_cut_peaks %>%
  rename(after_cut_peaks = travel_time_p50) %>%
  left_join(before_cut_peaks %>%
              rename(before_cut_peaks = travel_time_p50),
            by = "from_id") %>%
  mutate(difference_peaks = before_cut_peaks - after_cut_peaks)
offpeaks <- after_cut_offpeaks %>%
  rename(after_cut_offpeaks = travel_time_p50) %>%
  left_join(before_cut_offpeaks %>%
              rename(before_cut_offpeaks = travel_time_p50),
            by = "from_id") %>%
  mutate(difference_offpeaks = before_cut_offpeaks - after_cut_offpeaks)

# joined to census tract shapefile
peaks<- peaks%>%
  mutate(GEOID=as.character(from_id))
peaks_sf <- tracts %>%
  left_join(peaks, by = "GEOID")
peaks_sf <- peaks_sf %>%
  filter(!is.na(difference_peaks))
offpeaks<- offpeaks%>%
  mutate(GEOID=as.character(from_id))
offpeaks_sf <- tracts %>%
  left_join(offpeaks, by = "GEOID")
offpeaks_sf <- offpeaks_sf %>%
  filter(!is.na(difference_offpeaks))
## plotting

ggplot() + 
  geom_sf(data = peaks_sf, aes(fill = difference_peaks),color="NA") + 
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") + 
  theme_minimal() + 
  labs(title = "Change in Travel Time during Peak Hours", 
       subtitle = "After Cuts - Before Cuts (in minutes)",
       fill = "Travel Time (minutes)") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size = 13, face = "italic"),
        plot.title = element_text(size = 25, hjust= 0.5,face = "bold"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA, size = 0.8),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14))

ggsave("figures/peak_travel_time_change.png", width = 10, height = 8, dpi = 300)

ggplot() + 
  geom_sf(data = offpeaks_sf, aes(fill = difference_offpeaks),color="NA") + 
  scale_fill_viridis_c(option = "plasma", na.value = "lightgrey") + 
  theme_minimal() + 
  labs(title = "Change in Travel Time during Off-Peak Hours", 
       subtitle = "After Cuts - Before Cuts (in minutes)",
       fill = "Travel Time (minutes)") + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_text(size = 13, face = "italic"),
        plot.title = element_text(size = 25, hjust= 0.5,face = "bold"),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "grey", fill = NA, size = 0.8),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14))
ggsave("figures/offpeak_travel_time_change.png", width = 10, height = 8, dpi = 300)
