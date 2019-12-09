
# Load packages
library(tidyverse)
library(sf)
library(osmdata)
library(extrafont)

# Create bounding box for OSM query
philly_bb <- getbb(place_name = "Philadelphia, PA")

# Retrieve all road data from OSM
philly_roads <- opq(philly_bb) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary")) %>%
  osmdata_sf()

# Retrieve Philly bars from OSM
philly_bars <- opq(philly_bb) %>%
  add_osm_feature(key = "amenity", value = "bar") %>%
  osmdata_sf()

# Read data
parking_dat <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

# Convert to sf
p_dat_sf <- parking_dat %>%
  filter(lon > -75.4) %>%
  filter(lat > 39.85) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(month = lubridate::month(issue_datetime, label = TRUE, abbr = FALSE))

# Extract coordinates from ticket data
pts <- st_coordinates(p_dat_sf) %>% as_tibble()

# Extract coordinates from bar data
bar_pts <- st_coordinates(philly_bars$osm_points) %>% as_tibble()

# Nearest neighbor clustering
clustering <- RANN::nn2(bar_pts, pts, k = 1)

pts_final <- cbind(pts, index = clustering$nn.idx) %>%
  mutate(fine = p_dat_sf$fine, month = p_dat_sf$month) %>%
  group_by(index) %>%
  summarise(total_fine = sum(fine, na.rm = TRUE)) %>%
  ungroup()

# Creating plot object
for_map <- bar_pts %>%
  mutate(index = row_number()) %>%
  left_join(pts_final) %>%
  mutate(total_fine = replace_na(total_fine, 0))
  

# Plot
plt <- ggplot() +
  geom_sf(data = philly_roads$osm_lines, color = alpha("white", 0.2)) +
  geom_point(data = for_map, aes(X, Y, size = total_fine), color = "yellow", alpha = 0.5) +
  scale_size(guide = FALSE) +
  labs(title = "BAR PARKING",
       subtitle = "Which bars in Philadelphia, PA have the highest total\nparking ticket fines?",
       caption = "Author: Jason Jones\nSource: https://www.opendataphilly.org/") +
  cowplot::theme_map() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        text = element_text(family = "Segoe UI", color = "white"),
        plot.margin = unit(rep(0.25, 4), units = "in"))


ggsave(filename = "plot.png", plot = plt, device = "png", path = "2019/Week 49/",
       width = 10, height = 12, dpi = 320)
