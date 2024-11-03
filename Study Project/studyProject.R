# Load the arrow library
library(arrow)
library(ggplot2)
library(sf)

# Load the distinct_LONLAT.parquet file
distinct_LONLAT <- read_parquet("/Users/timlehmann/Downloads/distinct_LONLAT.parquet")

# Load the pre_processed_movement.parquet file
pre_processed_movement <- read_parquet("/Users/timlehmann/Downloads/pre_processed_movement.parquet")

# Greater London Area
greater_London_area <- st_read("/Users/timlehmann/Downloads/statistical-gis-boundaries-london/ESRI/London_Ward.shp")

ggplot(distinct_LONLAT, aes(x = XLON, y = XLAT)) +
  geom_point() +
  labs(title = "Geografische Koordinaten", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Vereinige alle Polygone zu einem einzigen Umriss
london_outline <- st_union(greater_London_area)

# Plot nur der äußeren Grenze
ggplot() +
  geom_sf(data = london_outline, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "Äußere Umrisslinie von London")

distinct_LONLAT_London <- st_intersection(distinct_LONLAT, london_outline)
