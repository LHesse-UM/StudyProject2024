# Load the arrow library
library(arrow)
library(ggplot2)
library(sf)
library(maps)
library(dplyr)
library(httr)

# Load the distinct_LONLAT.parquet file
distinct_LONLAT <- read_parquet("C:/Users/lucah/Downloads/distinct_LONLAT.parquet")


# Load the pre_processed_movement.parquet file
pre_processed_movement <- read_parquet("C:/Users/lucah/Downloads/pre_processed_movement.parquet")

# Greater London Area
greater_London_area <- st_read("C:/Users/lucah/Downloads/StudyProject2024-main/Study Project/data/London Boundaries/London_Ward.shp")


ersterJanuar <- pre_processed_movement[pre_processed_movement$AGG_DAY_PERIOD == "2020-01-01", ]
mergedData <- merge(ersterJanuar, distinct_LONLAT, by = "LONLAT_ID")

st_crs(greater_London_area)
shape_transformed <- st_transform(greater_London_area, crs = 4326)

filtered_data <- mergedData %>%
  filter(XLAT > 51.275, XLAT < 51.7, XLON > -0.52, XLON < 0.35)



url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"

# Abfrageparameter erstellen (angepasst für GeoJSON)
params <- list(
  where = "1=1",                  # Abfragebedingung, um alle Features abzurufen
  outFields = "*",                 # Alle Felder abrufen
  f = "geojson"                    # Format GeoJSON
)

# GeoJSON-Daten vom ArcGIS Map Server abrufen
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)
ggplot() +
  geom_point(data = filtered_data, aes(x = XLON, y = XLAT, color = mean_column), size = 0.01, alpha = 1) +
  scale_color_continuous(low = "green", high = "red", name = "Durchschnittliche Bewegung") + # Anpassung der Farbskala basierend auf 'mean_column'
  labs(title = "Aktivität in London am 01.01.2020", x = "Longitude", y = "Latitude") +
  geom_sf(data = linestring_data, color = "blue", size = 1, alpha = 0.8) +
  geom_sf(data = london_outline, fill = NA, color = "black", size = 0.1, alpha = 0.1) +
  theme_minimal()








#################################################################################################################
# IRRELEVANT GLAUBE ICH



ggplot(distinct_LONLAT, aes(x = XLON, y = XLAT)) +
  geom_point() +
  labs(title = "Geografische Koordinaten", x = "Longitude", y = "Latitude") +
  theme_minimal()

# Vereinige alle Polygone zu einem einzigen Umriss
london_outline <- st_union(shape_transformed)

# Plot nur der äußeren Grenze
ggplot() +
  geom_sf(data = london_outline, color = "black", fill = NA) +
  theme_minimal() +
  labs(title = "Äußere Umrisslinie von London")

distinct_LONLAT_London <- st_intersection(distinct_LONLAT, london_outline)
