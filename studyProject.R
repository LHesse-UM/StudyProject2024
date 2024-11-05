# Load the libraries
library(arrow)
library(ggplot2)
library(sf)
library(maps)
library(dplyr)
library(httr)

# Load raw data
distinct_LONLAT <- read_parquet("C:/Users/lucah/Downloads/distinct_LONLAT.parquet")
pre_processed_movement <- read_parquet("C:/Users/lucah/Downloads/pre_processed_movement.parquet")
greater_London_area <- st_read("C:/Users/lucah/Downloads/StudyProject2024-main/Study Project/data/London Boundaries/London_Ward.shp")

###### Preprocessing ######
# filter movement data to 2020-01-01, merge it and filter them to coordinates
ersterJanuar <- pre_processed_movement[pre_processed_movement$AGG_DAY_PERIOD == "2020-01-01", ]
mergedData <- merge(ersterJanuar, distinct_LONLAT, by = "LONLAT_ID")
filtered_data <- mergedData %>%
  filter(XLAT > 51.275, XLAT < 51.7, XLON > -0.52, XLON < 0.35)

filtered_data_sf <- st_as_sf(filtered_data, coords = c("XLON", "XLAT"), crs = 4326)

# transform greater_london_area and create outline
shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

# get movement data only for london outline
data_london <- st_intersection(filtered_data_sf, london_outline_sf)


###### Code Thames from MapServer ######
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)

###### Plot Activity in London Area ######
ggplot() +
  geom_sf(data = data_london, aes(color = mean_column), size = 1, alpha = 0.7) +
  scale_color_continuous(low = "green", high = "red", name = "Durchschnittliche Bewegung") + # Anpassung der Farbskala basierend auf 'mean_column'
  labs(title = "Aktivität in London am 01.01.2020", x = "Longitude", y = "Latitude") +
  geom_sf(data = linestring_data, color = "blue", size = 0.3, alpha = 0.2) +
  geom_sf(data = london_outline, fill = NA, color = "black", size = 0.1, alpha = 0.1) +
  theme_minimal()
