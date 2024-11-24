# Libraries laden
library(arrow)
library(sf)
library(dplyr)
library(leaflet)

# Daten vorbereiten (gleicher Workflow wie bisher)
distinct_LONLAT <- read_parquet("C:/Users/lucah/Downloads/distinct_LONLAT.parquet")
pre_processed_movement <- read_parquet("C:/Users/lucah/Downloads/pre_processed_movement.parquet")
greater_London_area <- st_read("C:/Users/lucah/Downloads/StudyProject2024-main/Study Project/data/London Boundaries/London_Ward.shp")

ersterJanuar <- pre_processed_movement[pre_processed_movement$AGG_DAY_PERIOD == "2020-01-10", ]
mergedData <- merge(ersterJanuar, distinct_LONLAT, by = "LONLAT_ID")
filtered_data <- mergedData %>%
  filter(XLAT > 51.275, XLAT < 51.7, XLON > -0.52, XLON < 0.35)

filtered_data_sf <- st_as_sf(filtered_data, coords = c("XLON", "XLAT"), crs = 4326)

shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

data_london <- st_intersection(filtered_data_sf, london_outline_sf)

# Thames-Daten von MapServer laden
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)

# Lokale GeoJSON-Datei laden und CRS anpassen
local_geojson <- st_read("C:/Users/lucah/Downloads/StudyProject2024-main (3)/StudyProject2024-main/export.geojson")
if (st_crs(local_geojson) != st_crs(data_london)) {
  local_geojson <- st_transform(local_geojson, st_crs(data_london))
}

# Interaktive Karte mit grauer Hintergrundkarte
leaflet() %>%
  
  addProviderTiles(providers$CartoDB.Positron, group = "Graue Karte") %>%
  
  # Weitere Layers hinzufügen
  addPolygons(data = london_outline_sf, color = "black", weight = 1, opacity = 0.5, fill = FALSE, group = "London Outline") %>%
  addCircleMarkers(data = data_london, color = ~colorNumeric("YlOrRd", data_london$mean_column)(mean_column),
                   radius = 2, stroke = FALSE, fillOpacity = 0.7, group = "Bewegungsdaten") %>%
  addPolylines(data = linestring_data, color = "blue", weight = 1, opacity = 0.5, group = "Thames Lines") %>%
  addCircleMarkers(data = local_geojson, color = "purple", radius = 3, stroke = FALSE, fillOpacity = 0.8, group = "GeoJSON-Punkte") %>%
  

  addLayersControl(
    baseGroups = c("Graue Karte"),  
    overlayGroups = c("London Outline", "Bewegungsdaten", "Thames Lines", "GeoJSON-Punkte"),
    options = layersControlOptions(collapsed = FALSE)
  )


