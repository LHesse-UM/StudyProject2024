# Libraries laden
library(arrow)
library(sf)
library(dplyr)
library(leaflet)
library(httr)
library(cluster)  # Für k-Means
library(htmltools)

# Daten vorbereiten
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

# Clustering: k-Means
set.seed(123)  # Reproduzierbarkeit
num_clusters <- 4  
data_london$cluster <- kmeans(data_london$mean_column, centers = num_clusters)$cluster
data_london$cluster <- as.factor(data_london$cluster)  

# Thames-Daten von MapServer laden
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)

# Lokale GeoJSON-Datei laden und ggfs. CRS anpassen
local_geojson <- st_read("C:/Users/lucah/Downloads/StudyProject2024-main (3)/StudyProject2024-main/pois_london.geojson")
if (st_crs(local_geojson) != st_crs(data_london)) {
  local_geojson <- st_transform(local_geojson, st_crs(data_london))
}

# Benutzerdefinierte Legendenfunktion
addLegendCustom <- function(map, position, colors, labels, title = NULL) {
  legend_html <- paste0(
    "<div style='background: white; padding: 8px; border-radius: 5px;'>",
    if (!is.null(title)) paste0("<b>", title, "</b><br>"),
    paste0(
      mapply(function(color, label) {
        paste0("<i style='background:", color, ";width:10px;height:10px;float:left;margin-right:8px;opacity:0.7;'></i>", label, "<br>")
      }, colors, labels, SIMPLIFY = FALSE),
      collapse = ""
    ),
    "</div>"
  )
  addControl(map, html = legend_html, position = position)
}

# Farben basierend auf den Clustern definieren
color_palette <- colorFactor(
  palette = c("orange", "yellow", "red", "green"),  # Farben für die Cluster
  domain = data_london$cluster
)

# 100x100 Meter großes Rechteck zu erstellen
create_square <- function(point, size = 150) {
  coords <- st_coordinates(point)
  lon <- coords[1]
  lat <- coords[2]
  
  # Eckpunkte des Rechtecks bestimmen (in Metern)
  bbox <- matrix(
    c(lon - size / 2, lat - size / 2,
      lon + size / 2, lat - size / 2,
      lon + size / 2, lat + size / 2,
      lon - size / 2, lat + size / 2,
      lon - size / 2, lat - size / 2), 
    ncol = 2, byrow = TRUE
  )
  
  # Polygon erstellen
  polygon <- st_polygon(list(bbox)) %>% st_sfc(crs = 3857)
  return(polygon)
}

# Anwendung der Funktion auf alle Punkte von data_london
data_london_transformed <- st_transform(data_london, crs = 3857)

# Rechtecke erzeugen
data_london_rectangles <- st_as_sf(data_london_transformed %>%
                                     rowwise() %>%
                                     mutate(geometry = create_square(geometry))) %>%
  st_transform(crs = 4326)

print(data_london_rectangles)

leaflet() %>%
  # Graue Hintergrundkarte hinzufügen
  addProviderTiles(providers$CartoDB.Positron, group = "Graue Karte") %>%
  
  # London Outline hinzufügen
  addPolygons(data = london_outline_sf, color = "black", weight = 1, opacity = 0.8, fill = FALSE, group = "London Outline") %>%
  
  # Rechtecke für Bewegungsdaten hinzufügen 
  addPolygons(data = data_london_rectangles,
              color = ~color_palette(cluster),  
              fillOpacity = 0.15,  
              opacity = 0.15,      
              weight = 1, group = "Average Movement Data") %>%
  
  # Thames-Daten hinzufügen
  addPolylines(data = linestring_data, color = "blue", weight = 1, opacity = 0.5, group = "Thames Lines") %>%
  
  # Lokale GeoJSON-Punkte hinzufügen
  addPolygons(data=local_geojson, color = ~case_when(
    type == 1 ~ "#FF6347",
    type == 2 ~ "#4682B4",
    type == 3 ~ "#32CD32",
    type == 4 ~ "#FFD700"
  ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
  addLegend(position = "bottomright", colors = c("#FF6347", "#4682B4", "#32CD32", "#FFD700"), labels = c("Bus stops", "Subway stations", "Football stadiums", "Other big arenas"), title = "All Types of POIs in London") %>%
  
  # Layer-Toggle hinzufügen
  addLayersControl(
    overlayGroups = c("London Outline", "Average Movement Data", "Thames River", "Bus Stations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Cluster-Legende hinzufügen
  addLegend(
    position = "bottomright",
    pal = color_palette,
    values = data_london$cluster,
    title = "Average Movement Data",
    opacity = 0.7
  ) %>%
  
  # Benutzerdefinierter Titel mit einem separaten HTML-Block
  htmlwidgets::prependContent(
    htmltools::tags$div(
      style = "position: absolute; top: 10px; left: 50%; transform: translateX(-50%); 
               z-index: 999; background: transparent; padding: 10px; border-radius: 5px;",
      htmltools::tags$h2("London Movement Data on January 1, 2020")
    )
  )
