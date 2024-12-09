# Libraries laden
library(arrow)
library(sf)
library(dplyr)
library(httr)
library(cluster) 
library(leaflet)
library(htmltools)
library(htmlwidgets) 

londonMovData <- mergedData2

days <- c(10, 31, 52, 58, 66, 207, 261, 296, 311, 350)

londonMovDataEventDays <- dplyr::filter(londonMovData, time %in% days)

# Daten filtern für time = 10
data_time_10 <- dplyr::filter(londonMovDataEventDays, time == 10)

londonMovDataEventDays_sf <- st_as_sf(data_time_10, coords = c("XLON", "XLAT"), crs = 4326)
shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

data_london <- st_intersection(londonMovDataEventDays_sf, london_outline_sf)
data_london_sf <- st_as_sf(data_time_10, coords = c("XLON", "XLAT"), crs = 4326)



################################################################################################################################
# Thames-Daten von MapServer laden
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)
linestring_data_polygon <- st_cast(linestring_data, "POLYGON")

######################################################################################################################################

min_value <- min(data_london$mean_column, na.rm = TRUE)
max_value <- max(data_london$mean_column, na.rm = TRUE)
breaks <- c(min_value, 0.4, 0.6, 0.9, 1.1, 1.9, 2.1, max_value)
color_scale <- colorBin(
  palette = c("#bcf7f7", "#6ad5fc", "#589afc", "darkgrey" ,"#FFCC99", "red", "darkred"),  # Farbverlauf
  domain = data_london$mean_column,
  bins = breaks
)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Datenpunkte hinzufügen (unter dem Polygon-Overlay)
  addCircleMarkers(
    data = data_london,
    radius = 2,
    color = ~color_scale(mean_column),  # Farbskala anwenden
    fillOpacity = 0.7,
    stroke = FALSE,
    label = ~paste0("Mean Column: ", round(mean_column, 2))
  ) %>%
  
  # Thames Polygon hinzufügen
  addPolygons(
    data = linestring_data_polygon,
    color = "lightgrey",
    fillColor = "lightgray",
    fillOpacity = 0.8,
    weight = 1,
    opacity = 1,
    group = "Thames Area"
  ) %>%
  
  # London Outline hinzufügen
  addPolygons(
    data = london_outline_sf,
    color = "black",
    weight = 2,
    opacity = 1,
    fillOpacity = 0.0,
    group = "London Outline"
  ) %>%
  
  # Legende hinzufügen
  addLegend(
    pal = color_scale,
    values = data_london$mean_column,
    title = "Relative to England Average",
    opacity = 1
  ) %>%
  
  # Benutzerdefinierter Titel mit einem separaten HTML-Block
  htmlwidgets::prependContent(
    htmltools::tags$div(
      style = "position: absolute; top: 10px; left: 50%; transform: translateX(-50%); 
               z-index: 999; background: transparent; padding: 10px; border-radius: 5px;",
      htmltools::tags$h2("London Movement Data on January 11, 2020")
    )
  ) %>%
  
  # Karte speichern (optional)
  saveWidget("data/London Movement/london_move_11-01.html")
  