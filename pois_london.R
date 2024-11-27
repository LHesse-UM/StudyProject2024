# Visualization London POIs
if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")
if (!require("htmlwidgets")) install.packages("htmlwidgets")
if (!require("httr")) install.packages("httr")

library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)
library(httr)


pois_london <- st_read("data/POIs London/pois_london.shp")
anzahl_types <- pois_london %>% 
  group_by(type) %>% 
  summarise(anzahl = n())
print(anzahl_types)

bbox <- st_bbox(pois_london)
buffer_factor <- 0.3
bbox_small <- list(
  xmin = bbox[[1]] + (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymin = bbox[[2]] + (bbox[[4]] - bbox[[2]]) * buffer_factor,
  xmax = bbox[[3]] - (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymax = bbox[[4]] - (bbox[[4]] - bbox[[2]]) * buffer_factor
)

##### London outline #####
greater_London_area <- st_read("data/London Boundaries/London_Ward.shp")
shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

##### Thames Line #####
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)

'
for (i in 1:4) {
  typ_daten <- subset(pois_london, type == i)
  if (nrow(typ_daten) > 0) {
    karte <- leaflet(data = typ_daten) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      fitBounds(bbox_small$xmin, bbox_small$ymin, bbox_small$xmax, bbox_small$ymax) %>% 
      addPolygons(color = ~case_when(
        type == 1 ~ "#FF6347",
        type == 2 ~ "#4682B4",
        type == 3 ~ "#32CD32",
        type == 4 ~ "#FFD700"
      ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
      addLegend(position = "bottomright", colors = c("#FF6347", "#4682B4", "#32CD32", "#FFD700"), labels = c("Bus stops", "Subway stations", "Football stadiums", "Other big arenas"), title = paste("Type", i, "POIs in London"))
    
    # Speichern der Karte als HTML-Datei
    saveWidget(karte, file = paste0("data/maps/karte_typ_", i, ".html"), selfcontained = TRUE)
  }
}
'

karte_alle_types <- leaflet(data = pois_london) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = london_outline_sf, color = "black", weight = 1, opacity = 0.8, fill = FALSE, group = "London Outline") %>%
  addPolylines(data = linestring_data, color = "blue", weight = 1, opacity = 0.5, group = "Thames Lines") %>%
  fitBounds(bbox_small$xmin, bbox_small$ymin, bbox_small$xmax, bbox_small$ymax) %>% 
  addPolygons(color = ~case_when(
    type == 1 ~ "#333333",
    type == 2 ~ "#666666",
    type == 3 ~ "#104E8B",
    type == 4 ~ "#1E90FF",
    type == 5 ~ "#8B4500",
    type == 6 ~ "#66CD00"
  ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
  addLegend(position = "bottomright", colors = c("#333333", "#666666", "#104E8B", "#1E90FF", "#8B4500", "#66CD00"), labels = c("Bus stations", "Subway stations", "Football stadiums", "Other big stadiums", "Sightseeings", "Parks"), title = "All Types of POIs in London")

saveWidget(karte_alle_types, file = "data/maps/karte_alle_types.html")
