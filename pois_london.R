# Visualization London POIs
if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)

daten <- st_read("data/POIs London/pois_london.shp")
print(daten)
anzahl_types <- daten %>% 
  group_by(type) %>% 
  summarise(anzahl = n())
print(anzahl_types)

bbox <- st_bbox(daten)
buffer_factor <- 0.3
bbox_small <- list(
  xmin = bbox[[1]] + (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymin = bbox[[2]] + (bbox[[4]] - bbox[[2]]) * buffer_factor,
  xmax = bbox[[3]] - (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymax = bbox[[4]] - (bbox[[4]] - bbox[[2]]) * buffer_factor
)

greater_London_area <- st_read("data/London Boundaries/London_Ward.shp")
shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

'
for (i in 1:4) {
  typ_daten <- subset(daten, type == i)
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

karte_alle_types <- leaflet(data = daten) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(data = london_outline_sf, color = "black", weight = 1, opacity = 0.8, fill = FALSE, group = "London Outline") %>%
  fitBounds(bbox_small$xmin, bbox_small$ymin, bbox_small$xmax, bbox_small$ymax) %>% 
  addPolygons(color = ~case_when(
    type == 1 ~ "#666666",
    type == 2 ~ "#7F7F7F",
    type == 3 ~ "#008B8B",
    type == 4 ~ "#00CED1",
    type == 5 ~ "#8B4500",
    type == 6 ~ "#32CD32"
  ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
  addLegend(position = "bottomright", colors = c("#666666", "#7F7F7F4", "#008B8B", "#00CED1", "#8B4500", "#32CD32"), labels = c("Bus stations", "Subway stations", "Football stadiums", "Other big stadiums", "Sightseeings", "Parks"), title = "All Types of POIs in London")

saveWidget(karte_alle_types, file = "data/maps/karte_alle_types.html")
