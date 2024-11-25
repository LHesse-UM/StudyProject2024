# Visualization London POIs

# Benötigte Pakete installieren und laden
if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

library(sf)
library(leaflet)
library(dplyr)
library(htmlwidgets)

# Shapefile einlesen
# Ersetze "/data/POIs London/pois_london.shp" mit dem tatsächlichen Pfad zur Shapefile
daten <- st_read("data/POIs London/pois_london.shp")
print(daten)
anzahl_types <- daten %>% 
  group_by(type) %>% 
  summarise(anzahl = n())
print(anzahl_types)

# bbox
bbox <- st_bbox(daten)
buffer_factor <- 0.3
bbox_small <- list(
  xmin = bbox[[1]] + (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymin = bbox[[2]] + (bbox[[4]] - bbox[[2]]) * buffer_factor,
  xmax = bbox[[3]] - (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymax = bbox[[4]] - (bbox[[4]] - bbox[[2]]) * buffer_factor
)

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

karte_alle_types <- leaflet(data = daten) %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  fitBounds(bbox_small$xmin, bbox_small$ymin, bbox_small$xmax, bbox_small$ymax) %>% 
  addPolygons(color = ~case_when(
    type == 1 ~ "#FF6347",
    type == 2 ~ "#4682B4",
    type == 3 ~ "#32CD32",
    type == 4 ~ "#FFD700"
  ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
  addLegend(position = "bottomright", colors = c("#FF6347", "#4682B4", "#32CD32", "#FFD700"), labels = c("Bus stops", "Subway stations", "Football stadiums", "Other big arenas"), title = "All Types of POIs in London")


# Speichere die Karte für alle POI-Typen
saveWidget(karte_alle_types, file = "data/maps/karte_alle_types.html", selfcontained = TRUE)
