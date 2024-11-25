# Visualization London POIs

# Benötigte Pakete installieren und laden
if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("dplyr")) install.packages("dplyr")

library(sf)
library(leaflet)
library(dplyr)

# Shapefile einlesen
# Ersetze "/data/POIs London/pois_london.shp" mit dem tatsächlichen Pfad zur Shapefile
daten <- st_read("data/POIs London/pois_london.shp")
bbox <- st_bbox(daten)
buffer_factor <- 0.3
bbox_small <- list(
  xmin = bbox[[1]] + (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymin = bbox[[2]] + (bbox[[4]] - bbox[[2]]) * buffer_factor,
  xmax = bbox[[3]] - (bbox[[3]] - bbox[[1]]) * buffer_factor,
  ymax = bbox[[4]] - (bbox[[4]] - bbox[[2]]) * buffer_factor
)

# Angenommen, der Spaltenname für die Typen ist "type" und die Werte sind von 1 bis 4
# Überprüfe die Struktur der Daten, um sicherzustellen, dass die Spalte "type" vorhanden ist
str(daten)

# Mit leaflet alle Typen anzeigen lassen
# Jeder Typ bekommt eine eigene Karte
for (i in 1:3) {
  typ_daten <- subset(daten, type == i)
  if (nrow(typ_daten) > 0) {  # Nur wenn es Daten für den aktuellen Typ gibt
    karte <- leaflet(data = typ_daten) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% # Verwende eine graue OSM-Karte (CartoDB Positron)
      fitBounds(bbox_small$xmin, bbox_small$ymin, bbox_small$xmax, bbox_small$ymax) %>% # Begrenze die Karte auf die kleinere Bounding Box des Shapefiles
      addPolygons(color = ~case_when(
        type == 1 ~ "#FF6347",
        type == 2 ~ "#4682B4",
        type == 3 ~ "#32CD32"
      ), weight = 1, opacity = 1, fillOpacity = 0.5) %>%
      addLegend(position = "bottomright", colors = c("#FF6347", "#4682B4", "#32CD32"), labels = c("Type 1", "Type 2", "Type 3"), title = paste("Type", i, "POIs in London"))
    print(karte)
  }
}
