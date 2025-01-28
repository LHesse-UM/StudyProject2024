library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(htmlwidgets)


significant_points <- combined_results %>%
  filter(p_value < 0.05)

ggplot(significant_points) +
  geom_sf(aes(geometry = geometry, color = cluster), size = 1) +
  labs(title = "Signifikante Punkte (p < 0.05)",
       subtitle = "Lokale Moran's I Werte für Pre-Corona") +
  theme_minimal()


unique_geometries <- significant_points %>%
  distinct(geometry)

# Häufigkeit jeder Geometrie berechnen
geometry_frequency <- significant_points %>%
  group_by(geometry) %>%
  summarise(count = n())

# Sortieren nach Häufigkeit
geometry_frequency <- geometry_frequency %>%
  arrange(desc(count))

# Vorschau
head(geometry_frequency)



# Häufigkeit jeder Geometrie berechnen
geometry_frequency <- significant_points %>%
  group_by(geometry) %>%
  summarise(count = n()) %>%
  ungroup()

# Nur Geometrien mit count == 7
geometry_with_seven <- geometry_frequency %>%
  filter(count == 7)

# Vorschau
print(geometry_with_seven)


geometry_with_seven_sf <- st_sf(geometry_with_seven)
ggplot(geometry_with_seven_sf) +
  geom_sf(color = "red", size = 1) +
  labs(title = "Geometrien mit Häufigkeit 7") +
  theme_minimal()


signi_points_with_all <- significant_points %>%
  st_join(geometry_with_seven, join = st_equals) %>%
  filter(!is.na(count))  # Behalten Sie nur Punkte, die in geometry_with_seven enthalten sind
head(signi_points_with_all)

signi_points_with_all_not_sf <- signi_points_with_all %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],  # Longitude
    lat = st_coordinates(geometry)[, 2]   # Latitude
  ) %>%
  arrange(lon, lat, interval)

point_comparison <- signi_points_with_all_not_sf %>%
  group_by(lon, lat) %>%
  summarise(
    min_moran_i = min(scaled_local_moran_i, na.rm = TRUE),
    max_moran_i = max(scaled_local_moran_i, na.rm = TRUE),
    mean_moran_i = mean(scaled_local_moran_i, na.rm = TRUE),
    range_moran_i = max_moran_i - min_moran_i,  # Spannweite der Werte
    consistency = ifelse(all(scaled_local_moran_i > 0), "Hotspot",
                         ifelse(all(scaled_local_moran_i < 0), "Coldspot", "Fluctuating"))
  )


ggplot(signi_points_with_all_not_sf, aes(x = interval, y = scaled_local_moran_i, group = interaction(lon, lat))) +
  geom_line(aes(color = interaction(lon, lat)), size = 1) +
  labs(
    title = "Veränderung der scaled_local_moran_i-Werte über Intervalle",
    x = "Intervall",
    y = "Scaled Local Moran's I",
    color = "Punkte (Koordinaten)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 

hotspot_analysis <- signi_points_with_all_not_sf %>%
  group_by(lon, lat) %>%
  summarise(
    consistency = ifelse(all(scaled_local_moran_i > 0), "Consistent Hotspot",
                         ifelse(all(scaled_local_moran_i < 0), "Consistent Coldspot", "Fluctuating"))
  )
ggplot(hotspot_analysis, aes(x = lon, y = lat, color = consistency)) +
  geom_point(size = 1) +
  labs(
    title = "Hotspot- und Coldspot-Kategorien über Intervalle",
    x = "Longitude",
    y = "Latitude",
    color = "Konsistenz"
  ) +
  theme_minimal()

coords <- coords %>%
  st_drop_geometry() %>%  # Entfernt die `geometry`-Spalte
  select(lon, lat) %>%    # Nur `lon` und `lat` auswählen
  distinct() %>%          # Eindeutige Werte
  na.omit()     
# Konvertieren in eine numerische Matrix
coords_matrix <- as.matrix(coords)

clusters <- dbscan(coords_matrix, eps = 0.01, minPts = 5)
print(clusters)



library(tidyr)
signi_points_with_all_not_sf <- signi_points_with_all_not_sf %>%
  mutate(
    lon = st_coordinates(geometry)[, 1],  # Longitude
    lat = st_coordinates(geometry)[, 2]   # Latitude
  )
signi_points_with_all_not_sf <- signi_points_with_all_not_sf %>%
  st_drop_geometry()

moran_matrix <- signi_points_with_all_not_sf %>%
  select(lon, lat, interval, scaled_local_moran_i) %>%
  pivot_wider(
    names_from = interval,               # Intervalle als Spalten
    values_from = scaled_local_moran_i,  # Werte für scaled_local_moran_i
    values_fill = 0                      # Fehlende Werte durch 0 ersetzen
  )

# Vorschau
head(moran_matrix)
coords_matrix <- moran_matrix %>%
  select(-lon, -lat) %>%
  as.matrix()

# Überprüfen
head(coords_matrix)
clusters <- dbscan(coords_matrix, eps = 0.125, minPts = 4)
for (eps_value in seq(0.001, 0.2, by = 0.002)) {
  clusters <- dbscan(coords_matrix, eps = eps_value, minPts = 4)
  cat("eps:", eps_value, "Anzahl Cluster:", length(unique(clusters$cluster)), "\n")
}

moran_matrix$cluster <- clusters$cluster
ggplot(moran_matrix, aes(x = lon, y = lat, color = as.factor(cluster))) +
  geom_point(size = 1) +
  labs(
    title = "Cluster basierend auf scaled_local_moran_i-Werten über Intervalle",
    x = "Longitude",
    y = "Latitude",
    color = "Cluster"
  ) +
  theme_minimal()


expanded_data <- signi_points_with_all_not_sf %>%
  left_join(moran_matrix %>% select(lon, lat, cluster), by = c("lon", "lat"))


cluster_trends <- expanded_data %>%
  group_by(interval, cluster.y) %>%
  summarise(mean_scaled_moran_i = mean(scaled_local_moran_i, na.rm = TRUE))

# Vorschau der Cluster-Trends
head(cluster_trends)


# Visualisieren der Trends
ggplot(cluster_trends, aes(x = interval, y = mean_scaled_moran_i, group = cluster.y, color = as.factor(cluster.y))) +
  geom_line(size = 1) +
  labs(
    title = "Cluster-Trends der scaled_local_moran_i-Werte über Intervalle",
    x = "Intervall",
    y = "Durchschnitt Scaled Local Moran's I",
    color = "Cluster"
  ) +
  theme_minimal()
ggplot(expanded_data, aes(x = interval, y = scaled_local_moran_i, group = interaction(lon, lat), color = as.factor(cluster.y))) +
  geom_line(alpha = 0.3) +
  labs(
    title = "Veränderung der scaled_local_moran_i-Werte über Intervalle für alle Punkte",
    x = "Intervall",
    y = "Scaled Local Moran's I",
    color = "Cluster"
  ) +
  theme_minimal()

clustered_points <- moran_matrix %>%
  select(lon, lat, cluster)

# Vorschau
head(clustered_points)
clustered_points_sf <- st_as_sf(clustered_points, coords = c("lon", "lat"), crs = 4326)
ggplot(clustered_points_sf) +
  geom_sf(aes(color = as.factor(cluster)), size = 2, alpha = 0.8) +
  scale_color_viridis_d() +
  labs(
    title = "Cluster basierend auf scaled_local_moran_i-Werten",
    color = "Cluster"
  ) +
  theme_minimal()
cluster_sizes <- moran_matrix %>%
  group_by(cluster) %>%
  summarise(size = n())  # Anzahl der Punkte pro Cluster

# Verknüpfen der Clustergrößen mit den Daten
moran_matrix <- moran_matrix %>%
  left_join(cluster_sizes, by = "cluster")

filtered_clusters <- moran_matrix %>%
  filter(cluster != 0)


# Vorschau der Daten mit Clustergrößen
ggplot(filtered_clusters, aes(x = lon, y = lat, color = as.factor(cluster), size = size)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis_d() +
  labs(
    title = "Visualisierung der Cluster (ohne Cluster 0)",
    x = "Longitude",
    y = "Latitude",
    color = "Cluster",
    size = "Clustergröße"
  ) +
  theme_minimal()

filtered_clusters <- moran_matrix %>%
  filter(cluster != 0 & cluster != 1)
filtered_clusters_sf <- st_as_sf(filtered_clusters, coords = c("lon", "lat"), crs = 4326)


poi_geojson_sf <- st_as_sf(poi_geojson)

leaflet(filtered_clusters_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 5,
    color = ~factor(cluster),
    popup = ~paste("Cluster:", cluster),
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright",
    pal = colorFactor("viridis", domain = filtered_clusters_sf$cluster),
    values = ~cluster,
    title = "Cluster"
  )
cluster_palette <- colorFactor("viridis", domain = filtered_clusters_sf$cluster)

# Interaktive Karte
leaflet(filtered_clusters_sf) %>%
  addTiles() %>%
  addPolygons(
    data = poi_geojson_sf,
    color = "blue",
    weight = 2,
    opacity = 0.6,
    popup = ~paste("Name:", Name)  # Popup mit einem Attribut (z. B. Name)
  ) %>%
  addCircleMarkers(
    radius = 5,
    color = ~cluster_palette(cluster),  # Farbpalette basierend auf Cluster
    popup = ~paste("Cluster:", cluster),  # Popup mit Cluster-Informationen
    fillOpacity = 0.8
  ) %>%
  addLegend(
    "bottomright",
    pal = cluster_palette,
    values = ~cluster,
    title = "Cluster"
  )

filtered_poi_geojson_sf <- poi_geojson_sf %>%
  filter(type %in% c(1, 2))
         
pal <- colorNumeric("viridis", domain = signi_points_with_all$scaled_local_moran_i)

# Interaktive Karte
leaflet(signi_points_with_all) %>%
  addTiles() %>%
  addPolygons(
    data = filtered_poi_geojson_sf,
    color = "blue",
    weight = 2,
    opacity = 0.6,
    popup = ~paste("Name:", Name)  # Popup mit einem Attribut (z. B. Name)
  ) %>%
  addCircleMarkers(
    radius = 1,
    color = ~pal(scaled_local_moran_i),  # Farben basierend auf scaled_local_moran_i
    fillOpacity = 0.8,
    popup = ~paste("scaled_local_moran_i:", round(scaled_local_moran_i, 3))  # Popup mit Werten
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~scaled_local_moran_i,
    title = "Heatmap: scaled_local_moran_i",
    opacity = 1
  )

mean_by_interval <- signi_points_with_all %>%
  group_by(interval) %>%
  summarise(mean_value = mean(mean_value, na.rm = TRUE))

print(mean_by_interval)

pal <- colorNumeric("viridis", domain = signi_points_with_all$mean_value)

# Interaktive Karte
leaflet(signi_points_with_all) %>%
  addTiles() %>%
  addCircleMarkers(
    radius = 1,
    color = ~pal(mean_value),
    fillOpacity = 0.8,
    popup = ~paste("Mean Value:", round(mean_value, 3))
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~mean_value,
    title = "Mean Value"
  )

###### 25.01.25 ######
# 1. Polygon rectangles
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

# Anwendung der Funktion auf alle Punkte von signi_points_with_all
signi_points_with_all_3857 <- st_transform(signi_points_with_all, crs = 3857)
significant_points_3857 <- st_transform(significant_points, crs=3857)

# Rechtecke erzeugen
signi_polygons_with_all <- st_as_sf(signi_points_with_all_3857 %>%
                                     rowwise() %>%
                                     mutate(geometry = create_square(geometry))) %>%
                                     st_transform(crs = 4326)
significant_polygons <- st_as_sf(significant_points_3857 %>%
                                      rowwise() %>%
                                      mutate(geometry = create_square(geometry))) %>%
                                      st_transform(crs = 4326)

intervals <- unique(significant_polygons$interval)
output_dir <- "C:/Users/t.krumrein/IdeaProjects/StudyProject2024/data/moransI/"

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

# Schleife über jedes Intervall
for (interval in intervals) {
  
  # Filter für das aktuelle Intervall
  interval_data <- significant_polygons[significant_polygons$interval == interval, ]
  
  global_moran <- global_morans_results[[which(sapply(global_morans_results, function(x) x$interval) == interval)]]
  morans_i_value <- round(global_moran$morans_i, 3)
  
  # Farbpalette erstellen (für das aktuelle Intervall)
  pal <- colorNumeric(
    palette = rev(c("red", "white", "blue")),
    domain = interval_data$scaled_local_moran_i
  )
  
  # Karte erstellen
  map_scaled_local <- leaflet(interval_data) %>%
    addTiles() %>%
    setView(
      lng = mean(st_coordinates(interval_data)[, 1]), # Mittelwert der Längengrade
      lat = mean(st_coordinates(interval_data)[, 2]), # Mittelwert der Breitengrade
      zoom = 12 # Setzen Sie den gewünschten Zoomlevel (niedriger = weiter herausgezoomt)
    ) %>%
    addPolygons(
      fillColor = ~pal(scaled_local_moran_i),
      weight = 1,
      opacity = 0,
      fillOpacity = 0.7,
      popup = ~paste0(
        "<strong>Mean Value: </strong>", mean_value, "<br>",
        "<strong>Scaled Local Moran's I: </strong>", scaled_local_moran_i, "<br>",
        "<strong>P-Value: </strong>", p_value
      )
    ) %>%
    addPolygons(
      data = poi_geojson_sf,
      color = "black",
      weight = 2,
      opacity = 0.6,
      popup = ~paste("Name:", Name)  # Popup mit einem Attribut (z. B. Name)
    ) %>%
    addPolygons(
      data = linestring_data_polygon,
      color = "black",
      fillColor = "lightgray",
      fillOpacity = 0.8,
      weight = 1,
      opacity = 1,
      group = "Thames Area"
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = interval_data$scaled_local_moran_i,
      title = paste(interval, "Scaled Local Moran's I"),
      opacity = 0.7
    ) %>%
    addControl(
      paste0(
        "<b>Global Moran's I:</b> ", morans_i_value
      ),
      position = "topright"
    )
  
  # Karte speichern
  file_path <- paste0(output_dir, interval, "_significant_points_morans_i_map.html")
  saveWidget(map_scaled_local, file_path, selfcontained = TRUE)
}
# 2. POIs analysis

