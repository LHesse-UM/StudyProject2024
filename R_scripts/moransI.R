library(dplyr)
library(lubridate)
library(spdep)
library(sf)
library(leaflet)
library(htmlwidgets)

# Sicherstellen, dass `time` als day-of-year in `mergedData2` existiert
mergedData2 <- mergedData2 %>%
  mutate(time = yday(AGG_DAY_PERIOD))  # `AGG_DAY_PERIOD` als Datum in day-of-year konvertieren


# Transformieren
mergedData2_sf <- st_as_sf(mergedData2, coords = c("XLON", "XLAT"), crs = 4326)

shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

data_london <- st_intersection(mergedData2_sf, london_outline_sf)

# Intervalle definieren
intervalle <- data.frame(
  intervall = c("Pre_Corona", "First_Lockdown_UK", "Return_to_relaxing_restrictions", 
                "Three_Tier_System", "Second_Lockdown_UK", "End_Second_Lockdown", "Tier_4_London"),
  start = c(0, 85, 130, 287, 309, 336, 355),
  end = c(84, 129, 286, 308, 335, 354, 365)
)

# DataFrames für jedes Intervall erstellen
Pre_Corona <- data_london %>%
  filter(!is.na(time) & time >= 0 & time <= 84)

First_Lockdown_UK <- data_london %>%
  filter(!is.na(time) & time >= 85 & time <= 129)

Return_to_relaxing_restrictions <- data_london %>%
  filter(!is.na(time) & time >= 130 & time <= 286)

Three_Tier_System <- data_london %>%
  filter(!is.na(time) & time >= 287 & time <= 308)

Second_Lockdown_UK <- data_london %>%
  filter(!is.na(time) & time >= 309 & time <= 335)

End_Second_Lockdown <- data_london %>%
  filter(!is.na(time) & time >= 336 & time <= 354)

Tier_4_London <- data_london %>%
  filter(!is.na(time) & time >= 355 & time <= 365)

##### calc mean for intervals
# Funktion: Mittelwert pro Punkt berechnen
calculate_means_per_point <- function(data_sf, interval_name) {
  data_sf %>%
    group_by(geometry) %>%  # Gruppieren nach Geometrie
    summarise(
      mean_value = mean(mean_column, na.rm = TRUE),  # Mittelwert berechnen
      n = n()  # Anzahl der Beobachtungen
    ) %>%
    ungroup() %>%
    mutate(interval = interval_name)  # Intervallnamen hinzufügen
}

# Ergebnisse für jedes Intervall berechnen und in eigene DataFrames speichern
Pre_Corona_means <- calculate_means_per_point(Pre_Corona, "Pre_Corona")
First_Lockdown_UK_means <- calculate_means_per_point(First_Lockdown_UK, "First_Lockdown_UK")
Return_to_relaxing_restrictions_means <- calculate_means_per_point(Return_to_relaxing_restrictions, "Return_to_relaxing_restrictions")
Three_Tier_System_means <- calculate_means_per_point(Three_Tier_System, "Three_Tier_System")
Second_Lockdown_UK_means <- calculate_means_per_point(Second_Lockdown_UK, "Second_Lockdown_UK")
End_Second_Lockdown_means <- calculate_means_per_point(End_Second_Lockdown, "End_Second_Lockdown")
Tier_4_London_means <- calculate_means_per_point(Tier_4_London, "Tier_4_London")

###### MoransI calc
# 1. data to sf object
# 2. neighborhood and weight matrix
# 3. morans I

# Funktion: Berechnung des lokalen Moran's I
calculate_local_morans_i <- function(data_sf, interval_name) {
  coords <- st_coordinates(data_sf)  # Extrahiere Koordinaten
  neighbors <- knearneigh(coords, k = 4)  # 4 nächste Nachbarn
  listw <- nb2listw(knn2nb(neighbors), style = "W")  # Gewichtsmatrix erstellen
  
  # Lokales Moran's I berechnen
  local_moran <- localmoran(data_sf$mean_value, listw)
  
  # Ergebnisse hinzufügen
  data_sf <- data_sf %>%
    mutate(
      local_moran_i = local_moran[, 1],  # Lokales Moran's I
      p_value = local_moran[, 5],       # P-Wert
      interval = interval_name          # Intervallname
    )
  
  return(data_sf)
}

# Funktion: Berechnung des globalen Moran's I
calculate_global_morans_i <- function(data_sf) {
  coords <- st_coordinates(data_sf)  # Extrahiere Koordinaten
  neighbors <- knearneigh(coords, k = 4)  # 4 nächste Nachbarn
  listw <- nb2listw(knn2nb(neighbors), style = "W")  # Gewichtsmatrix erstellen
  
  # Globales Moran's I berechnen
  global_moran <- moran.test(data_sf$mean_value, listw)
  
  return(list(
    estimate = global_moran$estimate[1],  # Moran's I-Wert
    p_value = global_moran$p.value        # P-Wert
  ))
}

# Liste der Intervalle
intervals <- list(
  list(data = Pre_Corona_means, name = "Pre_Corona"),
  list(data = First_Lockdown_UK_means, name = "First_Lockdown_UK"),
  list(data = Return_to_relaxing_restrictions_means, name = "Return_to_relaxing_restrictions"),
  list(data = Three_Tier_System_means, name = "Three_Tier_System"),
  list(data = Second_Lockdown_UK_means, name = "Second_Lockdown_UK"),
  list(data = End_Second_Lockdown_means, name = "End_Second_Lockdown"),
  list(data = Tier_4_London_means, name = "Tier_4_London")
)

# Ergebnisse berechnen
results_list <- lapply(intervals, function(interval) {
  calculate_local_morans_i(interval$data, interval$name)
})

# Kombiniere alle Ergebnisse in ein DataFrame
combined_results <- do.call(rbind, results_list)

# Berechnung des globalen Moran's I für jedes Intervall
global_morans_results <- lapply(intervals, function(interval) {
  global_moran <- calculate_global_morans_i(interval$data)
  list(
    interval = interval$name,
    morans_i = global_moran$estimate,
    p_value = global_moran$p_value
  )
})

# Globale Min-Max-Skalierung
global_min <- min(combined_results$local_moran_i, na.rm = TRUE)
global_max <- max(combined_results$local_moran_i, na.rm = TRUE)

# Normalisierung auf [-1, 1] für alle Intervalle
combined_results <- combined_results %>%
  mutate(
    standardized_local_moran_i = (local_moran_i - global_min) / (global_max - global_min) * 2 - 1
  )

color_scale <- colorNumeric(
  palette = c("blue", "grey", "red"),
  domain = c(-1, 1),
  na.color = "transparent"
)

for (interval in intervals) {
  interval_name <- interval$name
  data_sf <- combined_results %>% filter(interval == interval_name)
  
  global_moran <- global_morans_results[[which(sapply(global_morans_results, function(x) x$interval) == interval_name)]]
  morans_i_value <- round(global_moran$morans_i, 3)
  
  significant_data <- data_sf %>% filter(p_value < 0.05)
  non_significant_data <- data_sf %>% filter(p_value >= 0.05)
  
  # Map with all points
  map_all <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = significant_data,
      radius = 2,
      fillColor = ~color_scale(standardized_local_moran_i),
      fillOpacity = 0.8,
      weight = 0.3,
      popup = ~paste(
        "<strong>Longitude:</strong>", st_coordinates(geometry)[,1], "<br>",
        "<strong>Latitude:</strong>", st_coordinates(geometry)[,2], "<br>",
        "<strong>Standardized Local Moran's I:</strong>", round(standardized_local_moran_i, 3)
      )
    ) %>%
    addCircleMarkers(
      data = non_significant_data,
      radius = 0.5,
      fillColor = "lightgrey",
      fillOpacity = 0.05,
      color = "lightgrey",
      weight = 0.1
    ) %>%
    addLegend(
      "bottomright",
      pal = color_scale,
      values = c(-1, 1),
      title = paste("Standardized Local Moran's I:", interval_name),
      opacity = 1
    ) %>%
    addControl(
      paste0(
        "<b>Global Moran's I:</b> ", morans_i_value
      ),
      position = "topright"
    )
  
  saveWidget(map_all, file = paste0(interval_name, "_all_points_morans_i_map.html"))
  
  # Map with only significant points
  map_significant <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(
      data = significant_data,
      radius = 2,
      fillColor = ~color_scale(standardized_local_moran_i),
      fillOpacity = 0.8,
      weight = 0.3,
      popup = ~paste(
        "<strong>Longitude:</strong>", st_coordinates(geometry)[,1], "<br>",
        "<strong>Latitude:</strong>", st_coordinates(geometry)[,2], "<br>",
        "<strong>Standardized Local Moran's I:</strong>", round(standardized_local_moran_i, 3)
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = color_scale,
      values = c(-1, 1),
      title = paste("Standardized Local Moran's I (Significant Points):", interval_name),
      opacity = 1
    ) %>%
    addControl(
      paste0(
        "<b>Global Moran's I:</b> ", morans_i_value
      ),
      position = "topright"
    )
  
  saveWidget(map_significant, file = paste0(interval_name, "_significant_points_morans_i_map.html"))
}
