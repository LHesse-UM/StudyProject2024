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
calculate_means_per_point <- function(data, interval_name) {
  data %>%
    group_by(XLON, XLAT) %>%  # Gruppieren nach Koordinaten
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

# Optional: In sf-Objekte konvertieren, falls erforderlich
Pre_Corona_means_sf <- st_as_sf(Pre_Corona_means, coords = c("XLON", "XLAT"), crs = 4326)
First_Lockdown_UK_means_sf <- st_as_sf(First_Lockdown_UK_means, coords = c("XLON", "XLAT"), crs = 4326)
Return_to_relaxing_restrictions_means_sf <- st_as_sf(Return_to_relaxing_restrictions_means, coords = c("XLON", "XLAT"), crs = 4326)
Three_Tier_System_means_sf <- st_as_sf(Three_Tier_System_means, coords = c("XLON", "XLAT"), crs = 4326)
Second_Lockdown_UK_means_sf <- st_as_sf(Second_Lockdown_UK_means, coords = c("XLON", "XLAT"), crs = 4326)
End_Second_Lockdown_means_sf <- st_as_sf(End_Second_Lockdown_means, coords = c("XLON", "XLAT"), crs = 4326)
Tier_4_London_means_sf <- st_as_sf(Tier_4_London_means, coords = c("XLON", "XLAT"), crs = 4326)


###### MoransI calc
# 1. data to sf object
# 2. neighborhood and weight matrix
# 3. morans I

# Funktion zur Berechnung von Moran's I und Erstellung der Karten
calculate_morans_i_and_save_html <- function(data_sf, interval_name) {
  # 1. Koordinaten und räumliche Nachbarschaftsstruktur
  coords <- st_coordinates(data_sf)
  neighbors <- knearneigh(coords, k = 4)
  listw <- nb2listw(knn2nb(neighbors), style = "W")
  
  # 2. Moran's I berechnen
  moran_result <- moran.test(data_sf$mean_value, listw)
  print(paste("Moran's I für", interval_name, ":", moran_result$estimate[1]))
  
  # 3. Lokale Moran's I Statistik berechnen
  local_moran <- localmoran(data_sf$mean_value, listw)
  data_sf <- data_sf %>%
    mutate(local_moran_i = local_moran[, 1],  # Lokale Moran's I Werte
           p_value = local_moran[, 5])       # P-Werte
  
  # 4. Interaktive Karte erstellen
  map <- leaflet(data_sf) %>%
    addTiles() %>%  # OpenStreetMap hinzufügen
    addCircleMarkers(
      radius = 5,
      fillColor = ~colorNumeric("viridis", local_moran_i)(local_moran_i),
      fillOpacity = 0.8,
      color = "black",
      weight = 0.5,
      popup = ~paste(
        "<strong>Longitude:</strong>", st_coordinates(geometry)[,1], "<br>",
        "<strong>Latitude:</strong>", st_coordinates(geometry)[,2], "<br>",
        "<strong>Lokales Moran's I:</strong>", round(local_moran_i, 3), "<br>",
        "<strong>P-Wert:</strong>", round(p_value, 3)
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = colorNumeric("viridis", data_sf$local_moran_i),
      values = data_sf$local_moran_i,
      title = paste("Lokales Moran's I:", interval_name),
      opacity = 1
    )
  
  # 5. Karte als HTML speichern
  saveWidget(map, file = paste0(interval_name, "_morans_i_map.html"))
}

# Interaktive Karten für jedes Intervall berechnen und speichern
calculate_morans_i_and_save_html(Pre_Corona_means_sf, "Pre_Corona")
calculate_morans_i_and_save_html(First_Lockdown_UK_means_sf, "First_Lockdown_UK")
calculate_morans_i_and_save_html(Return_to_relaxing_restrictions_means_sf, "Return_to_relaxing_restrictions")
calculate_morans_i_and_save_html(Three_Tier_System_means_sf, "Three_Tier_System")
calculate_morans_i_and_save_html(Second_Lockdown_UK_means_sf, "Second_Lockdown_UK")
calculate_morans_i_and_save_html(End_Second_Lockdown_means_sf, "End_Second_Lockdown")
calculate_morans_i_and_save_html(Tier_4_London_means_sf, "Tier_4_London")