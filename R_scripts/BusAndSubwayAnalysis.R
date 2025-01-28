library(dplyr)
library(sf)
library(ggplot2)

# Busstationen (type == 1) filtern
bus_stations <- poi_geojson %>% filter(type == 1)

# U-Bahn-Stationen (type == 2) filtern
subway_stations <- poi_geojson %>% filter(type == 2)

# Optional: Ergebnisse anzeigen
head(bus_stations)
head(subway_stations)

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
Pre_Corona_means_3857 <- st_transform(Pre_Corona_means, crs = 3857)
First_Lockdown_UK_means_3857 <- st_transform(First_Lockdown_UK_means, crs=3857)
Return_to_relaxing_restrictions_means_3857 <- st_transform(Return_to_relaxing_restrictions_means, crs = 3857)
Three_Tier_System_means_3857 <- st_transform(Three_Tier_System_means, crs=3857)
Second_Lockdown_UK_means_3857 <- st_transform(Second_Lockdown_UK_means, crs = 3857)
End_Second_Lockdown_means_3857 <- st_transform(End_Second_Lockdown_means, crs=3857)
Tier_4_London_means_3857 <- st_transform(Tier_4_London_means, crs=3857)
combined_results_3857 <- st_transform(combined_results, crs = 3857)

# Rechtecke erzeugen
Pre_Corona_means_Polygons <- st_as_sf(Pre_Corona_means_3857 %>%
                                      rowwise() %>%
                                      mutate(geometry = create_square(geometry))) %>%
                                      st_transform(crs = 4326)
First_Lockdown_UK_means_Polygon <- st_as_sf(First_Lockdown_UK_means_3857 %>%
                                   rowwise() %>%
                                   mutate(geometry = create_square(geometry))) %>%
                                   st_transform(crs = 4326)
Return_to_relaxing_restrictions_means_Polygon <- st_as_sf(Return_to_relaxing_restrictions_means_3857 %>%
                                      rowwise() %>%
                                      mutate(geometry = create_square(geometry))) %>%
                                      st_transform(crs = 4326)
Three_Tier_System_means_Polygon <- st_as_sf(Three_Tier_System_means_3857 %>%
                                   rowwise() %>%
                                   mutate(geometry = create_square(geometry))) %>%
                                   st_transform(crs = 4326)
Second_Lockdown_UK_means_Polygon <- st_as_sf(Second_Lockdown_UK_means_3857 %>%
                                      rowwise() %>%
                                      mutate(geometry = create_square(geometry))) %>%
                                      st_transform(crs = 4326)
End_Second_Lockdown_means_Polygon <- st_as_sf(End_Second_Lockdown_means_3857 %>%
                                   rowwise() %>%
                                   mutate(geometry = create_square(geometry))) %>%
                                   st_transform(crs = 4326)
Tier_4_London_means_Polygon <- st_as_sf(Tier_4_London_means_3857 %>%
                                   rowwise() %>%
                                   mutate(geometry = create_square(geometry))) %>%
                                   st_transform(crs = 4326)
combined_results_Polygon <- st_as_sf(combined_results_3857 %>%
                                       rowwise() %>%
                                       mutate(geometry = create_square(geometry))) %>%
                                       st_transform(crs = 4326)

lbus_stations_3857 <- bus_stations
bus_stations <- st_transform(bus_stations, crs = 4326)
subway_stations_3857 <- subway_stations
subway_stations <- st_transform(subway_stations, crs = 4326)

interval_polygons <- list(
  Pre_Corona = Pre_Corona_means_Polygons,
  First_Lockdown = First_Lockdown_UK_means_Polygon,
  Relaxing_Restrictions = Return_to_relaxing_restrictions_means_Polygon,
  Three_Tier_System = Three_Tier_System_means_Polygon,
  Second_Lockdown = Second_Lockdown_UK_means_Polygon,
  End_Second_Lockdown = End_Second_Lockdown_means_Polygon,
  Tier_4_London = Tier_4_London_means_Polygon
)

# Ergebnisse-Listen für Bus- und U-Bahn-Stationen erstellen
bus_results <- list()
subway_results <- list()


for (interval_name in names(interval_polygons)) {
  interval <- interval_polygons[[interval_name]]
  
  # Intersektion und Kombination der Attribute für Busstationen
  bus_intersects <- st_join(interval, bus_stations, join = st_intersects) %>%
    mutate(interval = interval_name,        # Intervallname hinzufügen
           station_name = Name)             # Name der Busstation hinzufügen
  
  # Intersektion und Kombination der Attribute für U-Bahn-Stationen
  subway_intersects <- st_join(interval, subway_stations, join = st_intersects) %>%
    mutate(interval = interval_name,        # Intervallname hinzufügen
           station_name = Name)             # Name der U-Bahn-Station hinzufügen
  
  # Ergebnisse speichern
  bus_results[[interval_name]] <- bus_intersects
  subway_results[[interval_name]] <- subway_intersects
}

for (interval_name in names(bus_results)) {
  # NA-Einträge aus bus_results entfernen
  bus_results[[interval_name]] <- bus_results[[interval_name]] %>%
    filter(!is.na(station_name))  # Entfernt Zeilen mit NA in station_name
  
  # NA-Einträge aus subway_results entfernen
  subway_results[[interval_name]] <- subway_results[[interval_name]] %>%
    filter(!is.na(station_name))  # Entfernt Zeilen mit NA in station_name
}
summary(bus_results$Pre_Corona)
summary(subway_results$Pre_Corona)

pre_corona_bus_stations <- bus_results$Pre_Corona
pre_corona_subway_stations <- subway_results$Pre_Corona

# Plot mit ggplot
ggplot() +
  geom_sf(data = pre_corona_bus_stations, aes(color = interval), size = 3) +  # Punkte einfärben nach Intervall
  labs(title = "Busstationen - Pre_Corona",
       color = "Interval") +  # Legende beschriften
  theme_minimal()
ggplot() +
  geom_sf(data = pre_corona_subway_stations, aes(color = interval), size = 3) +  # Punkte einfärben nach Intervall
  labs(title = "Busstationen - Pre_Corona",
       color = "Interval") +  # Legende beschriften
  theme_minimal()

bus_results_mean <- list()
for (interval_name in names(bus_results)) {
  bus_results_mean[[interval_name]] <- bus_results[[interval_name]] %>%
    group_by(station_name) %>%  # Gruppieren nach der Station
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),  # Mittelwert berechnen
      n_intersections = n(),  # Anzahl der intersecting Kacheln
      geometry = first(geometry),  # Eine Geometrie pro Station
      .groups = "drop"
    )
}
subway_results_mean <- list()
for (interval_name in names(subway_results)) {
  subway_results_mean[[interval_name]] <- subway_results[[interval_name]] %>%
    group_by(station_name) %>%  # Gruppieren nach der Station
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),  # Mittelwert berechnen
      n_intersections = n(),  # Anzahl der intersecting Kacheln
      geometry = first(geometry),  # Eine Geometrie pro Station
      .groups = "drop"
    )
}


interval_order <- c(
  "Pre_Corona",
  "First_Lockdown",
  "Relaxing_Restrictions",
  "Three_Tier_System",
  "Second_Lockdown",
  "End_Second_Lockdown",
  "Tier_4_London"
)
interval_colors <- c(
  "Pre_Corona" = "#2E8B57",
  "First_Lockdown" = "red",
  "Relaxing_Restrictions" = "#90EE90",
  "Three_Tier_System" = "#FFA500",
  "Second_Lockdown" = "red",
  "End_Second_Lockdown" = "#90EE90",
  "Tier_4_London" = "#FFA500"
)

# Spalte `interval` in Faktor umwandeln mit der gewünschten Reihenfolge
bus_means_df <- bus_means_df %>%
  mutate(interval = factor(interval, levels = interval_order))

bus_overall_mean_per_interval <- bus_means_df %>%
  group_by(interval) %>%
  summarize(
    overall_mean = mean(mean_of_means, na.rm = TRUE), # Calculate overall mean
    .groups = "drop" # Avoid unnecessary group attributes
  )

subway_means_df <- bind_rows(subway_results_mean, .id = "interval") %>%
  mutate(interval = factor(interval, levels = interval_order))  # Ensure correct interval order

subway_overall_mean_per_interval <- subway_means_df %>%
  group_by(interval) %>%
  summarize(
    overall_mean = mean(mean_of_means, na.rm = TRUE),  # Calculate overall mean
    .groups = "drop"
  )

bus_facet_plot <- ggplot(bus_means_df, aes(x = interval, y = mean_of_means)) +
  geom_point(aes(color = interval), size = 3) +  # Use custom colors
  geom_line(aes(group = 1), color = "grey") +  # Connect points for context
  scale_color_manual(values = interval_colors) +  # Apply custom colors
  labs(
    title = "Mean of Means per Station and Interval",
    x = "Interval",
    y = "Mean of Means",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~station_name, scales = "fixed")

subway_facet_plot <- ggplot(subway_means_df, aes(x = interval, y = mean_of_means)) +
  geom_point(aes(color = interval), size = 3) +  # Use custom colors
  geom_line(aes(group = 1), color = "grey") +  # Connect points for context
  scale_color_manual(values = interval_colors) +  # Apply custom colors
  labs(
    title = "Mean of Means per Subway Station and Interval",
    x = "Interval",
    y = "Mean of Means",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~station_name, scales = "fixed")

# Plot the overall mean per interval
bus_overall_mean_plot <- ggplot(bus_overall_mean_per_interval, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) + # Add points with custom colors
  geom_line(group = 1, color = "grey") + # Connect points with a line
  scale_color_manual(values = interval_colors) + # Apply your custom colors
  labs(
    title = "Overall Mean Across All Bus Stations by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5) # Center the title
  )

subway_overall_mean_plot <- ggplot(subway_overall_mean_per_interval, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) +  # Add points with custom colors
  geom_line(group = 1, color = "grey") +  # Connect points with a line
  scale_color_manual(values = interval_colors) +  # Apply your custom colors
  labs(
    title = "Overall Mean Across All Subway Stations by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5)  # Center the title
  )

##### Daily intersect #####

