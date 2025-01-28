library(dplyr)
library(sf)
library(ggplot2)
library(tidyr)

# Busstationen (type == 1) filtern
bus_stations <- poi_geojson %>% filter(type == 1)

# U-Bahn-Stationen (type == 2) filtern
subway_stations <- poi_geojson %>% filter(type == 2)

poi_geojson_others <- poi_geojson %>% filter(type > 2)

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
data_london_3857 <- st_transform(data_london, crs = 3857)

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
data_london_Polygon <- st_as_sf(data_london_3857 %>%
                                       rowwise() %>%
                                       mutate(geometry = create_square(geometry))) %>%
                                       st_transform(crs = 4326)

bus_stations_3857 <- bus_stations
bus_stations <- st_transform(bus_stations, crs = 4326)
subway_stations_3857 <- subway_stations
subway_stations <- st_transform(subway_stations, crs = 4326)
poi_geojson_3857 <- poi_geojson
poi_geojson <- st_transform(poi_geojson_3857, crs = 4326)

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
other_pois_results <- list()


for (interval_name in names(interval_polygons)) {
  interval <- interval_polygons[[interval_name]]
  
  # Intersektion und Kombination der Attribute für Busstationen
  #bus_intersects <- st_join(interval, bus_stations, join = st_intersects) %>%
  #  mutate(interval = interval_name,        # Intervallname hinzufügen
  #         station_name = Name)             # Name der Busstation hinzufügen
  
  # Intersektion und Kombination der Attribute für U-Bahn-Stationen
  #subway_intersects <- st_join(interval, subway_stations, join = st_intersects) %>%
  #  mutate(interval = interval_name,        # Intervallname hinzufügen
  #         station_name = Name)             # Name der U-Bahn-Station hinzufügen
  
  other_pois_intersects <- st_join(interval, poi_geojson_others, join = st_intersects) %>%
    mutate(interval = interval_name,
           name = Name)
  
  # Ergebnisse speichern
  #bus_results[[interval_name]] <- bus_intersects
  #subway_results[[interval_name]] <- subway_intersects
  other_pois_results[[interval_name]] <- other_pois_intersects
}

for (interval_name in names(other_pois_results)) {
  # NA-Einträge aus bus_results entfernen
  #bus_results[[interval_name]] <- bus_results[[interval_name]] %>%
  #  filter(!is.na(station_name))  # Entfernt Zeilen mit NA in station_name
  
  # NA-Einträge aus subway_results entfernen
  #subway_results[[interval_name]] <- subway_results[[interval_name]] %>%
  #  filter(!is.na(station_name))  # Entfernt Zeilen mit NA in station_name
  other_pois_results[[interval_name]] <- other_pois_results[[interval_name]] %>%
    filter(!is.na(name))
}


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

type_3_results_mean <- list()
type_4_results_mean <- list()
type_5_results_mean <- list()
type_6_results_mean <- list()

for (interval_name in names(other_pois_results)) {
  # Extract the data for the current interval
  interval_data <- other_pois_results[[interval_name]]
  
  # Filter by type and compute the means
  type_3_results_mean[[interval_name]] <- interval_data %>%
    filter(type == 3) %>%  # Filter for type 3
    group_by(name) %>%  # Group by name
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),
      n_intersections = n(),
      geometry = first(geometry),
      .groups = "drop"
    )
  
  type_4_results_mean[[interval_name]] <- interval_data %>%
    filter(type == 4) %>%  # Filter for type 4
    group_by(name) %>%
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),
      n_intersections = n(),
      geometry = first(geometry),
      .groups = "drop"
    )
  
  type_5_results_mean[[interval_name]] <- interval_data %>%
    filter(type == 5) %>%  # Filter for type 5
    group_by(name) %>%
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),
      n_intersections = n(),
      geometry = first(geometry),
      .groups = "drop"
    )
  
  type_6_results_mean[[interval_name]] <- interval_data %>%
    filter(type == 6) %>%  # Filter for type 6
    group_by(name) %>%
    summarise(
      mean_of_means = mean(mean_value, na.rm = TRUE),
      n_intersections = n(),
      geometry = first(geometry),
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

# Combine and calculate mean for each type
type_3_df <- bind_rows(type_3_results_mean, .id = "interval") %>%
  mutate(interval = factor(interval, levels = interval_order)) %>%
  group_by(interval) %>%
  summarize(overall_mean = mean(mean_of_means, na.rm = TRUE), .groups = "drop")

type_4_df <- bind_rows(type_4_results_mean, .id = "interval") %>%
  mutate(interval = factor(interval, levels = interval_order)) %>%
  group_by(interval) %>%
  summarize(overall_mean = mean(mean_of_means, na.rm = TRUE), .groups = "drop")

type_5_df <- bind_rows(type_5_results_mean, .id = "interval") %>%
  mutate(interval = factor(interval, levels = interval_order)) %>%
  group_by(interval) %>%
  summarize(overall_mean = mean(mean_of_means, na.rm = TRUE), .groups = "drop")

type_6_df <- bind_rows(type_6_results_mean, .id = "interval") %>%
  mutate(interval = factor(interval, levels = interval_order)) %>%
  group_by(interval) %>%
  summarize(overall_mean = mean(mean_of_means, na.rm = TRUE), .groups = "drop")



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
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) +  # Add mean value labels
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
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) + 
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

type_3_plot <- ggplot(type_3_df, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) + 
  geom_line(group = 1, color = "grey") + 
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) + 
  scale_color_manual(values = interval_colors) +
  labs(
    title = "Overall Mean Across All Football Stadiums by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

type_4_plot <- ggplot(type_4_df, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) + 
  geom_line(group = 1, color = "grey") + 
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) + 
  scale_color_manual(values = interval_colors) +
  labs(
    title = "Overall Mean Across All Big Event Places by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

type_5_plot <- ggplot(type_5_df, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) + 
  geom_line(group = 1, color = "grey") + 
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) + 
  scale_color_manual(values = interval_colors) +
  labs(
    title = "Overall Mean Across All Popular Sightseeings by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

type_6_plot <- ggplot(type_6_df, aes(x = interval, y = overall_mean)) +
  geom_point(aes(color = interval), size = 4) + 
  geom_line(group = 1, color = "grey") + 
  geom_text(aes(label = round(overall_mean, 2)), vjust = -0.5, size = 3.5) + 
  scale_color_manual(values = interval_colors) +
  labs(
    title = "Overall Mean Across All Royal Parks by Interval",
    x = "Interval",
    y = "Overall Mean Value",
    color = "Interval"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

##### All POIs together #####
all_POIs_means <- data.frame(
  intervall = c("Pre_Corona", "First_Lockdown_UK", "Return_to_relaxing_restrictions", 
                "Three_Tier_System", "Second_Lockdown_UK", "End_Second_Lockdown", "Tier_4_London"),
  bus_station = c(1.05, 0.25, 0.44, 0.44, 0.36, 0.4, 0.26),
  subway_station = c(1.77, 0.35, 0.67, 0.72, 0.56, 0.63, 0.44),
  football_stadium = c(0.37, 0.2, 0.21, 0.18, 0.21, 0.19, 0.17),
  big_event_places = c(0.31, 0.16, 0.19, 0.19, 0.23, 0.18, 0.18),
  sightseeings = c(1.2, 0.23, 0.36, 0.4, 0.3, 0.34, 0.24),
  parks = c(0.53, 0.23, 0.28, 0.25, 0.25, 0.23, 0.19)
)

all_POIs_means_long <- all_POIs_means %>%
  pivot_longer(cols = -intervall, names_to = "category", values_to = "mean_value")

interval_order <- c("Pre_Corona", "First_Lockdown_UK", "Return_to_relaxing_restrictions", 
                    "Three_Tier_System", "Second_Lockdown_UK", "End_Second_Lockdown", "Tier_4_London")

category_colors <- c(
  "bus_station" = "#373636",
  "subway_station" = "#afafaf",
  "football_stadium" = "#312E74",
  "big_event_places" = "#abc4f8",
  "sightseeings" = "#7A5445",
  "parks" = "#8CDB6C"
)

# Convert the 'intervall' column to a factor with the correct order
all_POIs_means_long$intervall <- factor(all_POIs_means_long$intervall, levels = interval_order)

# Create the plot with the corrected order
all_POIs_means_plot <- ggplot(all_POIs_means_long, aes(x = intervall, y = mean_value, group = category)) +
  geom_line(aes(color = category), linewidth = 1.5, show.legend = TRUE) +  # Linien in der Legende
  geom_point(aes(color = intervall), size = 4, show.legend = FALSE) +  # Punkte nicht in der Legende
  scale_color_manual(
    values = c(category_colors, interval_colors),  # Farben für Linien und Punkte
    breaks = names(category_colors)  # Zeige nur Kategorien in der Legende
  ) +
  geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, size = 3.5) +  # Labels für Mittelwerte
  labs(
    title = "Overall Mean Across All POIs by Interval",
    x = "Interval",
    y = "Mean Value",
    color = "Legend"
  ) +
  scale_y_continuous(
    limits = c(0, 2),  # Bereich der y-Achse anpassen
    breaks = seq(0, 2, by = 0.2)  # Schritte der Skala
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # X-Achsen-Beschriftung drehen
    plot.title = element_text(hjust = 0.5),  # Titel zentrieren
    legend.position = "bottom"  # Legende unten
  )


##### Daily intersect #####
# Intersektion und Kombination der Attribute für Busstationen
bus_intersects_daily <- st_join(data_london_Polygon, bus_stations, join = st_intersects) %>%
  mutate(station_name = Name)             # Name der Busstation hinzufügen

bus_intersects_daily <- bus_intersects_daily %>% filter(!is.na(station_name))

# Intersektion und Kombination der Attribute für U-Bahn-Stationen
subway_intersects_daily <- st_join(data_london_Polygon, subway_stations, join = st_intersects) %>%
  mutate(station_name = Name)             # Name der U-Bahn-Station hinzufügen

subway_intersects_daily <- subway_intersects_daily %>% filter(!is.na(station_name))

