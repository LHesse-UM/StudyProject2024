
# Load the libraries
library(sf)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/Analysis")

# Einladen RDATA Luca
load("/Users/timlehmann/Downloads/DataPreprocessingForSTDBSCAN.RData")

### Stadiums
# Sicherstellen, dass die Spalte 'AGG_DAY_PERIOD' als Datum formatiert ist
mergedData2$AGG_DAY_PERIOD <- as.Date(mergedData2$AGG_DAY_PERIOD, format = "%Y-%m-%d")

# Liste der zu filternden Datumswerte für Stadien
dates_for_stadiums <- as.Date(c("2020-03-07", "2020-02-01", "2020-02-22", "2020-11-07",
                             "2020-01-11", "2020-02-29", "2020-07-26", "2020-09-19",
                             "2020-10-24", "2020-12-16"))

# Filtern der Daten nach den spezifischen Datumswerten
filtered_data <- mergedData2 %>% filter(AGG_DAY_PERIOD %in% dates_for_stadiums)

# Konvertieren der gefilterten Daten in ein sf-Objekt
filtered_data_sf <- st_as_sf(filtered_data, coords = c("XLON", "XLAT"), crs = 4326)

st_write(filtered_data_sf, "Stadiums/filtered_points.geojson", delete_dsn = TRUE)

# Ergebnisse anzeigen
print(head(filtered_data))


# 1. GeoJSON-Dateien laden
movement_data <- st_read("Stadiums/filtered_points.geojson")  # Bewegungsdaten als Punktdaten
pois_london_stadiums <- st_read("Stadiums/pois_london_stadiums.geojson")  # Polygone der Stadien

# 2. Räumlichen Join durchführen (Punkte mit Stadien verknüpfen)
movement_with_stadiums <- st_join(movement_data, pois_london_stadiums, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Stadions liegen
movement_with_stadiums <- movement_with_stadiums %>% filter(!is.na(Name))  # 'name' ist der Stadion-Name

# 4. Durchschnitt pro Stadion und Datum berechnen
stadium_daily_averages <- movement_with_stadiums %>%
  group_by(Name, AGG_DAY_PERIOD) %>%               # Gruppieren nach Stadion (Name) und Datum (AGC_DAY_PERIOD)
  summarise(
    daily_mean = mean(mean_column, na.rm = TRUE),  # Durchschnitt berechnen
    .groups = "drop"                              # Gruppierung nach der Berechnung entfernen
  )

# 5. Ergebnis anzeigen
print(stadium_daily_averages)

# 6. Ergebnis speichern (optional)
st_write(stadium_daily_averages, "Stadiums/stadium_daily_averages.geojson", row.names = FALSE, delete_dsn = TRUE)


# 7. Liniendiagramm: Durchschnittswerte pro Stadion und Datum
ggplot(stadium_daily_averages, aes(x = AGG_DAY_PERIOD, y = daily_mean, color = Name, group = Name)) +
  geom_line(size = 1) +                             # Linienplot
  geom_point(size = 3) +                            # Punkte hinzufügen
  theme_minimal() +                                 # Minimalistisches Theme
  labs(
    title = "Tägliche Durchschnittswerte pro Stadion",
    x = "Datum",
    y = "Durchschnittlicher Wert",
    color = "Stadion"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotierte x-Achsen-Beschriftung für bessere Lesbarkeit
  )

# 8. Facettenplot: Ein Plot pro Stadion
ggplot(stadium_daily_averages, aes(x = AGG_DAY_PERIOD, y = daily_mean)) +
  geom_line(size = 1, color = "blue") +             # Linienplot
  geom_point(size = 3, color = "red") +            # Punkte hinzufügen
  facet_wrap(~ Name, scales = "free_y") +          # Facetten nach Stadion (Name)
  theme_minimal() +
  labs(
    title = "Tägliche Durchschnittswerte pro Stadion",
    x = "Datum",
    y = "Durchschnittlicher Wert"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotierte x-Achsen-Beschriftung für bessere Lesbarkeit
  )



# Alle Stadien aus den bestehenden Daten
all_stadiums <- unique(stadium_daily_averages$Name)

# Vollständige Kombination aus Stadien und Tagen erstellen
complete_combinations <- expand.grid(
  Name = all_stadiums,
  AGG_DAY_PERIOD = dates_for_stadiums  # Passe hier den Spaltennamen an, falls nötig
)

# Sicherstellen, dass die Datums-Spalte in stadium_daily_averages als Date formatiert ist
stadium_daily_averages <- stadium_daily_averages %>%
  mutate(AGG_DAY_PERIOD = as.Date())

# Vollständige Kombination mit den vorhandenen Daten abgleichen und fehlende Tage ergänzen
completed_with_missing <- complete_combinations %>%
  left_join(stadium_daily_averages, by = c("Name", "AGG_DAY_PERIOD")) %>%
  mutate(
    daily_mean = ifelse(is.na(daily_mean), 0, daily_mean),  # Fehlende Werte auf 0 setzen
    geometry = ifelse(is.na(geometry), st_sfc(st_point()), geometry) # Leere Geometrie für fehlende Einträge
  )

# Konvertieren in sf-Objekt
completed_with_missing_sf <- st_as_sf(completed_with_missing, sf_column_name = "geometry", crs = st_crs(stadium_daily_averages))

# Ergebnis speichern
st_write(completed_with_missing_sf, "/mnt/data/completed_stadium_daily_averages.geojson", driver = "GeoJSON")


# Guter Facettenplot mit bedingten Farben für Punkte
ggplot(completed_with_missing_sf, aes(x = AGG_DAY_PERIOD, y = daily_mean)) +
  geom_line(size = 1, color = "blue") +
  geom_point(aes(color = daily_mean == 0), size = 3) +  # Farbe basierend auf daily_mean
  facet_wrap(~ Name) +
  scale_color_manual(
    values = c("TRUE" = "black", "FALSE" = "red"),       # Schwarz für 0, Rot für >0
    labels = c("TRUE" = "Wert = 0", "FALSE" = "Wert > 0"),
    name = "Mean Value"
  ) +
  scale_x_date(
    breaks = dates_for_stadiums,                                 # Nur die 10 spezifischen Daten als Breaks
    labels = format(dates_for_stadiums, "%d-%b")                # Datumsformat ohne Jahr
  ) +
  theme_minimal() +
  labs(
    title = "Tägliche Durchschnittswerte pro Stadion",
    x = "Datum",
    y = "Durchschnittlicher Wert"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotierte X-Achsen-Beschriftung
  )

# Beispiel: Erstellen einer Highlight-Spalte
completed_with_missing_sf$highlight <- ifelse(completed_with_missing_sf$AGG_DAY_PERIOD %in% c("2023-03-07", "2023-11-16"), TRUE, FALSE)

ggplot(completed_with_missing_sf, aes(x = AGG_DAY_PERIOD, y = daily_mean)) +
  geom_line(size = 1, color = "blue") +
  geom_point(aes(color = highlight), size = 3) +  # Farbe basierend auf der Highlight-Spalte
  facet_wrap(~ Name) +
  scale_color_manual(
    values = c("TRUE" = "green", "FALSE" = "black"),  # Grün für Highlights, Schwarz für andere
    labels = c("TRUE" = "Highlight", "FALSE" = "Normal"),
    name = "Punktfarbe"
  ) +
  scale_x_date(
    breaks = dates_for_stadiums,  # Nur die 10 spezifischen Daten als Breaks
    labels = format(dates_for_stadiums, "%d-%b")  # Datumsformat ohne Jahr
  ) +
  theme_minimal() +
  labs(
    title = "Tägliche Durchschnittswerte pro Stadion",
    x = "Datum",
    y = "Durchschnittlicher Wert"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotierte X-Achsen-Beschriftung
  )


##############

library(sf)
library(dplyr)
library(ggplot2)

setwd("~/Desktop/Analysis")

# Einladen RDATA Luca
load("/Users/timlehmann/Downloads/DataPreprocessingForSTDBSCAN.RData")
mergedData3 <- mergedData2

# Sicherstellen, dass die Spalte 'AGG_DAY_PERIOD' als Datum formatiert ist
mergedData2$AGG_DAY_PERIOD <- as.Date(mergedData2$AGG_DAY_PERIOD, format = "%Y-%m-%d")
mergedData3$AGG_DAY_PERIOD <- as.Date(mergedData2$AGG_DAY_PERIOD, format = "%Y-%m-%d")

# Liste der zu filternden Datumswerte für Emirates Stadion
dates_for_Arsenal <- as.Date(c("2020-01-01", "2020-01-18", "2020-01-06", "2020-02-16", "2020-02-23",
                               "2020-02-27", "2020-03-07", "2020-07-01", "2020-07-07", "2020-07-15",
                               "2020-07-26", "2020-07-18", "2020-08-01", "2020-09-19", "2020-10-04",
                               "2020-10-25", "2020-10-29", "2020-11-08", "2020-11-29", "2020-11-05",
                               "2020-12-13", "2020-12-16", "2020-12-26", "2020-12-22", "2020-12-03"))

# Filtern der Daten nach den spezifischen Datumswerten
filtered_data_emirates <- mergedData2 %>% filter(AGG_DAY_PERIOD %in% dates_for_Arsenal)

# Konvertieren der gefilterten Daten in ein sf-Objekt
filtered_data_emirates_sf <- st_as_sf(filtered_data_emirates, coords = c("XLON", "XLAT"), crs = 4326)

st_write(filtered_data_emirates_sf, "Stadiums/filtered_data_emirates.geojson", delete_dsn = TRUE)

# 1. GeoJSON-Dateien laden
movement_data_emirates <- st_read("Stadiums/filtered_data_emirates.geojson")  # Bewegungsdaten als Punktdaten
pois_emirates <- st_read("Stadiums/pois_emirates.geojson")  # Polygone der Stadien

# 2. Räumlichen Join durchführen (Punkte mit Stadien verknüpfen)
movement_with_stadiums <- st_join(movement_data_emirates, pois_emirates, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Stadions liegen
movement_with_stadiums <- movement_with_stadiums %>% filter(!is.na(Name))  # 'name' ist der Stadion-Name

# 4. Durchschnitt pro Stadion und Datum berechnen
emirates_daily_averages <- movement_with_stadiums %>%
  group_by(Name, AGG_DAY_PERIOD) %>%               # Gruppieren nach Stadion (Name) und Datum (AGG_DAY_PERIOD)
  summarise(
    daily_mean = mean(mean_column, na.rm = TRUE),  # Durchschnitt berechnen
    .groups = "drop"                              # Gruppierung nach der Berechnung entfernen
  )

# 5. Ergebnis anzeigen
print(emirates_daily_averages)

# 6. Ergebnis speichern 
st_write(emirates_daily_averages, "Stadiums/emirates_daily_averages.geojson", row.names = FALSE, delete_dsn = TRUE)


# Phaseninformationen mit korrekter Reihenfolge
phasen <- data.frame(
  Phase = factor(c("Pre-Corona", "First Lockdown", "Relaxing Restrictions", 
                   "Three-Tier System", "Second Lockdown", 
                   "End of Second Lockdown", "Four-Tier System"),
                 levels = c("Pre-Corona", "First Lockdown", "Relaxing Restrictions", 
                            "Three-Tier System", "Second Lockdown", 
                            "End of Second Lockdown", "Four-Tier System")),
  Start = as.Date(c("2020-01-01", "2020-03-26", "2020-05-11", "2020-10-14", 
                    "2020-11-05", "2020-12-02", "2020-12-21")),
  Ende = as.Date(c("2020-03-25", "2020-05-10", "2020-10-13", "2020-11-04", 
                   "2020-12-01", "2020-12-20", "2020-12-31"))
)

# Farbpalette für die Phasen
interval_colors <- c(
  "Pre-Corona" = "#2E8B57",
  "First Lockdown" = "red",
  "Relaxing Restrictions" = "#90EE90",
  "Three-Tier System" = "#FFA500",
  "Second Lockdown" = "red",
  "End of Second Lockdown" = "#90EE90",
  "Four-Tier System" = "#FFA500"
)

# Plot erstellen
ggplot(emirates_daily_averages, aes(x = AGG_DAY_PERIOD, y = daily_mean, color = Name, group = Name)) +
  geom_line(size = 1) +                             # Linienplot
  geom_point(size = 3) +                            # Punkte hinzufügen
  geom_rect(data = phasen, aes(xmin = Start, xmax = Ende, ymin = -Inf, ymax = Inf, fill = Phase),
            inherit.aes = FALSE, alpha = 0.2) +
  scale_fill_manual(values = interval_colors) +
  labs(
    title = "Daily averages Emirates Stadium with intervals",
    x = "Date",
    y = "Average Value",
    color = "Stadion",
    fill = "Phase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


###################################### O2

# Liste der zu filternden Datumswerte für O2 Arena
dates_for_O2 <- as.Date(c("2020-02-07", "2020-02-08", "2020-02-09", "2020-02-18", "2020-02-21", "2020-02-22", "2020-02-25", "2020-02-28", "2020-02-29",
                          "2020-03-01", "2020-03-03", "2020-03-05", "2020-03-06", "2020-03-07", "2020-03-08", "2020-03-11", "2020-03-13", "2020-03-14", "2020-03-15",
                          "2020-03-21", "2020-03-26", "2020-03-27", "2020-03-29", "2020-04-02", "2020-04-03", "2020-04-04", "2020-04-07", "2020-04-15", "2020-04-17",
                          "2020-04-19", "2020-04-22", "2020-04-23", "2020-04-24", "2020-04-25", "2020-04-26", "2020-05-08", "2020-05-14", "2020-05-15", "2020-05-16",
                          "2020-05-17", "2020-05-21", "2020-05-24", "2020-05-26", "2020-05-27", "2020-05-28", "2020-05-31", "2020-06-02", "2020-06-03", "2020-06-04",
                          "2020-06-05", "2020-06-06", "2020-06-08", "2020-06-09", "2020-06-10", "2020-06-11", "2020-06-17", "2020-06-18", "2020-06-20", "2020-06-21",
                          "2020-06-24", "2020-06-25", "2020-06-26", "2020-06-27", "2020-06-28", "2020-06-29", "2020-06-30", "2020-07-01", "2020-07-02", "2020-07-03",
                          "2020-07-04", "2020-07-05", "2020-07-08", "2020-07-09", "2020-07-18", "2020-07-15", "2020-07-17", "2020-07-19", "2020-07-26", "2020-07-27",
                          "2020-07-29", "2020-07-30", "2020-09-02", "2020-09-03", "2020-09-04", "2020-09-05", "2020-09-17", "2020-09-18", "2020-09-19", "2020-10-03",
                          "2020-10-05", "2020-10-06", "2020-10-16", "2020-10-17", "2020-10-18", "2020-10-24", "2020-10-25", "2020-10-28", "2020-11-02", "2020-11-04",
                          "2020-11-06", "2020-11-07", "2020-11-15", "2020-11-16", "2020-11-17", "2020-11-18", "2020-11-19", "2020-11-20", "2020-11-21", "2020-11-22",
                          "2020-12-02", "2020-12-09", "2020-12-14", "2020-12-16", "2020-12-17", "2020-12-04", "2020-12-05"))

# Filtern der Daten nach den spezifischen Datumswerten
filtered_data_O2 <- mergedData3 %>% filter(AGG_DAY_PERIOD %in% dates_for_O2)

# Konvertieren der gefilterten Daten in ein sf-Objekt
filtered_data_O2_sf <- st_as_sf(filtered_data_O2, coords = c("XLON", "XLAT"), crs = 4326)

st_write(filtered_data_O2_sf, "Stadiums/filtered_data_O2.geojson", delete_dsn = TRUE)

# 1. GeoJSON-Dateien laden
movement_data_O2 <- st_read("Stadiums/filtered_data_O2.geojson")  # Bewegungsdaten als Punktdaten
pois_O2 <- st_read("Stadiums/pois_O2.geojson")  # Polygone der Stadien

# 2. Räumlichen Join durchführen (Punkte mit Stadien verknüpfen)
movement_with_stadiums <- st_join(movement_data_O2, pois_O2, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Stadions liegen
movement_with_stadiums <- movement_with_stadiums %>% filter(!is.na(Name))  # 'name' ist der Stadion-Name

# 4. Durchschnitt pro Stadion und Datum berechnen
O2_daily_averages <- movement_with_stadiums %>%
  group_by(Name, AGG_DAY_PERIOD) %>%               # Gruppieren nach Stadion (Name) und Datum (AGG_DAY_PERIOD)
  summarise(
    daily_mean = mean(mean_column, na.rm = TRUE),  # Durchschnitt berechnen
    .groups = "drop"                              # Gruppierung nach der Berechnung entfernen
  )

# 5. Ergebnis anzeigen
print(O2_daily_averages)

# 6. Ergebnis speichern 
st_write(O2_daily_averages, "Stadiums/O2_daily_averages.geojson", row.names = FALSE, delete_dsn = TRUE)


# Phaseninformationen mit korrekter Reihenfolge
phasen <- data.frame(
  Phase = factor(c("Pre-Corona", "First Lockdown", "Relaxing Restrictions", 
                   "Three-Tier System", "Second Lockdown", 
                   "End of Second Lockdown", "Four-Tier System"),
                 levels = c("Pre-Corona", "First Lockdown", "Relaxing Restrictions", 
                            "Three-Tier System", "Second Lockdown", 
                            "End of Second Lockdown", "Four-Tier System")),
  Start = as.Date(c("2020-01-01", "2020-03-26", "2020-05-11", "2020-10-14", 
                    "2020-11-05", "2020-12-02", "2020-12-21")),
  Ende = as.Date(c("2020-03-25", "2020-05-10", "2020-10-13", "2020-11-04", 
                   "2020-12-01", "2020-12-20", "2020-12-31"))
)

# Farbpalette für die Phasen
interval_colors <- c(
  "Pre-Corona" = "#2E8B57",
  "First Lockdown" = "red",
  "Relaxing Restrictions" = "#90EE90",
  "Three-Tier System" = "#FFA500",
  "Second Lockdown" = "red",
  "End of Second Lockdown" = "#90EE90",
  "Four-Tier System" = "#FFA500"
)

# Plot erstellen
ggplot(O2_daily_averages, aes(x = AGG_DAY_PERIOD, y = daily_mean, color = Name, group = Name)) +
  geom_line(size = 1) +                             # Linienplot
  geom_point(size = 3) +                            # Punkte hinzufügen
  geom_rect(data = phasen, aes(xmin = Start, xmax = Ende, ymin = -Inf, ymax = Inf, fill = Phase),
            inherit.aes = FALSE, alpha = 0.2) +
  scale_fill_manual(values = interval_colors) +
  labs(
    title = "Daily averages O2 Arena with intervals",
    x = "Date",
    y = "Average Value",
    color = "Stadion",
    fill = "Phase"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
