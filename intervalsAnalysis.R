# Pakete laden
library(dplyr)
library(lubridate)
library(readr)
library(sf)
library(ggplot2)
library(gt)

# Intervalle definieren
intervalle <- data.frame(
  intervall = c("Pre Corona", "First Lockdown UK", "Return to relaxing restrictions", 
                "Three Tier System", "Second Lockdown UK", "End Second Lockdown", "Tier 4 London"),
  start = c(0, 85, 130, 287, 309, 336, 355),
  end = c(84, 129, 286, 308, 335, 354, 365)
)

# Formatierung: Datum in day-of-year umwandeln (falls time-Spalte nicht passt)
mergedData2 <- mergedData2 %>%
  mutate(time = yday(AGG_DAY_PERIOD))  # 'time' basierend auf Datum

# Ergebnisse berechnen: Mittelwerte pro Kachel pro Intervall
ergebnisse <- list()

for (i in 1:nrow(intervalle)) {
  start <- intervalle$start[i]
  end <- intervalle$end[i]
  intervall_name <- intervalle$intervall[i]
  
  # Filtere Daten für das aktuelle Intervall
  daten_intervall <- mergedData2 %>%
    filter(!is.na(time) & time >= start & time <= end)
  
  # Prüfen, ob Daten vorhanden sind
  if (nrow(daten_intervall) > 0) {
    # Berechne den Mittelwert pro Kachel und behalte XLON und XLAT
    mittelwerte <- daten_intervall %>%
      group_by(LONLAT_ID, XLON, XLAT) %>%
      summarise(Mittelwert = mean(mean_column, na.rm = TRUE)) %>%
      mutate(intervall = intervall_name)
    
    # Speichere die Ergebnisse
    ergebnisse[[i]] <- mittelwerte
  } else {
    message(paste("Keine Daten für Intervall:", intervall_name))
  }
}



# Ergebnisse zusammenführen
final_df <- bind_rows(ergebnisse)
print(head(final_df))

shape_transformed <- st_transform(greater_London_area, crs = 4326)
london_outline <- st_union(shape_transformed)  # Eine zusammenhängende Fläche erstellen

# 2. Konvertiere final_df zu einem sf-Objekt
final_df_sf <- final_df %>%
  st_as_sf(coords = c("XLON", "XLAT"), crs = 4326)

# 3. Räumliche Filterung: Nur Punkte innerhalb der London Area behalten
final_df_london <- st_intersection(final_df_sf, st_sf(geometry = london_outline))

# 4. Ergebnis prüfen
print(head(final_df_london))

# Koordinaten aus sf-Objekt extrahieren
final_df_london <- final_df_london %>%
  mutate(XLON = st_coordinates(.)[, 1],  # Längengrad (Longitude)
         XLAT = st_coordinates(.)[, 2])  # Breitengrad (Latitude)

min_mittelwert <- min(final_df_london$Mittelwert, na.rm = TRUE)
max_mittelwert <- max(final_df_london$Mittelwert, na.rm = TRUE)

# Ergebnisse anzeigen
print(paste("Minimum des Mittelwerts:", min_mittelwert))
print(paste("Maximum des Mittelwerts:", max_mittelwert))


breaks <- c(min_mittelwert, 0.4, 0.6, 0.9, 1.1, 1.9, 2.1, max_mittelwert)
color_scale <- colorBin(
  palette = c("#bcf7f7", "#6ad5fc", "#589afc", "darkgrey" ,"#FFCC99", "red", "darkred"),  # Farbverlauf
  domain = final_df_london$Mittelwert,
  bins = breaks
)

# Plot der Kacheln auf einer Karte für das erste Intervall
htmlIntervall <- final_df_london %>%
  filter(intervall == "Tier 4 London") # Pre Corona, First Lockdown UK, Return to relaxing restrictions, Three Tier System, Second Lockdown UK, End Second Lockdown, Tier 4 London

# Leaflet-Karte erstellen
leaflet(htmlIntervall) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Punkte für die Kacheln hinzufügen
  addCircleMarkers(
    lng = ~XLON,  # Longitude
    lat = ~XLAT,  # Latitude
    radius = 1,
    color = ~color_scale(Mittelwert),  # Farbskala für Mittelwerte
    fillOpacity = 0.8,
    stroke = FALSE,
    label = ~paste0("Mittelwert: ", round(Mittelwert, 2))
  ) %>%
  
  # Legende hinzufügen
  addLegend(
    pal = color_scale,
    values = htmlIntervall$Mittelwert,
    title = "Mittelwert der Bewegungsdaten",
    opacity = 1
  ) %>%
  
  # Benutzerdefinierter Titel hinzufügen
  htmlwidgets::prependContent(
    htmltools::tags$div(
      style = "position: absolute; top: 10px; left: 50%; transform: translateX(-50%);
               z-index: 999; background: transparent; padding: 10px; border-radius: 5px;",
      htmltools::tags$h2("London Movement Data - Four Tier in London")
    )
  ) %>%
  
  # Karte speichern (optional)
  saveWidget("data/London Movement/mean_TierFourLondonI.html")

preCoronaData <- final_df_london %>%
  filter(intervall == "Pre Corona") %>%
  select(Mittelwert)
summary(preCoronaData$Mittelwert)

firstLockdown <- final_df_london %>%
  filter(intervall == "First Lockdown UK") %>%
  select(Mittelwert)
summary(firstLockdown$Mittelwert)

returnRelaxing <- final_df_london %>%
  filter(intervall == "Return to relaxing restrictions") %>%
  select(Mittelwert)
summary(returnRelaxing$Mittelwert)

ThreeTireSystem <- final_df_london %>%
  filter(intervall == "Three Tier System") %>%
  select(Mittelwert)
summary(ThreeTireSystem$Mittelwert)

SecondLockdownUK <- final_df_london %>%
  filter(intervall == "Second Lockdown UK") %>%
  select(Mittelwert)
summary(SecondLockdownUK$Mittelwert)

EndSecondLockdown <- final_df_london %>%
  filter(intervall == "End Second Lockdown") %>%
  select(Mittelwert)
summary(EndSecondLockdown$Mittelwert)

Tier4London <- final_df_london %>%
  filter(intervall == "Tier 4 London") %>%
  select(Mittelwert)
summary(Tier4London$Mittelwert)

summary_table <- final_df_london %>%
  st_drop_geometry() %>%  # Geometrie entfernen für die Statistik
  group_by(intervall) %>%
  summarise(
    Min = min(Mittelwert, na.rm = TRUE),
    Q1 = quantile(Mittelwert, 0.25, na.rm = TRUE),
    Median = median(Mittelwert, na.rm = TRUE),
    Mean = mean(Mittelwert, na.rm = TRUE),
    Q3 = quantile(Mittelwert, 0.75, na.rm = TRUE),
    Max = max(Mittelwert, na.rm = TRUE),
    SD = sd(Mittelwert, na.rm = TRUE),
    N = n()  # Anzahl der Beobachtungen
  )

# Ergebnis anzeigen
print(summary_table)

final_df_london$intervall <- factor(final_df_london$intervall, 
                                    levels = c("Pre Corona", 
                                               "First Lockdown UK", 
                                               "Return to relaxing restrictions", 
                                               "Three Tier System", 
                                               "Second Lockdown UK", 
                                               "End Second Lockdown", 
                                               "Tier 4 London"))

custom_colors <- c(
  "Pre Corona" = "#008000",              
  "First Lockdown UK" = "#C70039",      
  "Return to relaxing restrictions" = "#DAF7A6",
  "Three Tier System" = "#FFC300",       
  "Second Lockdown UK" = "#C70039",     
  "End Second Lockdown" = "#DAF7A6",
  "Tier 4 London" = "#FFC300"
)

# Splitte die Daten: Ausreißer und Nicht-Ausreißer
main_data <- final_df_london %>% filter(Mittelwert <= 1.0)
outliers <- final_df_london %>% filter(Mittelwert > 1.0)

# Plot erstellen
ggplot() +
  # Boxplots für die Hauptdaten bis 1.0
  geom_boxplot(
    data = main_data,
    aes(x = factor(intervall), y = Mittelwert, fill = intervall),
    color = "black",
    outlier.size = 0.5,
    outlier.colour = "lightgrey"
  ) +
  # Separate Punkte für die Ausreißer
  geom_point(
    data = outliers,
    aes(x = factor(intervall), y = Mittelwert),
    color = "#4c514a",
    size = 0.8,
    shape = 16
  ) +
  scale_fill_manual(values = custom_colors) +
  coord_cartesian(ylim = c(0, 1.0)) +
  labs(
    title = "Comparison of the mobility data through time intervals",
    subtitle = "Outliers > 1.0 (darkgrey) are compromised to focus on the Boxplots",
    x = "Interval",
    y = "Mean of mobility data"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


ggplot(summary_table, aes(x = intervall, y = Mean, fill = intervall)) +
  # Balken für die Mittelwerte
  geom_bar(stat = "identity", color = "black") +
  
  # Linie, die die Mittelwerte verbindet
  geom_line(aes(group = 1), color = "blue", linetype = "dashed", size = 0.8, show.legend = FALSE) +
  
  # Punkte auf der Linie für die Mittelwerte
  geom_point(color = "black", size = 2, show.legend = FALSE) +
  
  # Titel und Achsentitel
  labs(
    title = "Mean Mobility Data by Interval",
    x = "Interval",
    y = "Mean of Mobility Data"
  ) +
  
  # Farbanpassung
  scale_fill_manual(values = custom_colors) +
  
  # Stil und Achsentext anpassen
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tabelle erstellen
gt_table <- gt(summary_table) %>%
  tab_header(
    title = "Summary Statistics",
    subtitle = "Mean, Median, SD, Min, Max and Q1-Q3"
  )

# Als HTML speichern
gtsave(gt_table, "data/London Movement/summary_table.html")



# Daten für den Zeitstrahl
timeline_data <- intervalle %>%
  mutate(
    start_date = as.Date("2020-01-01") + start,  # Startdatum aus dem Tag des Jahres berechnen
    end_date = as.Date("2020-01-01") + end      # Enddatum aus dem Tag des Jahres berechnen
  )

timeline_data$intervall <- factor(timeline_data$intervall, levels = rev(c(
  "Pre Corona",
  "First Lockdown UK",
  "Return to relaxing restrictions",
  "Three Tier System",
  "Second Lockdown UK",
  "End Second Lockdown",
  "Tier 4 London"
)))

# Plot erstellen
ggplot(timeline_data) +
  # Segmente für jedes Intervall
  geom_segment(
    aes(x = start_date, xend = end_date, y = intervall, yend = intervall, color = intervall),
    linewidth = 4  # Dicke der Linien
  ) +
  # Punkte für Start- und Endzeiten
  geom_point(
    aes(x = start_date, y = intervall),
    size = 3, color = "black"
  ) +
  geom_point(
    aes(x = end_date, y = intervall),
    size = 3, color = "black"
  ) +
  # Titel und Achsen
  labs(
    title = "Timeline of Mobility Intervals",
    x = "Date",
    y = "Intervals"
  ) +
  scale_color_manual(values = custom_colors) +  # Farben für die Intervalle
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"  # Entferne die Legende, falls nicht benötigt
  )
