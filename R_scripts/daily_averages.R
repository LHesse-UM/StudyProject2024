library(ggplot2)
library(dplyr)
library(mgcv)  # Falls du GAM verwendest

# Falls `daily_avg` noch ein sf-Objekt ist, Geometrie entfernen
daily_avg <- st_drop_geometry(daily_avg)

# Sicherstellen, dass `AGG_DAY_PERIOD` als Date-Format vorliegt
daily_avg <- daily_avg %>%
  mutate(AGG_DAY_PERIOD = as.Date(AGG_DAY_PERIOD))

# Definition der Covid-19-Zeiträume als Intervalle mit definierter Reihenfolge
intervals <- data.frame(
  start = as.Date(c("2020-01-01", "2020-03-26", "2020-05-11", "2020-10-14", 
                    "2020-11-05", "2020-12-02", "2020-12-21")),  
  end = as.Date(c("2020-03-25", "2020-05-10", "2020-10-13", "2020-11-04", 
                  "2020-12-01", "2020-12-20", "2020-12-31")),
  
  # Hier wird die Reihenfolge der Intervalle in der Legende definiert!
  label = factor(c("Pre-Corona", "First Lockdown", "Return to Relaxing Restrictions", 
                   "Three-Tier System", "Second Lockdown", "End of Second Lockdown", 
                   "Four-Tier System"),
                 levels = c("Pre-Corona", "First Lockdown", "Return to Relaxing Restrictions", 
                            "Three-Tier System", "Second Lockdown", "End of Second Lockdown", 
                            "Four-Tier System"))  # Definierte Reihenfolge
)

# Plot erstellen
ggplot() +
  
  # Hintergrundfarbige Intervalle hinzufügen
  geom_rect(data = intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            alpha = 0.2, inherit.aes = FALSE) +
  
  # Punkte plotten
  geom_point(data = daily_avg, aes(x = AGG_DAY_PERIOD, y = mean_movement), color = "red", alpha = 0.5) +
  
  # Genaue Glättungslinie mit GAM
  geom_smooth(data = daily_avg, aes(x = AGG_DAY_PERIOD, y = mean_movement), 
              method = "gam", formula = y ~ s(x, bs = "cs"), 
              color = "black", se = FALSE) +
  
  # Labels & Titel
  labs(title = "Daily Average of Movement Data",
       x = "Date",
       y = "Average Movement",
       fill = "Covid-19 Intervals") +
  
  # Farben für Intervalle
  scale_fill_manual(values = c("Pre-Corona" = "#2E8B57", 
                               "First Lockdown" = "red", 
                               "Return to Relaxing Restrictions" = "#90EE90",
                               "Three-Tier System" = "#FFA500",
                               "Second Lockdown" = "red",
                               "End of Second Lockdown" = "#90EE90",
                               "Four-Tier System" = "#FFA500")) +
  
  # X-Achse als Datum formatieren
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  
  # Design anpassen
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # X-Achse lesbarer machen


######################################################################################################
##### in different london areas
library(dplyr)
library(ggplot2)
library(sf)

# Boroughs in Regionen gruppieren
greater_London_area <- greater_London_area %>%
  mutate(region = case_when(
    BOROUGH %in% c("Camden", "City of London", "Hackney", "Hammersmith and Fulham", "Islington", 
                   "Kensington and Chelsea", "Lambeth", "Southwark", "Tower Hamlets", "Westminster") ~ "Central",
    BOROUGH %in% c("Barnet", "Enfield", "Haringey") ~ "North",
    BOROUGH %in% c("Bromley", "Croydon", "Kingston upon Thames", "Merton", 
                   "Richmond upon Thames", "Sutton", "Wandsworth") ~ "South",
    BOROUGH %in% c("Barking and Dagenham", "Bexley", "Greenwich", "Havering", 
                   "Lewisham", "Newham", "Redbridge", "Waltham Forest") ~ "East",
    BOROUGH %in% c("Brent", "Ealing", "Hillingdon", "Hounslow") ~ "West",
    TRUE ~ "West"
  ))

greater_London_regions <- greater_London_area %>%
  group_by(region) %>%
  summarise(geometry = st_union(geometry))  # Alle Boroughs pro Region zu einem Polygon zusammenfassen

# Plot der zusammengefassten Regionen
ggplot(data = greater_London_regions) +
  geom_sf(aes(fill = region), color = "black") +
  ggtitle("London Regions as Aggregated Polygons") +
  scale_fill_manual(values = c("Central" = "red", 
                               "North" = "blue", 
                               "South" = "green", 
                               "East" = "orange", 
                               "West" = "purple")) +
  theme_minimal()

## with boroughs
greater_London_boroughs <- greater_London_area %>%
  group_by(BOROUGH) %>%
  summarise(geometry = st_union(geometry))

ggplot() +
  
  # **Regionen als farbige Flächen**
  geom_sf(data = greater_London_regions, aes(fill = region), color = "black", alpha = 0.8) +
  
  # **Borough-Grenzen als schwarze Linien**
  geom_sf(data = greater_London_boroughs, fill = NA, color = "black", size = 0.3) +
  
  # Titel
  ggtitle("Regional Classification of London with Borough Borders") +
  
  # Farben für die Regionen
  scale_fill_manual(values = c("Central" = "red", 
                               "North" = "blue", 
                               "South" = "green", 
                               "East" = "orange", 
                               "West" = "purple")) +
  
  theme_minimal()


data_london <- st_as_sf(data_london, sf_column_name = "geometry")  # Sicherstellen, dass data_london ein sf-Objekt ist
greater_London_regions <- st_transform(greater_London_regions, st_crs(data_london)) 

data_london <- st_join(data_london, greater_London_regions, left = TRUE)  # Join mit den Regionen

daily_avg_region <- data_london %>%
  group_by(region, AGG_DAY_PERIOD) %>%
  summarise(mean_movement = mean(mean_column, na.rm = TRUE), .groups = "drop")

daily_avg_region <- daily_avg_region %>%
  mutate(AGG_DAY_PERIOD = as.Date(AGG_DAY_PERIOD))

intervals <- intervals %>%
  mutate(start = as.Date(start),
         end = as.Date(end))

ggplot(daily_avg_region, aes(x = AGG_DAY_PERIOD, y = mean_movement, color = region)) +
  
  # Punkte für jede Region
  geom_point(alpha = 0.4) +
  
  # Glättungslinie für jede Region
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1.2) +
  
  # Covid-19-Intervalle
  geom_rect(data = intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            alpha = 0.2, inherit.aes = FALSE) +
  
  # Labels & Titel
  labs(title = "Daily Average of Movement Data by London Regions",
       x = "Date",
       y = "Average Movement",
       color = "Region",
       fill = "Covid-19 Intervals") +
  
  # Farben für Regionen
  scale_color_manual(values = c("Central" = "red", 
                                "North" = "blue", 
                                "South" = "green", 
                                "East" = "orange", 
                                "West" = "purple")) +
  
  # Farben für Intervalle
  scale_fill_manual(values = c("Pre-Corona" = "#2E8B57", 
                               "First Lockdown" = "red", 
                               "Return to Relaxing Restrictions" = "#90EE90",
                               "Three-Tier System" = "#FFA500",
                               "Second Lockdown" = "red",
                               "End of Second Lockdown" = "#90EE90",
                               "Four-Tier System" = "#FFA500")) +
  
  # X-Achse formatieren
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############################################################################################
###### Covid Zahlen ######
library(ggplot2)
library(dplyr)
library(readr)
library(zoo)

# 📌 1️⃣ COVID-19-Fälle einlesen & vorbereiten
covid_cases <- read_delim("C:/Users/t.krumrein/IdeaProjects/StudyProject2024/data/Covid/Covid_cases_per_day.csv",
                          delim = ";",  # Korrektes Argument für das Trennzeichen
                          col_types = cols(
                            Date = col_date(format = "%d.%m.%Y"),  # Datumsformat richtig setzen
                            `New Corona Cases UK` = col_double()  # Zahlen als Double speichern
                          ))

# Spalten umbenennen für einfachere Handhabung
colnames(covid_cases) <- c("date", "new_cases")

covid_cases <- covid_cases %>%
  mutate(date = as.Date(date, format = "%d.%m.%Y"))

# Falls `daily_avg` noch ein sf-Objekt ist, Geometrie entfernen
daily_avg <- st_drop_geometry(daily_avg)

# Sicherstellen, dass `AGG_DAY_PERIOD` als Date-Format vorliegt
daily_avg <- daily_avg %>%
  mutate(AGG_DAY_PERIOD = as.Date(AGG_DAY_PERIOD))

covid_cases <- covid_cases %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))  # Falls das Format nicht passt, anpassen

# Daten zusammenführen
plot_data <- left_join(daily_avg, covid_cases, by = c("AGG_DAY_PERIOD" = "date"))

# 📌 3️⃣ Zweiachsiger Plot mit ggplot2
ggplot() +
  
  # Hintergrundfarbige Intervalle
  geom_rect(data = intervals, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = label),
            alpha = 0.2, inherit.aes = FALSE) +
  
  # Bewegung als rote Punkte (linke Achse)
  geom_point(data = plot_data, aes(x = AGG_DAY_PERIOD, y = mean_movement, color = "Movement Data"), alpha = 0.5) +
  
  # Glättungslinie für Bewegung (linke Achse)
  geom_smooth(data = plot_data, aes(x = AGG_DAY_PERIOD, y = mean_movement, color = "Smoothed Trend"), 
              method = "gam", formula = y ~ s(x, bs = "cs"), 
              se = FALSE) +
  
  # Korrekt skalierte COVID-19-Fälle als eigene Linie
  geom_line(data = plot_data, aes(x = AGG_DAY_PERIOD, y = new_cases / 100000 * 0.9, color = "COVID-19 Cases"), 
            linewidth = 1) +
  
  # Unabhängige Y-Achsen mit richtiger Skalierung
  scale_y_continuous(
    name = "Average Movement",  # Linke Achse
    limits = c(0, 0.8),
    sec.axis = sec_axis(~ . * 100000, name = "New COVID-19 Cases per Day")  # Rechte Achse richtig skaliert
  ) +
  
  # Labels & Titel
  labs(title = "Daily Average of Movement Data & COVID-19 Cases",
       x = "Date",
       fill = "Covid-19 Intervals",
       color = "Legend") +  # **Nur eine gemeinsame Legende für Farben**
  
  # Farben für Intervalle
  scale_fill_manual(values = c("Pre-Corona" = "#2E8B57", 
                               "First Lockdown" = "red", 
                               "Return to Relaxing Restrictions" = "#90EE90",
                               "Three-Tier System" = "#FFA500",
                               "Second Lockdown" = "red",
                               "End of Second Lockdown" = "#90EE90",
                               "Four-Tier System" = "#FFA500")) +
  
  # Farben für Linien und Punkte in der Legende definieren
  scale_color_manual(values = c("Movement Data" = "red", 
                                "Smoothed Trend" = "black", 
                                "COVID-19 Cases" = "blue")) +
  
  # X-Achse formatieren
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  
  # Design anpassen
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"))  # Lesbare X-Achse & fette Legendentitel

