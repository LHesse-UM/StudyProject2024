
# Load the libraries
library(sf)
library(dplyr)

setwd("~/Desktop/Analysis")

# Einladen RDATA Tobi
load("/Users/timlehmann/Downloads/intervals.RData")

# umbennenen
PreCorona <- Pre_Corona
rm(Pre_Corona)
FirstLockdown <- First_Lockdown_UK
rm(First_Lockdown_UK)
ReturnToRelaxingRestrictions <- Return_to_relaxing_restrictions
rm(Return_to_relaxing_restrictions)
ThreeTierSystem <- Three_Tier_System
rm(Three_Tier_System)
SecondLockdown <- Second_Lockdown_UK
rm(Second_Lockdown_UK)
EndSecondLockdown <- End_Second_Lockdown
rm(End_Second_Lockdown)
FourTierSystem <- Tier_4_London
rm(Tier_4_London)

pois_Bus_Sub_Sight_Parks <- st_read("MobilityIntervalls/pois_Bus_Sub_Sight_Parks.geojson")  

#########
# 100x100 Polygons



#########

### Pre Corona
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(PreCorona, "sf")) {
  PreCorona <- st_as_sf(PreCorona, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_PreCorona <- st_join(PreCorona, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_PreCorona <- movement_PreCorona %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_PreCorona, "MobilityIntervalls/1PreCorona/result_PreCorona.geojson", driver = "GeoJSON")


### First Lockdown
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(FirstLockdown, "sf")) {
  FirstLockdown <- st_as_sf(FirstLockdown, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_FirstLockdown <- st_join(FirstLockdown, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_FirstLockdown <- movement_FirstLockdown %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_FirstLockdown, "MobilityIntervalls/2FirstLockdown/result_FirstLockdown.geojson", driver = "GeoJSON")


### Return to Relaxing Restirictions
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(ReturnToRelaxingRestrictions, "sf")) {
  ReturnToRelaxingRestrictions <- st_as_sf(ReturnToRelaxingRestrictions, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_ReturnToRelaxingRestrictions <- st_join(ReturnToRelaxingRestrictions, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_ReturnToRelaxingRestrictions <- movement_ReturnToRelaxingRestrictions %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_ReturnToRelaxingRestrictions, "MobilityIntervalls/3ReturnToRelaxingRestrictions/result_ReturnToRelaxingRestrictions.geojson", driver = "GeoJSON")


### Three Tier System
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(ThreeTierSystem, "sf")) {
  ThreeTierSystem <- st_as_sf(ThreeTierSystem, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_ThreeTierSystem <- st_join(ThreeTierSystem, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_ThreeTierSystem <- movement_ThreeTierSystem %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_ThreeTierSystem, "MobilityIntervalls/4ThreeTierSystem/result_ThreeTierSystem.geojson", driver = "GeoJSON")


### Second Lockdown
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(SecondLockdown, "sf")) {
  SecondLockdown <- st_as_sf(SecondLockdown, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_SecondLockdown <- st_join(SecondLockdown, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_SecondLockdown <- movement_SecondLockdown %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_SecondLockdown, "MobilityIntervalls/5SecondLockdown/result_SecondLockdown.geojson", driver = "GeoJSON")



### End of Second Lockdown
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(EndSecondLockdown, "sf")) {
  EndSecondLockdown <- st_as_sf(EndSecondLockdown, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_EndSecondLockdown <- st_join(EndSecondLockdown, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_EndSecondLockdown <- movement_EndSecondLockdown %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_EndSecondLockdown, "MobilityIntervalls/6EndOfSecondLockdown/result_EndSecondLockdown.geojson", driver = "GeoJSON")


### Four Tier System
# 1. GeoJSON-Dateien laden
# schon geschehen
if (!inherits(FourTierSystem, "sf")) {
  FourTierSystem <- st_as_sf(FourTierSystem, coords = c("XLON", "XLAT"), crs = 4326)  # Ersetze lon/lat mit deinen Spaltennamen
}

# 2. Räumlichen Join durchführen (Punkte mit Polygons verknüpfen)
movement_FourTierSystem <- st_join(FourTierSystem, pois_Bus_Sub_Sight_Parks, join = st_intersects)

# 3. Punkte filtern: Nur Punkte, die innerhalb eines Polygons liegen
movement_FourTierSystem <- movement_FourTierSystem %>% filter(!is.na(Name))  

# 4. Gefilterte Punkte mit Stadioninformation speichern
st_write(movement_FourTierSystem, "MobilityIntervalls/7FourTierSystem/result_FourTierSystem.geojson", driver = "GeoJSON")


###########
result_PreCorona <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/1PreCorona/result_PreCorona.geojson")
result_FirstLockdown <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/2FirstLockdown/result_FirstLockdown.geojson")
result_ReturnToRelaxingRestrictions <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/3ReturnToRelaxingRestrictions/result_ReturnToRelaxingRestrictions.geojson")
result_ThreeTierSystem <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/4ThreeTierSystem/result_ThreeTierSystem.geojson")
result_SecondLockdown <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/5SecondLockdown/result_SecondLockdown.geojson")
result_EndOfSecondLockdown <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/6EndOfSecondLockdown/result_EndSecondLockdown.geojson")
result_FourTierSystem <- st_read("/Users/timlehmann/Desktop/Analysis/MobilityIntervalls/7FourTierSystem/result_FourTierSystem.geojson")


# 1. Durchschnitt berechnen pro Objekt (Name) für Zeitintervalle
average_PreCorona <- result_PreCorona %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "PreCorona")                  # Zeitintervall hinzufügen

average_FirstLockdown <- result_FirstLockdown %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "FirstLockdown")              # Zeitintervall hinzufügen

average_ReturnToRelaxingRestrictions <- result_ReturnToRelaxingRestrictions %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "ReturnToRelaxingRestrictions") # Zeitintervall hinzufügen

average_ThreeTierSystem <- result_ThreeTierSystem %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "ThreeTierSystem")              # Zeitintervall hinzufügen

average_SecondLockdown <- result_SecondLockdown %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "SecondLockdown")              # Zeitintervall hinzufügen

average_EndOfSecondLockdown <- result_EndOfSecondLockdown %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "EndOfSecondLockdown")              # Zeitintervall hinzufügen

average_FourTierSystem <- result_FourTierSystem %>%
  group_by(Name, type) %>%                          # Gruppieren nach Name und Type
  summarise(
    mean_value = mean(mean_column, na.rm = TRUE),   # Durchschnitt berechnen
    .groups = "drop"                                # Gruppierung nach der Operation aufheben
  ) %>%
  mutate(interval = "FourTierSystem")              # Zeitintervall hinzufügen

# Average pro Name
combined_intervals <- bind_rows(
  average_PreCorona,
  average_FirstLockdown,
  average_ReturnToRelaxingRestrictions,
  average_ThreeTierSystem,
  average_SecondLockdown,
  average_EndOfSecondLockdown,
  average_FourTierSystem
)
# Ergebnis speichern
st_write(combined_intervals, "MobilityIntervalls/combined_intevals.geojson", driver = "GeoJSON")

# Average pro Type
combined_intervals_type <- combined_intervals %>%
  group_by(type, interval) %>%                          # Gruppieren nach `type` und `interval`
  summarise(
    mean_value = mean(mean_value, na.rm = TRUE),       # Durchschnitt berechnen
    .groups = "drop"                                    # Gruppierung nach der Operation aufheben
  )
# Ergebnis speichern
st_write(combined_intervals_type, "MobilityIntervalls/combined_intevals_type.geojson", driver = "GeoJSON")