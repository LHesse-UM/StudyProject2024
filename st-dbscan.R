# Libraries laden
library(arrow)
library(sf)
library(dplyr)
library(leaflet)
library(httr)
library(cluster)  
library(htmltools)
library(htmlwidgets) 
library(reticulate)

mergedData2 <- merge(pre_processed_movement, distinct_LONLAT, by = "LONLAT_ID")

mergedData2 <- mergedData2 %>%
  mutate(time = as.numeric(as.Date(AGG_DAY_PERIOD) - as.Date("2020-01-01")))

# Auf London begrenzen
mergedData2 <- mergedData2 %>%
  filter(XLAT > 51.275, XLAT < 51.7, XLON > -0.52, XLON < 0.35)

data_matrix <- as.matrix(mergedData2 %>% select(XLON, XLAT, time, mean_column))

np <- import("numpy")
data_np <- np$array(data_matrix)

eps1 <- 0.001 
eps2 <- 2
min_samples <- as.integer(2)

st_dbscan <- import("st_dbscan")
st_dbscan_instance <- st_dbscan$ST_DBSCAN(eps1 = eps1, eps2 = eps2, min_samples = min_samples)

st_dbscan_instance$fit(data_np)

labels <- st_dbscan_instance$labels