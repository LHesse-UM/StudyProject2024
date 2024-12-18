library(arrow)
library(sf)
library(dplyr)
library(leaflet)
library(httr)
library(cluster)  
library(htmltools)
library(htmlwidgets) 
library(reticulate)

setwd("C:/r/StudyProject2024")
load("C:/r/StudyProject2024/DataPreprocessingForSTDBSCAN.RData")

january_filtered_data <- mergedData2 %>%
  filter(time >= 12 & time <= 18)

set.seed(42)  # Für Reproduzierbarkeit

sampled_data <- mergedData2 %>%
  sample_frac(0.001)  # Nimm 1% der Daten

data_matrix <- as.matrix(sampled_data %>% select(XLON, XLAT, time, mean_column))

np <- import("numpy")
data_np <- np$array(data_matrix)

eps1 <- 0.0020 
eps2 <- 3
min_samples <- as.integer(3)

st_dbscan <- import("st_dbscan")
st_dbscan_instance <- st_dbscan$ST_DBSCAN(eps1 = eps1, eps2 = eps2, min_samples = min_samples)

st_dbscan_instance$fit(data_np)

labels <- st_dbscan_instance$labels

table(labels)
print(labels)

labels_r <- py_to_r(labels)
if (length(labels_r) != nrow(sampled_data)) {
  stop("Die Länge der Labels stimmt nicht mit den Daten überein!")
}

sampled_data <- sampled_data %>%
  mutate(Cluster = labels_r)
