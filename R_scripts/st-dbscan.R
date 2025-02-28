library(arrow)
library(sf)
library(dplyr)
library(leaflet)
library(httr)
library(cluster)  
library(htmltools)
library(htmlwidgets) 
library(reticulate)
library(leaflet.extras)

# Set working directory and load data
setwd("C:/r/StudyProject2024")
load("C:/r/StudyProject2024/DataPreprocessingForSTDBSCAN.RData")

# Define CRS: metric (EPSG 27700) and geographic (EPSG 4326)
crs_metric <- 27700   # EPSG 27700 (meters)
crs_wgs84  <- 4326    # EPSG 4326 (lat/lon)

# Convert mergedData2 to sf, transform to metric, and add numeric X and Y
mergedData2_sf <- st_as_sf(mergedData2, coords = c("XLON", "XLAT"), crs = crs_wgs84)
mergedData2_metric <- st_transform(mergedData2_sf, crs = crs_metric)
coords_metric <- st_coordinates(mergedData2_metric)
mergedData2_metric <- mergedData2_metric %>% 
  mutate(X = coords_metric[, "X"], 
         Y = coords_metric[, "Y"])
mergedData2 <- as.data.frame(mergedData2_metric)

# Define original bounding box for Greater London (WGS84) and transform to metric
lat_min <- 51.29
lat_max <- 51.71
lon_min <- -0.51
lon_max <- 0.31
bbox_wgs84 <- st_as_sfc(st_bbox(c(xmin = lon_min, ymin = lat_min, xmax = lon_max, ymax = lat_max), crs = st_crs(crs_wgs84)))
bbox_metric <- st_transform(bbox_wgs84, crs = crs_metric)
bbox_coords <- st_bbox(bbox_metric)
x_min <- bbox_coords["xmin"]
x_max <- bbox_coords["xmax"]
y_min <- bbox_coords["ymin"]
y_max <- bbox_coords["ymax"]

# Define grid cell sizes (approx. 2km x 2km)
grid_size_x <- 2000  # meters
grid_size_y <- 2000  # meters

# Define time intervals (start and end times)
timeRanges <- list(
  c(20, 40),
  c(100, 120),
  c(200, 220),
  c(287, 307),
  c(310, 330),
  c(340, 360)
)

# Define eps2 values (in meters)
eps2_values <- c(0.001, 0.005, 0.01)

# Load boundary and line data (WGS84)
url <- "https://gis2.london.gov.uk/server/rest/services/apps/webmap_context_layer/MapServer/1/query"
params <- list(
  where = "1=1",
  outFields = "*",
  f = "geojson"
)
response <- GET(url, query = params)
linestring_data <- st_read(content(response, as = "text"), quiet = TRUE)

# Process Greater London outline
shape_transformed <- st_transform(greater_London_area, crs = crs_wgs84)
london_outline <- st_union(shape_transformed)
london_outline_sf <- st_sf(geometry = london_outline)

# Create vectors for grid cells
grid_cells_x <- seq(x_min, x_max - grid_size_x, by = grid_size_x)
grid_cells_y <- seq(y_min, y_max - grid_size_y, by = grid_size_y)
total_cells <- length(grid_cells_x) * length(grid_cells_y)
cell_counter <- 0

# Loop over eps2 values
for(eps2_current in eps2_values) {
  
  heatmap_data_per_interval <- vector("list", length(timeRanges))
  
  # Loop through grid cells
  for(x in grid_cells_x) {
    for(y in grid_cells_y) {
      
      cell_counter <- cell_counter + 1
      message(sprintf("Processing cell %d of %d (X=%.0f, Y=%.0f)", 
                      cell_counter, total_cells, x, y))
      
      # Filter data for current grid cell
      cell_data <- mergedData2 %>%
        filter(
          X > x, X <= x + grid_size_x,
          Y > y, Y <= y + grid_size_y
        )
      if(nrow(cell_data) == 0) next
      
      # Loop over time intervals
      for(i in seq_along(timeRanges)) {
        st_dbscan <- import("st_dbscan")
        np <- import("numpy")
        startTime <- timeRanges[[i]][1]
        endTime   <- timeRanges[[i]][2]
        
        # Filter data for current time interval
        interval_data <- cell_data %>%
          filter(time >= startTime & time <= endTime)
        if(nrow(interval_data) == 0) next
        
        set.seed(42)  
        data_matrix <- as.matrix(
          interval_data %>% select(X, Y, time, mean_column)
        )
        data_np <- np$array(data_matrix)
        
        # Parameters for ST-DBSCAN (in metric units)
        eps1 <- 66
        eps2 <- eps2_current
        min_samples <- as.integer(2)
        
        # Apply ST-DBSCAN
        st_dbscan_instance <- st_dbscan$ST_DBSCAN(eps1 = eps1, eps2 = eps2, min_samples = min_samples)
        st_dbscan_instance$fit(data_np)
        
        labels <- st_dbscan_instance$labels
        labels_r <- py_to_r(labels)
        interval_data <- interval_data %>% mutate(Cluster = labels_r)
        
        # Filter out noise and add heatmap data if available
        heatmap_data <- interval_data %>% filter(Cluster != -1)
        if(nrow(heatmap_data) > 0) {
          heatmap_data_per_interval[[i]] <- bind_rows(
            heatmap_data_per_interval[[i]],
            heatmap_data
          )
        }
      } 
    }
  }
  
  # Generate heatmaps for each time interval
  for(i in seq_along(timeRanges)) {
    interval_data <- heatmap_data_per_interval[[i]]
    if(!is.null(interval_data) && nrow(interval_data) > 0) {
      
      startTime <- timeRanges[[i]][1]
      endTime   <- timeRanges[[i]][2]
      
      # Convert metric coordinates back to geographic coordinates for mapping
      interval_data_sf <- st_as_sf(interval_data, coords = c("X", "Y"), crs = crs_metric)
      interval_data_ll <- st_transform(interval_data_sf, crs = crs_wgs84)
      coords_ll <- st_coordinates(interval_data_ll)
      interval_data_ll <- interval_data_ll %>% 
        mutate(XLON = coords_ll[,1], XLAT = coords_ll[,2])
      
      combined_heatmap_map <- leaflet(interval_data_ll) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addHeatmap(
          lng = ~XLON,
          lat = ~XLAT,
          intensity = ~1,
          radius = 13,
          blur = 20,
          max = 0.4,
          minOpacity = 0.7
        ) %>%
        addPolylines(
          data = linestring_data,
          color = "blue",
          weight = 3,
          opacity = 0.8
        ) %>%
        addPolygons(
          data = london_outline_sf,
          color = "black",
          weight = 1,
          opacity = 0.8,
          fill = FALSE,
          group = "London Outline"
        ) %>%
        addLegend(
          position = "bottomright",
          colors = c("blue", "black"),
          labels = c("Thames River", "London Outline"),
          title = "Legend",
          opacity = 0.7
        ) %>%
        addControl(
          html = "<h4>ST-DBSCAN</h4>",
          position = "topright"
        )
      
      message(sprintf("Creating heatmap for Time Interval %d-%d with %d data points.",
                      startTime, endTime, nrow(interval_data)))
      
      output_file <- paste0(
        "Combined_Heatmap_Interval_",
        startTime, "_", endTime,
        "_eps2_", eps2_current, 
        ".html"
      )
      saveWidget(combined_heatmap_map, file = output_file, selfcontained = TRUE)
      
    } else {
      message(sprintf("No data for Time Interval %d-%d (eps2=%.0f m).",
                      timeRanges[[i]][1], timeRanges[[i]][2], eps2_current))
    }
  }
}
