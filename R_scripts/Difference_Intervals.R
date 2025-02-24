library(dplyr)
library(sf)
library(ggplot2)


Pre_Corona$AGG_DAY_PERIOD <- as.Date(Pre_Corona$AGG_DAY_PERIOD)
Pre_Corona_sf <- st_as_sf(Pre_Corona, coords = c("geometry"), crs = 4326)
First_Lockdown_UK$AGG_DAY_PERIOD <- as.Date(First_Lockdown_UK$AGG_DAY_PERIOD)
First_Lockdown_UK_sf <- st_as_sf(First_Lockdown_UK, coords = c("geometry"), crs = 4326)
Return_to_relaxing_restrictions$AGG_DAY_PERIOD <- as.Date(Return_to_relaxing_restrictions$AGG_DAY_PERIOD)
Return_to_relaxing_restrictions_sf <- st_as_sf(Return_to_relaxing_restrictions, coords = c("geometry"), crs = 4326)
Three_Tier_System$AGG_DAY_PERIOD <- as.Date(Three_Tier_System$AGG_DAY_PERIOD)
Three_Tier_System_sf <- st_as_sf(Three_Tier_System, coords = c("geometry"), crs = 4326)
Second_Lockdown_UK$AGG_DAY_PERIOD <- as.Date(Second_Lockdown_UK$AGG_DAY_PERIOD)
Second_Lockdown_UK_sf <- st_as_sf(Second_Lockdown_UK, coords = c("geometry"), crs = 4326)
End_Second_Lockdown$AGG_DAY_PERIOD <- as.Date(End_Second_Lockdown$AGG_DAY_PERIOD)
End_Second_Lockdown_sf <- st_as_sf(End_Second_Lockdown, coords = c("geometry"), crs = 4326)
Tier_4_London$AGG_DAY_PERIOD <- as.Date(Tier_4_London$AGG_DAY_PERIOD)
Tier_4_London_sf <- st_as_sf(Tier_4_London, coords = c("geometry"), crs = 4326)

first_date <- min(Tier_4_London$AGG_DAY_PERIOD)
first_2weeks <- Tier_4_London_sf %>%
  filter(AGG_DAY_PERIOD >= first_date & AGG_DAY_PERIOD <= (first_date + 13))
Tier_4_London_avg_first_2 <- first_2weeks %>%
  group_by(LONLAT_ID) %>%
  summarise(mean_movement = mean(mean_column, na.rm = TRUE))

last_date <- max(End_Second_Lockdown$AGG_DAY_PERIOD)
last_2weeks <- End_Second_Lockdown_sf %>%
  filter(AGG_DAY_PERIOD >= (last_date - 13) & AGG_DAY_PERIOD <= last_date)
End_Second_Lockdown_avg_last_2 <- last_2weeks %>%
  group_by(LONLAT_ID) %>%
  summarise(mean_movement = mean(mean_column, na.rm = TRUE))


ggplot(Pre_Corona_avg_first_2) +
  geom_sf(aes(color = mean_movement), size = 1) +
  scale_color_viridis_c(option = "plasma") +
  ggtitle("Durchschnittliche Bewegung – Erste zwei Wochen") +
  labs(color = "Mean Movement") +
  theme_minimal()



plot_difference_movement <- function(first_2weeks_df, last_2weeks_df, interval1, interval2){
  
  # Umwandlung in sf-Objekte sicherstellen
  first_2weeks_sf <- st_as_sf(first_2weeks_df, wkt = "geometry", crs = 4326)
  last_2weeks_sf <- st_as_sf(last_2weeks_df, wkt = "geometry", crs = 4326)
  
  # Differenz berechnen
  difference_sf <- first_2weeks_sf %>%
    inner_join(last_2weeks_sf %>% st_drop_geometry(), by = "LONLAT_ID", suffix = c("_first", "_last")) %>%
    mutate(difference = mean_movement_last - mean_movement_first)
  
  plot_title <- paste("Difference between", interval1, "and", interval2)
  
  # Plot erzeugen
  plot <- ggplot(difference_sf) +
    geom_sf(aes(color = difference), size = 1) +
    scale_color_stepsn(colours = c("darkred", "red", "orange", "lightgrey", "lightgreen", "green", "darkgreen"),
                       breaks = c(-2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2),
                       limits = c(-2, 2)) +
    labs(title = plot_title, color = "Difference") +
    theme_minimal()
  
  return(plot)
}

plot_difference_movement(Pre_Corona_avg_last_2, First_Lockdown_UK_avg_first_2, "Pre Corona", "First Lockdown")
plot_difference_movement(First_Lockdown_UK_avg_last_2, Return_to_relaxing_restrictions_avg_first_2, "First Lockdown", "Return to ralxing restrictions")
plot_difference_movement(Return_to_relaxing_restrictions_avg_last_2, Three_Tier_System_avg_first_2, "Return to ralxing restrictions", "Tier 3 System")
plot_difference_movement(Three_Tier_System_avg_last_2, Second_Lockdown_UK_avg_first_2, "Tier 3 System", "Second Lockdown")
plot_difference_movement(Second_Lockdown_UK_avg_last_2, End_Second_Lockdown_avg_first_2, "Second Lockdown", "End Second Lockdown")
plot_difference_movement(End_Second_Lockdown_avg_first_2,Tier_4_London_avg_first_2, "End Second Lockdown", "Tier 4 System")
