library(dplyr)
library(lubridate)
library(spdep)
library(sf)

# Sicherstellen, dass `time` als day-of-year in `mergedData2` existiert
mergedData2 <- mergedData2 %>%
  mutate(time = yday(AGG_DAY_PERIOD))  # `AGG_DAY_PERIOD` als Datum in day-of-year konvertieren

# Intervalle definieren
intervalle <- data.frame(
  intervall = c("Pre_Corona", "First_Lockdown_UK", "Return_to_relaxing_restrictions", 
                "Three_Tier_System", "Second_Lockdown_UK", "End_Second_Lockdown", "Tier_4_London"),
  start = c(0, 85, 130, 287, 309, 336, 355),
  end = c(84, 129, 286, 308, 335, 354, 365)
)

# DataFrames für jedes Intervall erstellen
Pre_Corona <- mergedData2 %>%
  filter(!is.na(time) & time >= 0 & time <= 84)

First_Lockdown_UK <- mergedData2 %>%
  filter(!is.na(time) & time >= 85 & time <= 129)

Return_to_relaxing_restrictions <- mergedData2 %>%
  filter(!is.na(time) & time >= 130 & time <= 286)

Three_Tier_System <- mergedData2 %>%
  filter(!is.na(time) & time >= 287 & time <= 308)

Second_Lockdown_UK <- mergedData2 %>%
  filter(!is.na(time) & time >= 309 & time <= 335)

End_Second_Lockdown <- mergedData2 %>%
  filter(!is.na(time) & time >= 336 & time <= 354)

Tier_4_London <- mergedData2 %>%
  filter(!is.na(time) & time >= 355 & time <= 365)


###### MoransI calc
