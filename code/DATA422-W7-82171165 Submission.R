# 0. Load necessary libraries
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(plotly)

# -------------------------------------------
# Deliverable 1: Create a basic map centered on New Zealand in leaflet (with a title)
# -------------------------------------------

# Centre of NZ at 172.8 E 41.8 S 

map_basic_nz <- leaflet() |>
  addTiles() |>
  setView(lng = 172.8, lat = -41.8, zoom = 5) |> 
addControl(
  html = "<div style='text-align:left;'><h3>Deliverable 1/Task 1:<br>Create a basic map centered on New Zealand in leaflet</h3></div>", 
  position = "topright"
)

# Display the leaflet map Deliverable 1/Task 1:
map_basic_nz

# -------------------------------------------
# Deliverable 2: Add markers to the map for major cities
# -------------------------------------------

# Create a dataset with city names, latitudes, longitudes, and populations
nz_cities <- tibble(
  city = c("Auckland", "Wellington", "Christchurch"),
  lat = c(-36.8485, -41.2865, -43.5321),
  lng = c(174.7633, 174.7762, 172.6362),
  population = c(1570000, 212000, 375000)  # 2016 population estimates
)

nz_cities_map <- leaflet(nz_cities) |>
  addTiles() |>
  setView(lng = 172.8, lat = -41.8, zoom = 5) |> 
  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population:", population)) |>
addControl(
  html = "<div style='text-align:left;'><h3>Deliverable 2/Task 2:<br>Add markers to the map for major cities (2016)</h3></div>", 
  position = "topright"
)

# Display the map with the title Deliverable 2/Task 2
nz_cities_map


# -------------------------------------------
# Deliverable 3: Generate a heatmap based on population data for the major cities
# -------------------------------------------

# Generate a heatmap with a custom title using addControl
nz_heatmap <- leaflet(nz_cities) |>
  addTiles() |>
  setView(lng = 172.8, lat = -41.8, zoom = 5) |> 
  addHeatmap(lng = ~lng, lat = ~lat, intensity = ~population, radius = 15, blur = 20) |>
  addControl(
    html = "<div style='text-align:left;'><h3>Deliverable 3/Task 3:<br>Heatmap based on population data for the major cities(2016)</h3></div>", 
    position = "topright"
  )

# Display the heatmap with title Deliverable 3/Task 3
nz_heatmap

# -------------------------------------------
# Deliverable 4: Create a choropleth map using New Zealand Statistics data with sf and ggplot
# -------------------------------------------

# Task 4: Load geoJSON
nz_geo <- st_read("assignment/nz_ta.geojson", quiet = TRUE)
ggplot(nz_geo) +
  geom_bar(aes(x = reorder(TA2016_NAM, -AREA_SQ_KM), y = AREA_SQ_KM), stat = "identity") +  # Reorder x-axis by area
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +                              # Rotate x-axis labels
  labs(
    title = "(Not required)\nArea of New Zealand Territories (Sorted by Area)",
    x = "Territory Name",
    y = "Area (sq km)"
  )
# Task 5:  Black and White plot of the NZ Territories
ggplot(nz_geo) +
  geom_sf() +      # Plot the sf object (territorial boundaries)
  theme_void() +                            
  labs(
    title = "Task 5:  Black and White plot of the NZ Territories",
    x = "Territory",
    y = "Population"
  )

# Task 6:  Read population data;  renaming column ; filter NA data
population_data <- read_csv("assignment/nz_territory_2016_population.csv", show_col_types = FALSE) |>
  rename(territory = nz_territory, population = `2016_population`) |>
  filter(!is.na(population))

#  bar chart sorted by population
ggplot(population_data) +
  geom_bar(aes(x = reorder(territory, -population), y = population), stat = "identity") +  # Sort by population in descending order
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +                              # Rotate x-axis labels for readability
  labs(
    title = "Task 6:  Read population data \nPopulation of New Zealand Territories (Sorted by Population)",
    x = "Territory",
    y = "Population"
  )
# TASK 7 Check geoJSON
unmatched_geo <- anti_join(nz_geo, population_data, by = c("TA2016_NAM" = "territory"))
if(nrow(unmatched_geo) == 0) {
  print("Task 7: All rows in geoJSON have matching data in the CSV file")
} else {
  print("Task 7: Unmatched rows in geoJSON:")
  print(unmatched_geo)
}

unmatched_population <- anti_join(population_data, nz_geo, by = c("territory" = "TA2016_NAM"))
if(nrow(unmatched_population) == 0) {
  print("Task 7: All rows in the CSV have matching data in the geoJSON file")
} else {
  print("Task 7: Unmatched rows in CSV:")
  print(unmatched_population)
}

# TASK 7: Merge the population data with the geoJSON data
nz_data <- left_join(nz_geo, population_data, by = c("TA2016_NAM" = "territory"))

# TASK8: Create a basic choropleth map using ggplot2
choropleth_map <- ggplot(nz_data) +
  geom_sf(aes(fill = population)) +
  scale_fill_viridis_c(option = "C") +
  theme_void() +
  labs(title = "Deliverable 4/TASK 8: Create a choropleth map using New Zealand Statistics data with sf and ggplot", fill = "Population")

# Display the choropleth map
choropleth_map




# -------------------------------------------
# Deliverable 5: Customize a map of your own, combining all the techniques learned
# -------------------------------------------

# TASK 9: Change colour pallet; use log scale. 
enhanced_map <- ggplot(nz_data) +
  geom_sf(aes(fill = log10(population))) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_void() +
  labs(
    title    = "Deliverable 5/TASK 9: Customize a map of your own, combining all the techniques learned (Log Scale)", 
    subtitle = "2016 Population Data from Stats NZ",
    fill     = "Log Population",
    caption  = "Source: Stats NZ"
  ) +
  theme(
    plot.title      = element_text(size = 16, face = "bold"),
    plot.subtitle   = element_text(size = 12),
    legend.position = "right",
    legend.title    = element_text(size = 10),
    legend.text     = element_text(size = 8)
  )

# TASK 9: Save png file. 
ggsave("assignment/TASK_9_map.png", plot = enhanced_map)

# Display the enhanced map Deliverable 5/TASK 9
enhanced_map


# -------------------------------------------
# Deliverable 6: Extra for Experts (Optional)
# Create an interactive choropleth map using plotly
# -------------------------------------------

# Create a basic choropleth map using ggplot2
choropleth_map <- ggplot(nz_data) +
  geom_sf(aes(fill = population)) +
  scale_fill_viridis_c(option = "C") +
  theme_void() +
  labs(title = "Deliverable 6: Interactive choropleth maps (Optional)", fill = "Population")



# Convert the choropleth map into an interactive plotly map
interactive_map <- ggplotly(choropleth_map)

# Display the interactive map
interactive_map

