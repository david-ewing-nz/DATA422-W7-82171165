# 1. Load necessary libraries
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
library(ggplot2)
library(viridis)
library(plotly)

# -------------------------------------------
# Deliverable 1: Create a basic map centered on New Zealand in leaflet
# -------------------------------------------

#nz_leaflet_map <- leaflet() |>
#  addTiles() |>
#  setView(lng = 174.0, lat = -40.9, zoom = 5)

# Display the basic leaflet map
#nz_leaflet_map

# Create a basic map centered on New Zealand with a title
nz_leaflet_map <- leaflet() |>
  addTiles() |>
  setView(lng = 174.0, lat = -40.9, zoom = 5) |>
  addControl(
    html = "<h3>Deliverable 1: Create a basic map centered on New Zealand in leaflet</h3>", 
    position = "topright"
  )

# Display the basic leaflet map with the title
nz_leaflet_map

# -------------------------------------------
# Deliverable 2: Add markers to the map for major cities
# -------------------------------------------

# Create a dataset with city names, latitudes, longitudes, and populations
nz_cities <- tibble(
  city = c("Auckland", "Wellington", "Christchurch"),
  lat = c(-36.8485, -41.2865, -43.5321),
  lng = c(174.7633, 174.7762, 172.6362),
  population = c(1624000, 215900, 381500)
)

# Add markers for major cities to the leaflet map
#nz_cities_map <- leaflet(nz_cities) |>
#  addTiles() |>
#  setView(lng = 174.0, lat = -40.9, zoom = 5) |>
#  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population:", population))

# Display the map with markers
#nz_cities_map

nz_cities_map <- leaflet(nz_cities) |>
  addTiles() |>
  setView(lng = 174.0, lat = -40.9, zoom = 5) |>
  addMarkers(~lng, ~lat, popup = ~paste(city, "<br>Population:", population)) |>
  addControl(
    html = "<h3>Deliverable 2: Add markers to the map for major cities</h3>", 
    position = "topright"
  )

# Display the map with the title
nz_cities_map


# -------------------------------------------
# Deliverable 3: Generate a heatmap based on population data for the major cities
# -------------------------------------------

# Generate a heatmap using population data for major cities
#nz_heatmap <- leaflet(nz_cities) |>
#  addTiles() |>
#  setView(lng = 174.0, lat = -40.9, zoom = 5) |>
#  addHeatmap(lng = ~lng, lat = ~lat, intensity = ~population, radius = 15, blur = 20)

# Display the heatmap
#nz_heatmap

# Generate a heatmap with a custom title using addControl
nz_heatmap <- leaflet(nz_cities) |>
  addTiles() |>
  setView(lng = 174.0, lat = -40.9, zoom = 5) |>
  addHeatmap(lng = ~lng, lat = ~lat, intensity = ~population, radius = 15, blur = 20) |>
  addControl(
    html = "<h3>Deliverable 3: Generate a heatmap based on population data for the major cities</h3>", 
    position = "topright"
  )

# Display the heatmap with title
nz_heatmap

# -------------------------------------------
# Deliverable 4: Create a choropleth map using New Zealand Statistics data with sf and ggplot
# -------------------------------------------

# Load geoJSON and CSV data
nz_geo <- st_read("../assignment/nz_ta.geojson", quiet = TRUE)
population_data <- read_csv("../assignment/nz_territory_2016_population.csv", show_col_types = FALSE) |>
  rename(territory = nz_territory, population = `2016_population`) |>
  filter(!is.na(population))

# Merge the population data with the geoJSON data
nz_data <- left_join(nz_geo, population_data, by = c("TA2016_NAM" = "territory"))

# Create a basic choropleth map using ggplot2
choropleth_map <- ggplot(nz_data) +
  geom_sf(aes(fill = population)) +
  scale_fill_viridis_c(option = "C") +
  theme_void() +
  labs(title = "Deliverable 4: Create a choropleth map using New Zealand Statistics data with sf and ggplot", fill = "Population")

# Display the choropleth map
choropleth_map

# 5. Enhance the map with log scale and more styling








# -------------------------------------------
# Deliverable 5: Customize a map of your own, combining all the techniques learned
# -------------------------------------------


enhanced_map <- ggplot(nz_data) +
  geom_sf(aes(fill = log10(population))) +
  scale_fill_viridis_c(option = "C", direction = -1) +
  theme_void() +
  labs(
    title    = "Deliverable 5: Customize a map of your own, combining all the techniques learned (Log Scale)", 
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

# Display the enhanced map
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

