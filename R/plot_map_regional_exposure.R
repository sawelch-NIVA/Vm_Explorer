# 3. Map Visualization ----

#' Define spatial operation helper functions
#' -----------------------------------------

#' @description Make geometries valid with robust error handling
#' @param geom sf object to validate
#' @return valid sf object
ensure_valid_geometry <- function(geom) {
  if (!all(st_is_valid(geom))) {
    geom <- sf::st_make_valid(geom)
  }
  return(geom)
}

#' @description Create buffer with validity checks
#' @param geom sf object to buffer
#' @param dist buffer distance
#' @return buffered sf object
safe_buffer <- function(geom, dist) {
  buffered <- st_buffer(geom, dist)
  return(ensure_valid_geometry(buffered))
}

#' @description Perform intersection with error handling
#' @param x first sf object
#' @param y second sf object
#' @return intersection result
safe_intersection <- function(x, y) {
  # Ensure inputs are valid
  x <- ensure_valid_geometry(x)
  y <- ensure_valid_geometry(y)

  # Perform intersection with error handling
  result <- tryCatch(
    {
      st_intersection(x, y)
    },
    error = function(e) {
      # Fall back to a more robust but slower approach
      message("Standard intersection failed, using buffer approach")
      # Use small negative buffer to avoid edge issues, then positive to restore
      x_buffered <- st_buffer(x, -0.0001) %>% st_buffer(0.0001)
      y_buffered <- st_buffer(y, -0.0001) %>% st_buffer(0.0001)
      st_intersection(x_buffered, y_buffered)
    }
  )

  return(ensure_valid_geometry(result))
}

#' @description Prepare copper data with time periods
#' @param copper_data Raw copper measurement data
#' @return Processed data with time periods
prepare_copper_data <- function(copper_data) {
  copper_data %>%
    mutate(
      # Extract year from date
      YEAR = as.numeric(format(as.Date(SAMPLE_DATE), "%Y")),
      # Create time period groups
      TIME_PERIOD = case_when(
        YEAR <= 1995 ~ "1980 - 1995",
        YEAR > 1995 & YEAR <= 2010 ~ "1996 - 2010",
        YEAR > 2010 ~ "2010 - 2025",
        TRUE ~ "Unknown"
      )
    ) %>%
    # Filter to include only the three time periods we want
    filter(TIME_PERIOD %in% c("1980 - 1995", "1996 - 2010", "2010 - 2025"))
}

#' @description Convert points data to sf object with filtering
#' @param data Data with coordinates
#' @param compartment Environmental compartment to filter ("Freshwater" or "Marine/Salt Water")
#' @param bbox Bounding box for filtering
#' @return sf object with filtered points
prepare_points_sf <- function(data, compartment, bbox) {
  data %>%
    filter(ENVIRON_COMPARTMENT_SUB == compartment) %>%
    filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
    # Filter out points outside mainland Norway bbox
    filter(
      LONGITUDE >= bbox["xmin"] &
        LONGITUDE <= bbox["xmax"] &
        LATITUDE >= bbox["ymin"] &
        LATITUDE <= bbox["ymax"]
    ) %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
}

#' @description Aggregate point data by fylke and time period
#' @param points_with_fylke sf point data with fylke assignments
#' @return aggregated data frame
aggregate_by_fylke_time <- function(points_with_fylke) {
  points_with_fylke %>%
    st_drop_geometry() %>%
    group_by(fylkesnavn, TIME_PERIOD) %>%
    summarise(
      mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
      max_value = max(MEASURED_VALUE, na.rm = TRUE),
      sample_count = n(),
      .groups = "drop"
    )
}

## function: create_facet_map() ----
#' @description Create map visualization with time facets
#' @param data sf object with data to visualize
#' @param background_data sf object to use as background
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param bbox Bounding box
#' @param color_scheme Color scheme for the map ("blue", "green", etc.)
#' @param legend_title Legend title
#' @param binned Whether to use binned color scale instead of continuous
#' @return ggplot object
create_map <- function(data, background_data, title, subtitle, bbox, color_scheme, legend_title, binned = TRUE) {
  # Define color parameters based on scheme
  color_params <- list(
    "blue" = list(
      option = "mako",
      begin = 0.1,
      end = 0.9
    ),
    "green" = list(
      option = "viridis",
      begin = 0.1,
      end = 0.9
    ),
    "red" = list(
      option = "magma",
      begin = 0.1,
      end = 0.9
    )
  )

  chosen_params <- color_params[[color_scheme]]

  # Create either binned or continuous color scale
  if (binned) {
    chosen_scale <- scale_fill_viridis_b(
      option = chosen_params$option,
      name = legend_title,
      trans = "log10",
      limits = c(0.1, 10000),
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.1", "1", "10", "100", "1000", "10000"), # Explicit labels for better formatting
      na.value = "grey50",
      begin = chosen_params$begin,
      end = chosen_params$end
    )
  } else {
    chosen_scale <- scale_fill_viridis_c(
      option = chosen_params$option,
      name = legend_title,
      trans = "log10",
      limits = c(0.1, 10000),
      na.value = "grey50",
      begin = chosen_params$begin,
      end = chosen_params$end
    )
  }

  # Calculate sample counts by time period
  sample_counts <- data %>%
    st_drop_geometry() %>%
    group_by(TIME_PERIOD) %>%
    summarise(total_samples = sum(sample_count, na.rm = TRUE), .groups = "drop")

  # Create more accurate time period labels with sample counts
  data <- data %>%
    left_join(sample_counts, by = "TIME_PERIOD") %>%
    mutate(
      TIME_PERIOD_LABEL = case_when(
        TIME_PERIOD == "1980 - 1995" ~ paste0("1980 - 1995 (n = ", total_samples, ")"),
        TIME_PERIOD == "1996 - 2010" ~ paste0("1996 - 2010 (n = ", total_samples, ")"),
        TIME_PERIOD == "2010 - 2025" ~ paste0("2010 - 2025 (n = ", total_samples, ")"),
        TRUE ~ paste0(TIME_PERIOD, " (n = ", total_samples, ")")
      )
    )

  ggplot() +
    # Background layer for excluded areas (light grey)
    geom_sf(
      data = background_data,
      fill = "grey90",
      color = "white",
      size = 0.3
    ) +
    # Data layer with fylke boundaries
    geom_sf(
      data = data,
      aes(fill = mean_value),
      color = "white",
      size = 0.5
    ) +
    # Apply chosen color scale
    chosen_scale +
    # Facet by time period with updated labels
    facet_wrap(~TIME_PERIOD_LABEL, ncol = 3) +
    # Set the plot boundaries
    coord_sf(
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(55, bbox["ymax"]),
      expand = FALSE,
      datum = st_crs(4326)
    ) +
    consistent_theming_map() +
    theme(
      panel.spacing.x = unit(-14, "lines"),
      panel.spacing.y = unit(2, "lines"),
      panel.background = element_blank(),

      # Improved legend formatting
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.5, "cm"),
      legend.box.margin = margin(t = 15, b = 15),
      legend.text = element_text(size = 12, margin = margin(r = 10, t = 5)),
      legend.title.position = "left",
      legend.title = element_text(vjust = 0.85), # Vertical adjustment to align with keys
      legend.box.spacing = unit(-0.8, "cm"), # Added spacing
      # Improved year facet labels
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      strip.placement = "bottom"
    )
}


### map insets ----


# 1. Load and prepare spatial data ----

# Define expanded bounding box for mainland Norway
norway_bbox <- st_bbox(
  c(
    xmin = 3.5, # Western coast - expanded
    ymin = 40, # Southern tip - expanded
    xmax = 31.5, # Eastern border
    ymax = 71.5 # Northern tip of mainland
  ),
  crs = 4326
)

# Load the geojson file
norway_fylke_raw <- st_read(
  dsn = "R/data/Basisdata_0000_Norge_4258_Fylker_GeoJSON.geojson"
)

# Clean and prepare the fylke data
norway_fylke <- norway_fylke_raw %>%
  ensure_valid_geometry() %>%
  # Use a larger tolerance for simplification to avoid degenerate edges
  st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # Ensure explicit CRS transformation to WGS84
  st_transform(crs = 4326) %>%
  ensure_valid_geometry()

# Clip fylke data to mainland Norway bbox
norway_fylke_mainland <- st_crop(norway_fylke, norway_bbox) %>%
  ensure_valid_geometry()

# Load Norway and surrounding lands data
norway_region <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  country = c("Norway", "Sweden", "Finland", "Denmark", "Russia")
) %>%
  st_transform(crs = 4326) %>%
  ensure_valid_geometry()

# Filter to Norway only
norway_only <- norway_region %>%
  filter(admin == "Norway")

# Crop to bounding box
norway_region_clipped <- st_crop(norway_only, norway_bbox) %>%
  ensure_valid_geometry()

# Create marine waters layer
bbox_sf <- st_as_sfc(norway_bbox) %>%
  st_set_crs(4326)

marine_zone <- st_difference(bbox_sf, st_union(norway_region_clipped)) %>%
  ensure_valid_geometry()

# Create coastal buffers
coastal_buffer_distance <- 0.1 # degrees
norway_fylke_coastal_buffers <- norway_fylke_mainland %>%
  safe_buffer(dist = coastal_buffer_distance) %>%
  safe_intersection(marine_zone)

# 2. Process copper data ----

# Prepare copper data with time periods
copper_clean_filtered <- prepare_copper_data(copper_clean)

# Create freshwater and marine points
freshwater_points <- prepare_points_sf(
  copper_clean_filtered,
  "Freshwater",
  norway_bbox
)

marine_points <- prepare_points_sf(
  copper_clean_filtered,
  "Marine/Salt Water",
  norway_bbox
)

# 3. Perform spatial operations ----

# Keep only freshwater points that intersect with land
freshwater_points_land <- safe_intersection(
  freshwater_points,
  st_union(norway_region_clipped)
)

# Join with administrative regions
freshwater_with_fylke <- st_join(
  freshwater_points_land,
  norway_fylke_mainland %>% dplyr::select(fylkesnavn),
  join = st_intersects
) %>%
  filter(!is.na(fylkesnavn))

# Join marine points with coastal buffers
marine_with_fylke <- st_join(
  marine_points,
  norway_fylke_coastal_buffers %>% dplyr::select(fylkesnavn),
  join = st_intersects
) %>%
  filter(!is.na(fylkesnavn))

# Print summaries to check data distribution
cat("Freshwater points per time period:\n")
print(table(freshwater_with_fylke$TIME_PERIOD))

cat("\nMarine points per time period:\n")
print(table(marine_with_fylke$TIME_PERIOD))

# 4. Aggregate data ----

# Aggregate data by fylke and time period
freshwater_fylke_data <- aggregate_by_fylke_time(freshwater_with_fylke)
marine_fylke_data <- aggregate_by_fylke_time(marine_with_fylke)

# 5. Prepare map data ----

# Properly clip fylke to land
norway_land_area <- st_union(norway_region_clipped)

# Ensure fylke is correctly clipped to land for freshwater map
fylke_land_proper <- safe_intersection(norway_fylke_mainland, norway_land_area)

# Prepare data for freshwater (land) map
fylke_land_with_data <- fylke_land_proper %>%
  left_join(freshwater_fylke_data, by = "fylkesnavn")

# Prepare data for marine (coastal) map
fylke_coastal_with_data <- norway_fylke_coastal_buffers %>%
  left_join(marine_fylke_data, by = "fylkesnavn")

# Create background datasets
# For freshwater map - all fylke areas
freshwater_background <- norway_fylke_mainland

# For marine map - all coastal buffer areas
marine_background <- norway_fylke_coastal_buffers

# 6. Create and save separate maps ----

# Create freshwater map
freshwater_map <- create_map(
  data = fylke_land_with_data,
  background_data = freshwater_background,
  bbox = norway_bbox,
  color_scheme = "green", # Use green-based color scheme for freshwater
  legend_title = "Mean Copper Concentration (µg/l)",
  binned = TRUE
)

# Create marine map
marine_map <- create_map(
  data = fylke_coastal_with_data |> filter(!is.na(TIME_PERIOD)),
  background_data = marine_background,
  bbox = norway_bbox,
  color_scheme = "blue", # Use blue-based color scheme for marine
  legend_title = "Mean Copper Concentration (µg/l)",
  binned = TRUE
)

# Add borders to Norway in both plots for better context
# First create a function to add Norway outline
add_norway_outline <- function(plot) {
  plot +
    geom_sf(
      data = norway_region_clipped,
      fill = NA,
      color = "grey30",
      size = 0.2,
      linetype = "dotted"
    )
}

# Apply outline to both maps
freshwater_map <- add_norway_outline(freshwater_map)
marine_map <- add_norway_outline(marine_map)

# Display maps (optional)
print(freshwater_map)
print(marine_map)

ggsave(
  "plots/freshwater_copper_map.png",
  freshwater_map,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "plots/marine_copper_map.png",
  marine_map,
  width = 12,
  height = 8,
  dpi = 300
)

# 7. Create Map with Inset for Oslofjord Region ----

#' @description Create map with inset of Oslofjord region that handles faceting
#' @param main_data sf object with data for the main map
#' @param background_data sf object for background
#' @param inset_bbox bounding box for the inset region
#' @return ggplot object with inset map
#'
#'
create_map_with_inset <- function(main_data, background_data, inset_bbox = NULL) {
  # If inset_bbox is not provided, use default Oslofjord coordinates
  if (is.null(inset_bbox)) {
    inset_bbox <- st_bbox(
      c(
        xmin = 10.3, # Western side of Oslofjord
        ymin = 59.0, # Southern part of the fjord
        xmax = 11.2, # Eastern side of Oslofjord
        ymax = 60.0 # Northern part including Oslo
      ),
      crs = 4326
    )
  }

  # First, create the main map using the existing create_map function
  main_map <- create_map(
    data = main_data,
    background_data = background_data,
    bbox = norway_bbox,
    color_scheme = "green",
    legend_title = "Mean Copper Concentration (µg/l)",
    binned = TRUE
  )

  # Add the rectangle to show the inset area
  main_map <- main_map +
    geom_rect(
      xmin = inset_bbox[1], # xmin
      ymin = inset_bbox[2], # ymin
      xmax = inset_bbox[3], # xmax
      ymax = inset_bbox[4], # ymax
      fill = NA,
      colour = "black",
      size = 0.6
    )

  # Create a separate inset map focusing on the Oslofjord region
  # This will be one map without facets
  oslofjord_inset <- ggplot(norway_region_clipped) +
    # Background layer
    geom_sf(
      data = background_data |> st_crop(y = inset_bbox),
      fill = "grey90",
      color = "white",
      size = 0.3
    ) +
    # Data layer with fylke boundaries
    geom_sf(
      data = main_data |> st_crop(y = inset_bbox),
      aes(fill = mean_value),
      color = "white",
      size = 0.5
    ) +
    # Apply the same color scale as the main map
    scale_fill_viridis_b(
      option = "viridis",
      name = "Mean Copper Concentration (µg/l)",
      trans = "log10",
      limits = c(0.1, 10000),
      breaks = c(0.1, 1, 10, 100, 1000, 10000),
      labels = c("0.1", "1", "10", "100", "1000", "10000"),
      na.value = "grey50",
      begin = 0.1,
      end = 0.9
    ) +
    # Add the same faceting as the main map, but keep the facet labels smaller
    facet_wrap(~TIME_PERIOD, ncol = 3) +
    # Set the Oslofjord coordinates
    coord_sf(
      xlim = c(inset_bbox[1], inset_bbox[3]),
      ylim = c(inset_bbox[2], inset_bbox[4]),
      expand = TRUE,
      datum = st_crs(4326)
    ) +
    # Add Norway outline
    geom_sf(
      data = norway_region_clipped |> st_crop(y = inset_bbox),
      fill = NA,
      color = "grey30",
      size = 0.2,
      linetype = "dotted"
    ) +
    geom_sf_label(aes(label = fylkesnavn))
  # Simplified theme for the inset
  theme_void() +
    theme(
      legend.position = "none",
      strip.text = element_blank(),
      strip.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, size = 0.3)
    )

  # Use cowplot to combine the main map and the inset
  library(cowplot)

  # Create the combined plot
  combined_plot <- ggdraw(main_map) +
    draw_plot(
      oslofjord_inset,
      # Position in bottom right corner
      x = 0.75, # distance from left
      y = 0.05, # distance from bottom
      width = 0.25, # width of inset
      height = 0.25 # height of inset
    )

  return(combined_plot)
}

# Define Oslofjord bounding box
oslofjord_bbox <- st_bbox(
  c(
    xmin = 10.3, # Western side of Oslofjord
    ymin = 59.0, # Southern part of the fjord
    xmax = 11.2, # Eastern side of Oslofjord
    ymax = 60.0 # Northern part including Oslo
  ),
  crs = 4326
)

# Create the map with inset
# freshwater_map_with_inset <- create_map_with_inset(
#   main_data = fylke_land_with_data,
#   background_data = freshwater_background,
#   inset_bbox = oslofjord_bbox
# )
#
# # Display the map with inset
# print(freshwater_map_with_inset)
#
# # Save the map
# ggsave(
#   "plots/freshwater_copper_map_with_oslofjord_inset.png",
#   freshwater_map_with_inset,
#   width = 12,
#   height = 8,
#   dpi = 300
# )
#
# # Similar code for marine map
# marine_map_with_inset <- create_map_with_inset(
#   main_data = fylke_coastal_with_data |> filter(!is.na(TIME_PERIOD)),
#   background_data = marine_background,
#   inset_bbox = oslofjord_bbox
# )
#
# # Display and save the marine map with inset
# print(marine_map_with_inset)
# ggsave(
#   "plots/marine_copper_map_with_oslofjord_inset.png",
#   marine_map_with_inset,
#   marine_map,
#   width = 12,
#   height = 8,
#   dpi = 300
# )
