# Setup ----
pacman::p_load(
  "MASS", # be careful with namespacing of the many select() functions
  "tidyverse",
  "ggplot2",
  "viridis",
  "lubridate",
  "sf",
  "scales",
  "patchwork",
  "forecast",
  "tsibble",
  "feasts",
  "fabletools",
  "ggridges",
  "readxl",
  "plotly",
  "rnaturalearth",
  "rnaturalearthdata",
  "ggh4x",
  "lwgeom",
  "ggrepel",
  "extrafont"
)

extrafont::choose_font("Sarabun")
consistent_theming <- function(base_size) theme_void(base_family = "Sarabun", base_size = 14)

`%notin%` <- Negate(`%in%`)

compartments_df <- read_csv("R/data/compartments_count.csv")

copper_env <- read_excel("R/data/complete_dataset_20250226_1807.xlsx") |>
  dplyr::select(where(~ !all(is.na(.)))) |> # save some data by trimming fields that are currently irrelevant
  filter(
    ENVIRON_COMPARTMENT %notin% c("No information/not reported", "Not reported")
  ) |>
  mutate(
    MEASURED_UNIT = case_match(
      MEASURED_UNIT,
      "mg/kg t.v." ~ "mg/kg dw",
      "mg/kg v.v." ~ "mg/kg ww"
    )
  )

copper_sites <- read_excel(
  "R/data/complete_dataset_20250226_1807.xlsx",
  sheet = "Sites_Data"
)
# Copper Pollution Visualization in Norway ----

# Clean and prepare the data
copper_clean <- copper_env %>%
  # Convert date strings and create time features
  mutate(
    SAMPLE_DATE = as.Date(SAMPLE_DATE, format = "%d.%m.%Y"),
    YEAR = year(SAMPLE_DATE),
    MONTH = month(SAMPLE_DATE),
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      MONTH %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  # Remove rows with missing measurement values
  filter(!is.na(MEASURED_VALUE) & !is.na(SAMPLE_DATE)) %>%
  # Log-transform measurements for visualization
  mutate(
    LOG_MEASURED_VALUE = log10(pmax(MEASURED_VALUE, 0.001))
  ) |>
  left_join(copper_sites, by = "SITE_CODE")

# 1. Basic Stats ----
summary(copper_clean$MEASURED_VALUE)
table(copper_clean$ENVIRON_COMPARTMENT)
table(copper_clean$ENVIRON_COMPARTMENT_SUB)

# 2. Time Series Visualization ----
aquatic_data <- copper_clean %>%
  # Include both Aquatic and specific Biota data
  filter(
    ENVIRON_COMPARTMENT == "Aquatic" |
      (ENVIRON_COMPARTMENT == "Biota" &
        ENVIRON_COMPARTMENT_SUB == "Biota/Aquatic")
  ) %>%
  mutate(YearMonth = floor_date(SAMPLE_DATE, "month")) |>
  filter(ENVIRON_COMPARTMENT_SUB != "Not reported") %>%
  # Create a new grouping variable that includes units for Biota
  mutate(
    plot_group = case_when(
      ENVIRON_COMPARTMENT == "Biota" & MEASURED_UNIT == "mg/kg ww" ~
        "Biota/Aquatic (ww)",
      ENVIRON_COMPARTMENT == "Biota" & MEASURED_UNIT == "mg/kg dw" ~
        "Biota/Aquatic (dw)",
      TRUE ~ ENVIRON_COMPARTMENT_SUB
    )
  )

ts_compartment_data <- aquatic_data %>%
  group_by(plot_group, YearMonth) %>%
  summarise(
    mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(plot_group, YearMonth)

# Initialize the plot
ts_compartment_plotly <- plot_ly()

# Better color scheme for categorical data
# Using colorbrewer palette which is better for categorical variables
compartments <- unique(ts_compartment_data$plot_group)
colors <- RColorBrewer::brewer.pal(max(8, length(compartments)), "Set1")

for (i in 1:length(compartments)) {
  comp_data <- ts_compartment_data %>%
    filter(plot_group == compartments[i])

  # Define base color and paler version for observed data
  base_color <- colors[i]
  pale_color <- adjustcolor(base_color, alpha.f = 0.3)

  # Add observed data with paler color but solid line
  ts_compartment_plotly <- ts_compartment_plotly %>%
    add_trace(
      data = comp_data,
      x = ~YearMonth,
      y = ~mean_value,
      type = "scatter",
      mode = "lines",
      name = paste0(compartments[i], " - Observed"),
      line = list(color = pale_color, width = 1, dash = "solid"),
      legendgroup = compartments[i]
    )

  # Only calculate trend if we have enough data points (>10)
  if (nrow(comp_data) > 10) {
    trend_model <- try(
      loess(
        log10(mean_value) ~ as.numeric(YearMonth),
        data = comp_data,
        span = 0.3
      ),
      silent = TRUE
    )
    if (!inherits(trend_model, "try-error")) {
      comp_data$trend <- 10^predict(trend_model)
      ts_compartment_plotly <- ts_compartment_plotly %>%
        add_trace(
          data = comp_data,
          x = ~YearMonth,
          y = ~trend,
          type = "scatter",
          mode = "lines",
          name = paste0(compartments[i], " - Trend"),
          line = list(color = base_color, width = 3, dash = "solid"),
          legendgroup = compartments[i]
        )
    }
  }
}

ts_compartment_plotly <- ts_compartment_plotly %>%
  plotly::layout(
    title = "Copper Concentrations by Environmental Compartment",
    xaxis = list(title = "Date"),
    yaxis = list(
      title = "Concentration",
      type = "log",
      # Regular log scale formatting with cleaner ticks
      dtick = 1, # Log-scale tick increment of 1 means powers of 10
      tickformat = ".1f"
    ),
    hovermode = "closest"
  )

# View the interactive plot
ts_compartment_plotly

# 3. Map Visualization ----

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

# Load the geojson file with careful geometry handling
norway_fylke_raw <- st_read(
  dsn = "R/data/Basisdata_0000_Norge_4258_Fylker_GeoJSON.geojson"
)

# Clean and prepare the fylke data with careful geometry handling
norway_fylke <- norway_fylke_raw %>%
  # Force validity using lwgeom for more robust repair
  sf::st_make_valid() %>%
  # Use a larger tolerance for simplification to avoid degenerate edges
  st_simplify(preserveTopology = TRUE, dTolerance = 0.001) %>%
  # Ensure explicit CRS transformation to WGS84
  st_transform(crs = 4326)

# Double check validity and fix any remaining issues
valid_check <- st_is_valid(norway_fylke)
if (any(!valid_check)) {
  cat("Warning: Some geometries are still invalid after cleaning\n")
  # Use buffer with a small positive value instead of 0
  norway_fylke <- st_buffer(norway_fylke, 0.001)
  # Final validity check
  norway_fylke <- sf::st_make_valid(norway_fylke)
}

# Clip fylke data to mainland Norway bbox
norway_fylke_mainland <- st_crop(norway_fylke, norway_bbox)
# Fix any potential issues from cropping
norway_fylke_mainland <- sf::st_make_valid(norway_fylke_mainland)

# Load Norway and surrounding lands data from rnaturalearth
norway_region <- ne_countries(
  scale = "medium",
  returnclass = "sf",
  # Include surrounding countries for context
  country = c("Norway", "Sweden", "Finland", "Denmark", "Russia")
) %>%
  # Ensure same CRS
  st_transform(crs = 4326) %>%
  # Ensure valid geometries for the countries
  sf::st_make_valid()

# remove this to show neighbouring countries
norway_only <- norway_region |> filter(admin == "Norway")

# Crop the region data to our mainland Norway bounding box
norway_region_clipped <- st_crop(norway_only, norway_bbox)
norway_region_clipped <- sf::st_make_valid(norway_region_clipped)

# For land-sea distinction, get a marine layer
# Create marine waters by getting a rectangle of the bbox and removing land
bbox_sf <- st_as_sfc(norway_bbox) %>% st_set_crs(4326)
# Use a careful approach for the difference operation
marine_zone <- st_difference(bbox_sf, st_union(norway_region_clipped))
marine_zone <- sf::st_make_valid(marine_zone)

# Create a safe buffer function with validity checks
safe_buffer <- function(geom, dist) {
  buffered <- st_buffer(geom, dist)
  # Check and repair if needed
  if (!all(st_is_valid(buffered))) {
    buffered <- sf::st_make_valid(buffered)
  }
  return(buffered)
}

# Safe intersection function
safe_intersection <- function(x, y) {
  # Ensure inputs are valid
  x <- sf::st_make_valid(x)
  y <- sf::st_make_valid(y)

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

  return(sf::st_make_valid(result))
}

# Assuming copper_clean has a DATE or YEAR column for time filtering
# Extract year from date and create a period column
copper_clean <- copper_clean %>%
  mutate(
    # If DATE is already in your data, extract year
    # Adjust the column name as needed
    YEAR = as.numeric(format(as.Date(SAMPLE_DATE), "%Y")),
    # Create time period groups
    TIME_PERIOD = case_when(
      YEAR <= 1990 ~ "1980s",
      YEAR > 1990 & YEAR <= 2010 ~ "2000s",
      YEAR > 2010 ~ "Since 2010",
      TRUE ~ "Unknown"
    )
  )

# Process copper data by water type and time period
# Filter to include only the three time periods we want
copper_clean_filtered <- copper_clean %>%
  filter(TIME_PERIOD %in% c("1980s", "2000s", "Since 2010"))

# Clean and filter freshwater points data
freshwater_points <- copper_clean_filtered %>%
  filter(ENVIRON_COMPARTMENT_SUB == "Freshwater") %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  # Filter out points outside mainland Norway bbox
  filter(
    LONGITUDE >= norway_bbox["xmin"] &
      LONGITUDE <= norway_bbox["xmax"] &
      LATITUDE >= norway_bbox["ymin"] &
      LATITUDE <= norway_bbox["ymax"]
  ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Clean and filter marine points data
marine_points <- copper_clean_filtered %>%
  filter(ENVIRON_COMPARTMENT_SUB == "Marine/Salt Water") %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE)) %>%
  # Filter out points outside mainland Norway bbox
  filter(
    LONGITUDE >= norway_bbox["xmin"] &
      LONGITUDE <= norway_bbox["xmax"] &
      LATITUDE >= norway_bbox["ymin"] &
      LATITUDE <= norway_bbox["ymax"]
  ) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Create coastal zones for each fylke with safer operations
# First, create a coastal buffer around each fylke land area
coastal_buffer_distance <- 0.1 # degrees, adjust as needed
norway_fylke_coastal_buffers <- norway_fylke_mainland %>%
  # Create a buffer around each fylke with safety checks
  safe_buffer(dist = coastal_buffer_distance) %>%
  # Only keep the marine part of each buffer
  safe_intersection(marine_zone)

# Make sure points are in the right zones using boolean operations
# Keep only freshwater points that intersect with land using safe operation
freshwater_points_land <- safe_intersection(
  freshwater_points,
  st_union(norway_region_clipped)
)

# Perform spatial joins with fylke data for land points
freshwater_with_fylke <- st_join(
  freshwater_points_land,
  norway_fylke_mainland %>% select(fylkesnavn),
  join = st_intersects
) %>%
  # Remove points that didn't get a fylke assignment
  filter(!is.na(fylkesnavn))

# Join marine points with coastal buffers to get fylke assignment
marine_with_fylke <- st_join(
  marine_points,
  norway_fylke_coastal_buffers %>% select(fylkesnavn),
  join = st_intersects
) %>%
  # Keep only points that got a fylke assignment
  filter(!is.na(fylkesnavn))

# Aggregate data by fylke and time period for both water types
freshwater_fylke_data <- freshwater_with_fylke %>%
  st_drop_geometry() %>%
  group_by(fylkesnavn, TIME_PERIOD) %>%
  summarise(
    mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
    max_value = max(MEASURED_VALUE, na.rm = TRUE),
    sample_count = n(),
    .groups = "drop"
  )

marine_fylke_data <- marine_with_fylke %>%
  st_drop_geometry() %>%
  group_by(fylkesnavn, TIME_PERIOD) %>%
  summarise(
    mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
    max_value = max(MEASURED_VALUE, na.rm = TRUE),
    sample_count = n(),
    .groups = "drop"
  )

# Find the global min and max values for consistent color scaling
all_values <- c(
  freshwater_fylke_data$mean_value,
  marine_fylke_data$mean_value
)
value_min <- min(all_values, na.rm = TRUE)
value_max <- max(all_values, na.rm = TRUE)

# Create a single joined dataset for the faceted approach
fylke_land_with_data <- norway_fylke_mainland %>%
  # Add a type indicator
  mutate(zone_type = "Freshwater") %>%
  # Join with freshwater data
  left_join(freshwater_fylke_data, by = "fylkesnavn")

fylke_coastal_with_data <- norway_fylke_coastal_buffers %>%
  # Add a type indicator
  mutate(zone_type = "Marine") %>%
  # Join with marine data
  left_join(marine_fylke_data, by = "fylkesnavn")

# Combine both datasets
all_fylke_data <- rbind(
  fylke_land_with_data,
  fylke_coastal_with_data
)

# Create a faceted map
faceted_map <- ggplot() +
  # Marine background
  # Country outlines
  # geom_sf(
  #   data = norway_region_clipped,
  #   fill = "grey",
  #    color = NA,
  #   size = 0.5
  # ) +

  # All fylke data (both land and coastal)
  geom_sf(data = all_fylke_data |> filter(!is.na(TIME_PERIOD)), aes(fill = mean_value), alpha = 1, colour = NA) +

  # ggrepel::geom_label_repel(data = all_fylke_data,
  #                           aes(label = mean_value |> round(digits = 2),
  #                               geometry = geometry),
  #                           stat = "sf_coordinates",
  #                           min.segment.length = 1,
  #                           max.overlaps = 10) +

  scale_fill_viridis(
    option = "viridis",
    name = "Mean Copper\nConcentration (µg/l)",
    trans = "log10",
    limits = c(0.1, 10000),
    na.value = "grey90"
  ) +

  # Facet by time period
  facet_wrap(~TIME_PERIOD, ncol = 3) +

  # Set the plot boundaries
  coord_sf(
    xlim = c(norway_bbox["xmin"], norway_bbox["xmax"]),
    ylim = c(55, norway_bbox["ymax"]),
    expand = FALSE,
    datum = st_crs(4326)
  ) +

  # Labels
  # labs(
  #   title = "Copper Concentrations in Norwegian Waters",
  #   subtitle = "Freshwater (Land) and Marine (Coastal Region) Measured Concentration of Total Copper, 1980-2025, by Region/Fylke"
  # ) +
  consistent_theming() +
  theme(panel.spacing.x = unit(-5, "lines"))


# View the faceted map
print(faceted_map)

# 4. Biota Tissue Concentrations Visualization ----
copper_species_lookup <- read_csv("R/data/copper_species_lookup.csv")
biota_data <- copper_clean %>%
  filter(ENVIRON_COMPARTMENT == "Biota" & !is.na(SAMPLE_TISSUE)) |>
  dplyr::select(ENVIRON_COMPARTMENT_SUB, SAMPLE_SPECIES, SAMPLE_TISSUE, MEASURED_VALUE, MEASURED_UNIT, SITE_GEOGRAPHIC_FEATURE) |>
  # Standardize taxonomic notation in species names
  mutate(
    # Preserve original names
    original_species = SAMPLE_SPECIES,
    # Standardize taxonomy notation for joining
    clean_species = str_replace_all(
      SAMPLE_SPECIES,
      c(
        "\\(Klasse\\)" = " (Class)",
        "\\(Familie\\)" = " (Family)",
        "\\(Orden\\)" = " (Order)",
        "\\(Slekt\\)" = " (Genus)"
      )
    )
  )

biota_classified <- biota_data %>%
  # Left join with our custom lookup table
  left_join(copper_species_lookup, by = c("SAMPLE_SPECIES")) %>%
  # Create fallback classification for species not in our lookup
  mutate(
    # Use species_group from lookup if available, otherwise apply fallback rules
    final_group = species_group,
    # Create a display name for the species
    display_name = case_when(
      # If we have an English name from lookup, use it with scientific name
      !is.na(Name_EN) ~ paste0(SAMPLE_SPECIES, " (", Name_EN, ")"),

      # Otherwise just use the scientific name
      TRUE ~ SAMPLE_SPECIES
    ),

    # Create a shorter display name for facets
    short_name = case_when(
      # If we have an English name, use that
      !is.na(Name_EN) ~ Name_EN,

      # Otherwise extract genus or use first part of name
      TRUE ~ sub("^([A-Za-z]+).*$", "\\1 sp.", SAMPLE_SPECIES)
    )
  )

species_tissue_data <- biota_classified %>%
  group_by(SAMPLE_SPECIES, SAMPLE_TISSUE) %>%
  filter(n() >= 5) %>% # Minimum 5 samples per species/tissue
  ungroup()

species_tissue_data <- species_tissue_data %>%
  mutate(
    # Ensure consistent ecological group ordering
    final_group = factor(
      final_group,
      levels = c(
        "Fish",
        "Molluscs",
        "Plants",
        "Worms",
        "Arthropods",
        "Benthic Organisms",
        "Other"
      )
    ),

    # Create combined species-tissue identifier for y-axis
    species_tissue = paste(short_name, "-", SAMPLE_TISSUE),

    # Create hierarchical label for faceting
    facet_label = paste(final_group, ":", short_name)
  )

# First calculate sample counts for each species-tissue combination
sample_counts <- species_tissue_data %>%
  group_by(species_tissue, final_group) %>%
  reframe(
    n_samples = n(),
    .groups = "drop",
    SAMPLE_TISSUE,
    SITE_GEOGRAPHIC_FEATURE,
    MEASURED_VALUE,
    MEASURED_UNIT,
    short_name
  ) %>%
  mutate(
    # Add sample count to display label
    tissue_with_n = paste0(SAMPLE_TISSUE, " (", n_samples, ")"),
    SITE_GEOGRAPHIC_FEATURE = case_match(SITE_GEOGRAPHIC_FEATURE,
                                         "Coastal, fjord" ~ "SW",
                                         "Lake, pond, pool, reservoir" ~ "FW",
                                         "River, stream, canal" ~ "FW"),
    tissue_with_n_ordered = factor(tissue_with_n,
                                   levels = levels(reorder(tissue_with_n, MEASURED_VALUE, median, decreasing = TRUE))),
    short_name_ordered = factor(short_name,
                            levels = levels(reorder(short_name, MEASURED_VALUE, median, decreasing = TRUE)))
  )

sample_counts_fw <- sample_counts |> filter(SITE_GEOGRAPHIC_FEATURE == "FW")
sample_counts_sw <- sample_counts |> filter(SITE_GEOGRAPHIC_FEATURE == "SW")

# Create a custom labeller function that makes only the species names italic
# TODO: For some reason this breaks tissue names...
custom_labeller <- function(labels) {
  # Check if the labels are from the 'short_name_ordered' variable
  lapply(names(labels), function(variable) {
    if (variable == "short_name_ordered") {
      # Make species names (short_name) italic
      lapply(labels[[variable]], function(x) parse(text = paste0("italic('", x, "')")))
    } else {
      # Return other labels unchanged
      labels[[variable]]
    }
  })
}

density_plot_fw <- ggplot(
  sample_counts_fw,
  aes(
    x = MEASURED_VALUE,
    y = reorder(tissue_with_n, MEASURED_VALUE, median),
    fill = final_group,
    linetype = MEASURED_UNIT
  )
) +
  # Ridge density plot with semi-transparency
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = 0.15) +
  # geom_density_ridges(alpha = 0.8, scale = 0.9, rel_min_height = 0.01) +

  # Log scale for concentrations with formatted labels
  scale_fill_brewer(palette = "Set3", name = "Organism Type") +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
    limits = c(0.01, 100000)
  ) +
  facet_nested_wrap(~ short_name_ordered + tissue_with_n_ordered,
                    dir = "h",
                    strip.position = "left",
                    scales = "free_y",
                    nest_line = element_line(linetype = 1),
                    ncol = 1,
                    # labeller = custom_labeller,
                    shrink = TRUE) +
  # Clear labeling
  labs(
  #   title = "Copper Bioaccumulation Across Marine Organisms (Norway, 1990-2025)",
  #   subtitle = "Distribution of concentration by species and tissue type",
    x = "Copper Concentration (µg/kg, log scale)",
    y = NULL,
  ) +

  # PowerPoint-friendly styling
  consistent_theming(base_size = 8) +
  theme(
    # Title styling
    # plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    # plot.subtitle = element_text(
    #   size = 12,
    #   hjust = 0.5,
    #   margin = margin(b = 20)
    # ),

    # Facet styling - add prominent section borders
    # strip.text = element_text(face = "bold", size = 14, hjust = 0),
    # strip.background = element_rect(
    #   fill = "grey90",
    #   color = "black",
    #   linewidth = 1
    # ),
    # panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    strip.text = element_text(hjust = 0.9),
    strip.clip = "off",

    # Axis styling
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10),

    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),

    # Grid styling
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),


    # Add padding to avoid cut-off
    plot.margin = margin(t = 20, r = 20)
  )

density_plot_sw <- ggplot(
  sample_counts_sw,
  aes(
    x = MEASURED_VALUE,
    y = reorder(tissue_with_n, MEASURED_VALUE, median),
    fill = final_group,
    linetype = MEASURED_UNIT
  )
) +
  # Ridge density plot with semi-transparency
  stat_density_ridges(geom = "density_ridges_gradient", bandwidth = 0.15) +
  # geom_density_ridges(alpha = 0.8, scale = 0.9, rel_min_height = 0.01) +

  # Log scale for concentrations with formatted labels
  scale_fill_brewer(palette = "Set3", name = "Organism Type") +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
    limits = c(0.01, 100000)
  ) +
  facet_nested_wrap(~ short_name_ordered + tissue_with_n_ordered,
                    dir = "h",
                    strip.position = "left",
                    scales = "free_y",
                    nest_line = element_line(linetype = 1),
                    ncol = 1,
                    # labeller = custom_labeller,
                    shrink = TRUE) +
  # Clear labeling
  labs(
    #   title = "Copper Bioaccumulation Across Marine Organisms (Norway, 1990-2025)",
    #   subtitle = "Distribution of concentration by species and tissue type",
    x = "Copper Concentration (µg/kg, log scale)",
    y = NULL,
  ) +

  # PowerPoint-friendly styling
  consistent_theming(base_size = 8) +
  theme(
    # Title styling
    # plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    # plot.subtitle = element_text(
    #   size = 12,
    #   hjust = 0.5,
    #   margin = margin(b = 20)
    # ),

    # Facet styling - add prominent section borders
    # strip.text = element_text(face = "bold", size = 14, hjust = 0),
    # strip.background = element_rect(
    #   fill = "grey90",
    #   color = "black",
    #   linewidth = 1
    # ),
    # panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    strip.text = element_text(hjust = 0.9),
    strip.clip = "off",

    # Axis styling
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.text.x = element_text(size = 10),

    # Legend styling
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),

    # Grid styling
    panel.grid.major.x = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),


    # Add padding to avoid cut-off
    plot.margin = margin(t = 20, r = 20)
  )

# TODO: split into 2 plots: FW & SW
density_plot_fw
density_plot_sw

species_stats <- species_tissue_data %>%
  group_by(final_group) %>%
  summarise(
    species_count = n_distinct(SAMPLE_SPECIES),
    tissue_types = n_distinct(SAMPLE_TISSUE),
    sample_count = n(),
    median_conc = median(MEASURED_VALUE, na.rm = TRUE),
    max_conc = max(MEASURED_VALUE, na.rm = TRUE),
    min_conc = min(MEASURED_VALUE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(median_conc))

# Print statistics
print("Bioaccumulation Statistics by Ecological Group:")
print(species_stats)


# 5.  Sustained and Acute Exposure Heatmaps ----
# Filter to aquatic samples only
# Combine freshwater and marine data with fylke information
trim_fylke_names <- function(fylke_vector) {
  str_extract(string = fylke_vector, pattern = "^[^-]*(?= -|$)")
}

combined_fylke_temporal <- bind_rows(
  freshwater_with_fylke %>%
    st_drop_geometry() %>%
    mutate(water_type = "Freshwater"),
  marine_with_fylke %>%
    st_drop_geometry() %>%
    mutate(water_type = "Marine/Salt Water")
)

# Group by fylke, year, and water type
fylke_year_data <- combined_fylke_temporal %>%
  mutate(YEAR = year(SAMPLE_DATE)) %>%
  group_by(fylkesnavn, YEAR, water_type) %>%
  summarise(
    mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
    max_value = max(MEASURED_VALUE, na.rm = TRUE),
    sample_count = n(),
    .groups = "drop"
  ) %>%
  # Filter to fylker with multiple years of data
  group_by(fylkesnavn, water_type) %>%
  filter(n() >= 3) %>% # At least 3 years of data per fylke-water type combo
  ungroup() %>%
  mutate(fylkesnavn = trim_fylke_names(fylkesnavn))

# Create sustained exposure heatmap by fylke
sustained_heatmap_fylke <- fylke_year_data %>%
  ggplot(aes(x = YEAR, y = fylkesnavn, fill = mean_value)) +
  geom_tile() +
  facet_wrap(~water_type, scales = "free_y", ncol = 1) +
  scale_fill_viridis(
    option = "viridis",
    name = "Mean Copper\nConcentration",
    trans = "log10",
    labels = comma_format()
  ) +
  labs(
    title = "Sustained Copper Exposure Levels by Fylke",
    subtitle = "Annual mean concentrations (log scale)",
    x = "Year",
    y = "Fylke"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

# View the plot
print(sustained_heatmap_fylke)

# Create acute exposure heatmap by fylke
acute_heatmap_fylke <- fylke_year_data %>%
  ggplot(aes(x = YEAR, y = fylkesnavn, fill = max_value)) +
  geom_tile() +
  facet_wrap(~water_type, scales = "free_y", ncol = 1) +
  scale_fill_viridis(
    option = "inferno",
    name = "Maximum Copper\nConcentration",
    trans = "log10",
    labels = comma_format()
  ) +
  labs(
    title = "Acute Copper Exposure Levels by Fylke",
    subtitle = "Annual maximum concentrations (log scale)",
    x = "Year",
    y = "Fylke"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

# View the plot
print(acute_heatmap_fylke)

# 6. Seasonality Analysis ----
# Extract seasonal data
combined_fylke_points <- bind_rows(
  freshwater_with_fylke %>% mutate(water_type = "Freshwater"),
  marine_with_fylke %>% mutate(water_type = "Marine/Salt Water")
)

# Process for seasonal patterns
seasonal_data_fylke <- combined_fylke_points %>%
  filter(ENVIRON_COMPARTMENT_SUB != "Not reported") %>% # Exclude "Not reported"
  mutate(
    MONTH = month(SAMPLE_DATE),
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      MONTH %in% c(9, 10, 11) ~ "Fall"
    )
  ) %>%
  group_by(fylkesnavn, water_type, MONTH, SEASON) %>%
  summarise(
    mean_value = mean(MEASURED_VALUE, na.rm = TRUE),
    median_value = median(MEASURED_VALUE, na.rm = TRUE),
    sample_count = n(),
    .groups = "drop"
  ) %>%
  # Ensure months are in order
  mutate(
    MONTH = factor(
      MONTH,
      levels = 1:12,
      labels = c(
        "Jan",
        "Feb",
        "Mar",
        "Apr",
        "May",
        "Jun",
        "Jul",
        "Aug",
        "Sep",
        "Oct",
        "Nov",
        "Dec"
      )
    ),
    SEASON = factor(SEASON, levels = c("Winter", "Spring", "Summer", "Fall"))
  ) %>%
  mutate(fylkesnavn = trim_fylke_names(fylkesnavn))

# Create monthly patterns plot with fylke faceting
monthly_plot_fylke <- ggplot(
  seasonal_data_fylke,
  aes(x = MONTH, y = mean_value, group = water_type, color = water_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~fylkesnavn, scales = "fixed") + # Facet by fylke
  scale_color_viridis(discrete = TRUE, option = "plasma", name = "Water Type") +
  scale_y_log10(labels = comma_format()) +
  labs(
    title = "Seasonal Patterns of Copper Concentrations by Fylke",
    subtitle = "Monthly averages across all years (log scale)",
    x = NULL,
    y = "Mean Copper Concentration (log scale)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7), # Smaller text for x-axis to fit in facets
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold"),
    legend.position = "right"
  )

# View the plot
print(monthly_plot_fylke)
