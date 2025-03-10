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
  "forecast", "tsibble",
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
  "extrafont",
  "gtable",
  "grid",
  "cowplot",
  "magick"
)

extrafont::choose_font("Sarabun")
consistent_theming_map <- function(base_size) theme_void(base_family = "Sarabun", base_size = 14)
consistent_theming_graph <- function(base_size) theme_minimal(base_family = "Sarabun", base_size = 14)

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
# Generates maps of mean exposure per fylke for 20-year period, freshwater and marine
source(file = "R/plot_map_regional_exposure.R")

# 4. Biota Tissue Concentrations Visualization ----
source(file = "R/plot_density_tissue_concentration.R")


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
