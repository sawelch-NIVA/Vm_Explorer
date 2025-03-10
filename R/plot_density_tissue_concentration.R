# Standardized function to create density plots
create_density_plot <- function(data, site_type = NULL) {
  # Filter data if site_type is provided
  if (!is.null(site_type)) {
    data <- data %>% filter(SITE_GEOGRAPHIC_FEATURE == site_type)
  }

  # Create the plot with standardized settings
  output <- ggplot(
    data,
    aes(
      x = MEASURED_VALUE,
      y = reorder(tissue_with_n, MEASURED_VALUE, median),
      fill = final_group
    )
  ) +
    # geom_dotplot(
    #   dotsize = 0.01,
    #   stroke = 0,
    #   binwidth = 5,
    #   binpositions = "all"
    # ) +
    # Ridge density plot with semi-transparency
    stat_density_ridges(geom = "density_ridges_gradient", bandwidth = 0.15) +

    # Log scale for concentrations with formatted labels
    scale_fill_brewer(palette = "Set3", name = "") +
    scale_x_log10(
      breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
      limits = c(0.01, 100000)
    ) +
    facet_nested_wrap(
      ~ short_name_ordered + tissue_with_n_ordered,
      dir = "h",
      strip.position = "right",
      scales = "free_y",
      # nest_line = element_line(linetype = 1),
      ncol = 1,
      shrink = TRUE
    ) +
    # Clear labeling
    labs(
      x = "Copper Concentration (mg/kg wet weight, log scale)",
      y = NULL
    ) +
    # Base theme
    ggridges::theme_ridges() +
    # Custom consistent theme
    consistent_theming_graph(base_size = 8) +
    theme(
      # Strip styling - keep original approach
      strip.text = element_text(hjust = 0.9, colour = "#505050"),
      strip.clip = "off",

      # Axis styling
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      axis.text.x = element_text(size = 10),

      # Legend styling
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),

      # Grid styling
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # Add padding to avoid cut-off
      plot.margin = margin(t = 20, r = 20),

      # Clean background for both plots
      panel.background = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      panel.spacing.y = unit(-1, "lines"),
      panel.border = element_blank()
    )

  insertLayer(
    output,
    geom_vline(
      xintercept = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
      color = "grey90",
      size = 0.5
    ),
    after = 1
  )

  return(output)
}

insertLayer <- function(ggplot_object, after = 0, ...) {
  #  ggplot_object     : Plot object
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)

  if (after < 0) after <- after + length(ggplot_object$layers)

  if (!length(ggplot_object$layers)) ggplot_object$layers <- list(...) else
    ggplot_object$layers <- append(ggplot_object$layers, list(...), after)

  return(ggplot_object)
}

# Data preparation steps (kept from original code)
copper_species_lookup <- read_csv("R/data/copper_species_lookup.csv")
biota_data <- copper_clean %>%
  filter(ENVIRON_COMPARTMENT == "Biota" & !is.na(SAMPLE_TISSUE)) |>
  dplyr::select(
    ENVIRON_COMPARTMENT_SUB,
    SAMPLE_SPECIES,
    SAMPLE_TISSUE,
    MEASURED_VALUE,
    SITE_GEOGRAPHIC_FEATURE
  ) |>
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
  ) |>
  left_join(trophic_fish_data, by = c("SAMPLE_SPECIES" = "Species"))

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

# Calculate sample counts for each species-tissue combination
sample_counts <- species_tissue_data %>%
  group_by(species_tissue, final_group, SITE_GEOGRAPHIC_FEATURE, short_name) %>%
  mutate(
    SITE_GEOGRAPHIC_FEATURE = case_match(
      SITE_GEOGRAPHIC_FEATURE,
      "Coastal, fjord" ~ "SW",
      "Lake, pond, pool, reservoir" ~ "FW",
      "River, stream, canal" ~ "FW"
    )
  ) |>
  reframe(
    n_samples = n(),
    .groups = "drop",
    SAMPLE_TISSUE,
    SITE_GEOGRAPHIC_FEATURE,
    MEASURED_VALUE,
    MeanTrophicLevel,
    short_name
  ) %>%
  mutate(
    # Add sample count to display label
    tissue_with_n = paste0(SAMPLE_TISSUE, " (", n_samples, ")"),
    tissue_with_n_ordered = factor(
      tissue_with_n,
      levels = levels(reorder(
        tissue_with_n,
        MEASURED_VALUE,
        median,
        decreasing = TRUE
      ))
    ),
    # Fix: Create a short_name with trophic level first, then create the factor
    short_name_with_trophic = ifelse(
      is.na(MeanTrophicLevel),
      short_name,
      paste0(short_name, " (T=", MeanTrophicLevel, ")")
    ),
    short_name_ordered = factor(
      short_name_with_trophic,
      levels = levels(reorder(
        short_name_with_trophic,
        MEASURED_VALUE,
        median,
        decreasing = TRUE
      ))
    )
  ) |>
  mutate(
    tissue_with_n_ordered = case_when(
      str_detect(tissue_with_n_ordered, "Gill") ~
        str_replace(tissue_with_n_ordered, "Gill", "Gill [dry]"),
      TRUE ~ as.character(tissue_with_n_ordered)
    )
  )
# Create the freshwater and marine density plots
density_plot_fw <- create_density_plot(sample_counts, "FW") +
  coord_cartesian(ylim = c(0, 5), clip = "off")

density_plot_sw <- create_density_plot(sample_counts, "SW") +
  coord_cartesian(ylim = c(0, 5), clip = "off")

# Display plots
density_plot_fw
density_plot_sw

# Save plots
ggsave(
  "plots/plot_density_tissue_freshwater.png",
  plot = density_plot_fw,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "plots/plot_density_tissue_marine.png",
  plot = density_plot_sw,
  width = 10,
  height = 6,
  dpi = 300
)
