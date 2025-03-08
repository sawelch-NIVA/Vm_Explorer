# PCA Analysis for Copper Concentration Data ----
# Separated by Biota and Non-biota samples

# Load required packages ----
library(tidyverse)
library(FactoMineR)  # For PCA
library(factoextra)  # For visualizing PCA results
library(lubridate)   # For date handling
library(gridExtra)   # For arranging multiple plots

# Data preparation function ----
prepare_pca_data <- function(data, compartment_filter) {
  data %>%
    # Filter based on compartment type
    filter(ENVIRON_COMPARTMENT %in% compartment_filter) %>%
    # Select relevant variables
    dplyr::select(
      # Time variables
      SAMPLE_DATE,
      # Location variables
      SITE_GEOGRAPHIC_FEATURE,
      # Medium variables
      ENVIRON_COMPARTMENT,
      ENVIRON_COMPARTMENT_SUB,
      SAMPLE_SPECIES,
      SAMPLE_TISSUE,
      # Response variable
      MEASURED_VALUE,
      MEASURED_UNIT
    ) %>%
    # Create standardized date variables
    mutate(
      # Extract year and month for temporal patterns
      sample_year = year(SAMPLE_DATE),
      sample_month = month(SAMPLE_DATE),
      # Create seasonal variable
      season = case_when(
        sample_month %in% c(12, 1, 2) ~ "Winter",
        sample_month %in% c(3, 4, 5) ~ "Spring",
        sample_month %in% c(6, 7, 8) ~ "Summer",
        sample_month %in% c(9, 10, 11) ~ "Fall"
      )
    ) %>%
    # Standardize units if needed
    mutate(
      # Ensure measurements are in consistent units - adapt this based on actual data units
      MEASURED_VALUE_STD = case_when(
        MEASURED_UNIT == "µg/kg ww" ~ MEASURED_VALUE,
        MEASURED_UNIT == "mg/kg ww" ~ MEASURED_VALUE * 1000,
        MEASURED_UNIT == "µg/kg dw" ~ MEASURED_VALUE,
        MEASURED_UNIT == "mg/kg dw" ~ MEASURED_VALUE * 1000,
        TRUE ~ MEASURED_VALUE
      ),
      # Log-transform concentrations
      log_conc = log10(MEASURED_VALUE_STD + 1) # Adding 1 to handle potential zeros
    )
}

# PCA function ----
run_pca <- function(data, title_prefix) {
  # Convert categorical variables to factors
  data <- data %>%
    mutate(across(c(SITE_GEOGRAPHIC_FEATURE, ENVIRON_COMPARTMENT,
                    ENVIRON_COMPARTMENT_SUB, season), as.factor))

  # Handle missing values for PCA
  data_complete <- data %>%
    filter(complete.cases(.))

  # Check if we have enough data for PCA
  if(nrow(data_complete) < 5) {
    warning(paste0("Not enough complete data for ", title_prefix, " PCA"))
    return(NULL)
  }

  # Select PCA variables - adapt as needed based on your research question
  pca_vars <- model.matrix(~ 0 +
                             sample_year +
                             season +
                             SITE_GEOGRAPHIC_FEATURE +
                             log_conc,
                           data = data_complete) %>%
    as.data.frame()

  # Run PCA
  pca_result <- PCA(pca_vars, graph = FALSE)

  # Create plots
  p1 <- fviz_eig(pca_result, addlabels = TRUE) +
    ggtitle(paste0(title_prefix, " - Scree Plot"))

  p2 <- fviz_pca_var(pca_result,
                     col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE) +
    ggtitle(paste0(title_prefix, " - Variables"))

  p3 <- fviz_pca_ind(pca_result,
                     col.ind = "cos2",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE) +
    ggtitle(paste0(title_prefix, " - Samples"))

  p4 <- fviz_pca_biplot(pca_result,
                        col.var = "contrib",
                        col.ind = "cos2",
                        repel = TRUE,
                        ggtheme = theme_minimal()) +
    ggtitle(paste0(title_prefix, " - Biplot"))

  # Get variable contributions
  var_contrib <- get_pca_var(pca_result)$contrib
  print(paste0("Variable contributions for ", title_prefix, ":"))
  print(round(var_contrib[, 1:3], 2))

  # Return results
  return(list(
    pca_result = pca_result,
    plots = list(scree = p1, vars = p2, samples = p3, biplot = p4),
    data = data_complete
  ))
}

# Create additional visualizations based on PCA findings ----
create_additional_plots <- function(data, title_prefix) {
  # Skip if no data
  if(is.null(data)) return(NULL)

  p1 <- ggplot(data, aes(x = SITE_GEOGRAPHIC_FEATURE, y = log_conc, fill = ENVIRON_COMPARTMENT_SUB)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
      title = paste0(title_prefix, " - Concentration by Location"),
      x = "Geographic Feature",
      y = "Log10 Copper Concentration",
      fill = "Sub-Compartment"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Only create temporal plot if we have enough years of data
  if(length(unique(data$sample_year)) > 3) {
    p2 <- ggplot(data, aes(x = sample_year, y = log_conc, color = ENVIRON_COMPARTMENT_SUB)) +
      geom_smooth(method = "loess", se = TRUE) +
      facet_wrap(~ SITE_GEOGRAPHIC_FEATURE) +
      theme_minimal() +
      labs(
        title = paste0(title_prefix, " - Temporal Trends"),
        x = "Year",
        y = "Log10 Copper Concentration",
        color = "Sub-Compartment"
      )
  } else {
    p2 <- ggplot(data, aes(x = season, y = log_conc, fill = ENVIRON_COMPARTMENT_SUB)) +
      geom_boxplot() +
      facet_wrap(~ SITE_GEOGRAPHIC_FEATURE) +
      theme_minimal() +
      labs(
        title = paste0(title_prefix, " - Seasonal Patterns"),
        x = "Season",
        y = "Log10 Copper Concentration",
        fill = "Sub-Compartment"
      )
  }

  return(list(p1, p2))
}

# Main analysis workflow ----

# 1. Biota analysis ----
biota_data <- prepare_pca_data(copper_clean, "Biota")
biota_pca <- run_pca(biota_data, "Biota")

# 2. Non-biota analysis ----
nonbiota_data <- prepare_pca_data(copper_clean, c("Aquatic", "Sediment", "Soil"))
nonbiota_pca <- run_pca(nonbiota_data, "Non-Biota")

# 3. Generate additional plots ----
biota_additional_plots <- create_additional_plots(biota_pca$data, "Biota")
nonbiota_additional_plots <- create_additional_plots(nonbiota_pca$data, "Non-Biota")

# 4. Save key results ----
# Save PCA plots
if(!is.null(biota_pca)) {
  ggsave("plots/biota_pca_vars.png", biota_pca$plots$vars, width = 10, height = 8)
  ggsave("plots/biota_pca_biplot.png", biota_pca$plots$biplot, width = 10, height = 8)
}

if(!is.null(nonbiota_pca)) {
  ggsave("plots/nonbiota_pca_vars.png", nonbiota_pca$plots$vars, width = 10, height = 8)
  ggsave("plots/nonbiota_pca_biplot.png", nonbiota_pca$plots$biplot, width = 10, height = 8)
}

# 5. Special tissue-specific analysis for biota ----
if(!is.null(biota_pca) && !is.null(biota_pca$data)) {
  # Check if we have enough samples with tissue information
  if(sum(!is.na(biota_pca$data$SAMPLE_TISSUE)) > 10) {
    # Analyze tissue-specific patterns
    tissue_plot <- ggplot(biota_pca$data,
                          aes(x = SAMPLE_TISSUE, y = log_conc, fill = SAMPLE_SPECIES)) +
      geom_boxplot() +
      theme_minimal() +
      labs(
        title = "Copper Concentration by Tissue Type",
        x = "Tissue",
        y = "Log10 Copper Concentration",
        fill = "Species"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggsave("plots/biota_tissue_concentration.png", tissue_plot, width = 10, height = 8)
  }
}

# 6. Print key findings for PCA dimensions ----
if(!is.null(biota_pca$pca_result)) {
  summary_biota <- summary(biota_pca$pca_result)
  cat("\nBiota PCA Summary:\n")
  print(summary_biota$eig)

  # Extract and print interpretations of first two dimensions
  cat("\nBiota PCA - Key Variables in Dimension 1:\n")
  print(dimdesc(biota_pca$pca_result, axes = 1))

  cat("\nBiota PCA - Key Variables in Dimension 2:\n")
  print(dimdesc(biota_pca$pca_result, axes = 2))
}

if(!is.null(nonbiota_pca$pca_result)) {
  summary_nonbiota <- summary(nonbiota_pca$pca_result)
  cat("\nNon-Biota PCA Summary:\n")
  print(summary_nonbiota$eig)

  # Extract and print interpretations of first two dimensions
  cat("\nNon-Biota PCA - Key Variables in Dimension 1:\n")
  print(dimdesc(nonbiota_pca$pca_result, axes = 1))

  cat("\nNon-Biota PCA - Key Variables in Dimension 2:\n")
  print(dimdesc(nonbiota_pca$pca_result, axes = 2))
}
