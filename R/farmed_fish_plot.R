# Setup ----
pacman::p_load("tidyverse", "ggplot2", "viridis", "lubridate", "sf",
               "scales", "patchwork", "forecast", "tsibble", "feasts",
               "fabletools", "ggridges", "readxl", "plotly", "rnaturalearth", "rnaturalearthdata",
               "ggh4x", "lwgeom", "ggrepel", "DescTools")

`%notin%` <- Negate(`%in%`)

fish_data <- read_excel(path = "R/data/Aquaculture_Total_Weight.xlsx", skip = 1)


  # Create plot with improved formatting ----
# Create plot with improved formatting ----
# Define custom color palette based on theme colors ----
custom_palette <- c(
  "#3E95F7",  # Light blue (position 1)
  "#006666",  # Teal (position 2)
  "#000000",  # Black
  "#F4EDCA",  # Cream/beige
  "#000080",  # Navy blue
  "#0000FF",  # Blue
  "#F5F5E5",  # Off-white
  "#99E6D9",  # Mint
  "#FF9933"   # Orange
)

# Create plot with improved formatting and custom palette ----
p <- ggplot(data = fish_data,
            mapping = aes(x = Year,
                          y = Total/1e6,  # Convert to megatonnes
                          color = Type)) +
  geom_point(size = 2) +
  geom_line(aes(linetype = Type), linewidth = 1) +
  geom_text(data = subset(fish_data, Year == 2050),
            aes(label = "Goal for 2050"),
            hjust = 1.1,
            vjust = 0.5,
            size = 5,
            family = "Sarabun") +  # Specify Sarabun font family
  scale_linetype_manual(values = c("Measured" = "solid", "Goal" = "dashed")) +
  scale_color_manual(values = c("Measured" = custom_palette[1],
                                "Goal" = custom_palette[2])) +  # Custom colors
  labs(y = "Weight of farmed fish (megatonnes)",
       x = "Year") +
  consistent_theming_graph(base_size = 16) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank()) +  # Remove gridlines
  expand_limits(x = 2050)  # Add some space for the label

p

# Save at PowerPoint-friendly size with Cairo rendering ----
ggsave(
  filename = "plots/01_fish_data_plot.png",
  plot = p,
  width = 7.5,  # Roughly 1/4 of PPT slide width
  height = 4.25, # Appropriate aspect ratio
  dpi = 300
)

