# theme.R ----
# Simple theming for ggplot2 and plotly visualizations
# Using Sarabun font for consistent typography

# Load required packages
library(showtext)
library(ggplot2)
library(plotly)
library(viridis)
library(RColorBrewer)

# Font setup ----
setup_font <- function() {
  font_add_google("Sarabun", "sarabun")
  showtext_auto()
}

# Themes ----

# Default color palettes
default_palettes <- list(
  continuous = viridis(10),
  categorical = brewer.pal(8, "Set2")
)

# ggplot2 theme function
theme_custom <- function(base_size = 12, style = "paper") {
  # Adjust sizes based on style
  sizes <- if (style == "presentation") {
    list(
      title = 16,
      subtitle = 14,
      axis = 12,
      text = 10
    )
  } else {
    list(
      title = 14,
      subtitle = 12,
      axis = 10,
      text = 8
    )
  }

  # Create theme
  theme_minimal(base_size = base_size, base_family = "sarabun") %+replace%
    theme(
      # Text elements
      plot.title = element_text(
        family = "sarabun",
        face = "bold",
        size = sizes$title
      ),
      plot.subtitle = element_text(
        family = "sarabun",
        size = sizes$subtitle
      ),

      # Axis styling
      axis.title = element_text(
        family = "sarabun",
        size = sizes$axis
      ),
      axis.text = element_text(
        family = "sarabun",
        size = sizes$text
      ),

      # Legend styling
      legend.title = element_text(
        family = "sarabun",
        face = "bold"
      ),
      legend.position = if (style == "paper") "right" else "bottom",

      # Grid styling
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),

      # Facet styling
      strip.text = element_text(
        family = "sarabun",
        face = "bold"
      ),
      strip.background = element_rect(
        fill = "grey90",
        color = NA
      )
    )
}

# Plotly theme function
plotly_theme <- function(p, style = "paper", title = NULL, log_y = FALSE) {
  # Font settings based on style
  font_size <- if (style == "presentation") 14 else 12

  # Base layout
  layout_settings <- list(
    font = list(family = "Sarabun", size = font_size),
    title = title,
    xaxis = list(
      title = list(font = list(family = "Sarabun")),
      gridcolor = "grey90"
    ),
    yaxis = list(
      title = list(font = list(family = "Sarabun")),
      gridcolor = "grey90",
      type = if (log_y) "log" else NULL
    ),
    legend = list(
      orientation = if (style == "paper") "v" else "h",
      y = if (style == "paper") 1 else -0.2
    )
  )

  # Apply layout
  p %>% layout(layout_settings)
}

# Convenience wrappers ----

# Apply theme to ggplot
apply_theme_gg <- function(
  plot,
  style = "paper",
  log_scale = FALSE,
  categorical = FALSE
) {
  # Apply theme
  plot <- plot + theme_custom(style = style)

  # Apply scales based on type
  if (categorical) {
    if ("fill" %in% names(plot$mapping)) {
      plot <- plot + scale_fill_brewer(palette = "Set2")
    }
    if ("color" %in% names(plot$mapping) || "colour" %in% names(plot$mapping)) {
      plot <- plot + scale_color_brewer(palette = "Set2")
    }
  } else {
    if ("fill" %in% names(plot$mapping)) {
      plot <- plot +
        scale_fill_viridis_c(
          option = "viridis",
          trans = if (log_scale) "log10" else "identity"
        )
    }
    if ("color" %in% names(plot$mapping) || "colour" %in% names(plot$mapping)) {
      plot <- plot +
        scale_color_viridis_c(
          option = "viridis",
          trans = if (log_scale) "log10" else "identity"
        )
    }
  }

  return(plot)
}

# Apply theme to plotly
apply_theme_plotly <- function(
  plot,
  style = "paper",
  title = NULL,
  log_y = FALSE
) {
  plotly_theme(plot, style = style, title = title, log_y = log_y)
}
