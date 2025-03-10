# Install and load the package
pacman::p_load("rfishbase")
# Get trophic levels for specific species
# You can use scientific names
copper_species <- copper_species_lookup |>
  mutate(
    SAMPLE_SPECIES = SAMPLE_SPECIES |>
      str_remove("\\(([^\\)]+)\\)") |>
      str_trim(),
    species_group,
    .keep = "none"
  )

if (!file.exists("R/data/trophic_data.csv")) {
  trophic_data <- rfishbase::ecology(
    species_list = copper_species |>
      pull(SAMPLE_SPECIES),
    fields = c("Species", "FoodTroph")
  )
  write_csv(trophic_data, file = "R/data/trophic_data.csv")
}

trophic_data <- read_csv("R/data/trophic_data.csv") |>
  left_join(copper_species, by = c("Species" = "SAMPLE_SPECIES"))

# 4 hits for Salmo trutta, but they're all about the same

trophic_data <- trophic_data |>
  group_by(Species) |>
  reframe(MeanTrophicLevel = mean(FoodTroph) |> round(1))

ggplot(trophic_data) +
  geom_col(aes(
    x = Species,
    y = FoodTroph,
    group = Species,
    fill = species_group
  )) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
