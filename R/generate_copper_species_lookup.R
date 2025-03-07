# Species Lookup Table for Copper Analysis ----
# This table provides standardized English names and ecological grouping
# for species in the copper bioaccumulation dataset.
# Includes only species relevant to the current dataset.

# Create a focused species lookup table
copper_species_lookup <- tibble(
  # Scientific name as the primary key
  SAMPLE_SPECIES = c(
    # Fish species
    "Gadus morhua", "Clupea harengus", "Salmo trutta", "Salmo salar",
    "Platichthys flesus", "Melanogrammus aeglefinus", "Pollachius virens",
    "Sander lucioperca", "Esox lucius", "Perca fluviatilis",
    "Rutilus rutilus", "Abramis brama", "Leuciscus idus", "Thymallus thymallus",
    "Gadidae (Familie)", "Salmonidae (Familie)", "Actinopterygii (Klasse)",
    "Gobiidae (Familie)", "Labridae (Familie)",

    # Mollusks
    "Mytilus edulis", "Modiolus modiolus", "Ostrea edulis",
    "Littorina (Slekt)", "Littorina littorea", "Crassostrea gigas",
    "Gastropoda (Klasse)", "Erginus (Slekt)", "Lepetella (Slekt)",

    # Crustaceans
    "Carcinus maenas", "Cancer pagurus", "Homarus gammarus",
    "Litopenaeus vannamei", "Lithodidae (Familie)", "Euphausiacea (Orden)",
    "Nephrops norvegicus", "Palaemon adspersus",

    # Plants and algae
    "Fucus vesiculosus", "Laminaria hyperborea", "Ascophyllum nodosum",

    # Worms
    "Arenicola marina", "Polychaeta (Klasse)", "Nereis diversicolor",

    # Insects
    "Chironomidae (Familie)", "Chironomus plumosus",

    # Benthic organisms (general)
    "Bunndyr", "Benthic organisms",

    # Birds
    "Larus argentatus", "Cygnus cygnus"
  ),

  # English common name
  Name_EN = c(
    # Fish species
    "Atlantic Cod", "Atlantic Herring", "Brown Trout", "Atlantic Salmon",
    "European Flounder", "Haddock", "Pollock",
    "Zander", "Northern Pike", "European Perch",
    "Roach", "Common Bream", "Ide", "Grayling",
    "Cod Family", "Salmon Family", "Ray-finned Fishes",
    "Goby Family", "Wrasse Family",

    # Mollusks
    "Blue Mussel", "Horse Mussel", "European Flat Oyster",
    "Periwinkle", "Common Periwinkle", "Pacific Oyster",
    "Gastropods", "Erginus", "Lepetella",

    # Crustaceans
    "Green Shore Crab", "Edible Crab", "European Lobster",
    "Whiteleg Shrimp", "King Crab Family", "Krill Order",
    "Norway Lobster", "Dock Shrimp",

    # Plants and algae
    "Bladderwrack", "Tangle", "Knotted Wrack",

    # Worms
    "Lugworm", "Polychaete Worms", "Ragworm",

    # Insects
    "Non-biting Midges", "Plumose Midge",

    # Benthic organisms
    "Benthic Organisms", "Benthic Organisms",

    # Birds
    "Herring Gull", "Whooper Swan"
  ),

  # Ecological grouping for visualization
  species_group = c(
    # Fish species
    "Fish", "Fish", "Fish", "Fish",
    "Fish", "Fish", "Fish",
    "Fish", "Fish", "Fish",
    "Fish", "Fish", "Fish", "Fish",
    "Fish", "Fish", "Fish",
    "Fish", "Fish",

    # Mollusks
    "Mollusks", "Mollusks", "Mollusks",
    "Mollusks", "Mollusks", "Mollusks",
    "Mollusks", "Mollusks", "Mollusks",

    # Crustaceans
    "Crustaceans", "Crustaceans", "Crustaceans",
    "Crustaceans", "Crustaceans", "Crustaceans",
    "Crustaceans", "Crustaceans",

    # Plants and algae
    "Plants", "Plants", "Plants",

    # Worms
    "Worms", "Worms", "Worms",

    # Insects
    "Insects", "Insects",

    # Benthic organisms
    "Benthic Organisms", "Benthic Organisms",

    # Birds
    "Birds", "Birds"
  ),

  # Original Norwegian name where applicable
  species_name_source = c(
    # Fish species
    "Torsk", "Sild", "Ørret", "Laks",
    "Skrubbe", "Hyse", "Sei",
    "Gjørs", "Gjedde", "Abbor",
    "Mort", "Brasme", "Vederbuk", "Harr",
    "Torskefamilien", "Laksefamilien", "Strålefinnefisker",
    "Kutlingfamilien", "Leppefiskfamilien",

    # Mollusks
    "Blåskjell", "Oskjell", "Flatøsters",
    "Strandsnegl", "Vanlig strandsnegl", "Stillehavsøsters",
    "Snegler", "Erginus", "Lepetella",

    # Crustaceans
    "Strandkrabbe", "Taskekrabbe", "Europeisk hummer",
    "Vannameireke", "Kongekrabbefamilien", "Krill",
    "Sjøkreps", "Strandreke",

    # Plants and algae
    "Blæretang", "Stortare", "Grisetang",

    # Worms
    "Fjæremark", "Børsteormer", "Kongsnegl",

    # Insects
    "Fjærmygg", "Fjærmygg art",

    # Benthic organisms
    "Bunndyr", "Bunndyr",

    # Birds
    "Gråmåke", "Sangsvane"
  )
)

# Export the lookup table to a CSV file
write_csv(copper_species_lookup, "R/data/copper_species_lookup.csv")

# Usage example:
# copper_clean %>%
#   filter(!is.na(SAMPLE_SPECIES)) %>%
#   left_join(copper_species_lookup, by = "SAMPLE_SPECIES") %>%
#   mutate(
#     display_name = ifelse(!is.na(Name_EN),
#                          paste0(SAMPLE_SPECIES, " (", Name_EN, ")"),
#                          SAMPLE_SPECIES)
#   )
