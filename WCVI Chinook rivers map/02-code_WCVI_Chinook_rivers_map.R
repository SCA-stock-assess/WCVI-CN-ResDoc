# Packages ----------------------------------------------------------------


pkgs <- c("here", "tidyverse", "readxl", "janitor", "sf", "stars", "ggspatial")
#install.packages(pkgs)


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(janitor)
library(sf) # Large library providing many tools for working with spatial data 
library(stars)
library(ggspatial) # Add scalebar and North arrow to plots




# Load and plot spatial data ----------------------------------------


# List of WCVI Chinook Rivers from Diana's FWA table
cn_stream_names <- read_xlsx(
  here("Kobe plot", "01-data_FWA table update version 4 April17.xlsx"),
  sheet = "RiverEscData",
  skip = 2
) |> 
  clean_names() |> 
  filter(!is.na(stream_name)) |> 
  pull(stream_name) |> 
  str_to_title() |> 
  str_remove_all("(?i)river|creek|creeks|\\(.+\\)|aggregate|tribs|lake|system") |> 
  str_split_fixed("/", 2) |> 
  str_trim() |> 
  str_subset("\\w+")


# Vancouver island stream arcs from BC Freshwater Atlas
stream_lines <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "FWA_STREAM_NETWORKS_SP",
    "FWSTRMNTWR_line.shp"
  )
) |> 
  rename_with(str_to_lower) |> 
  mutate(
    gnis_name = str_remove_all(gnis_name, "(?i)creek|river") |> 
      str_trim()
  ) |> 
  filter(gnis_name %in% c(cn_stream_names, "Stamp"))



# Define bounding box for Vancouver Island
vi_coords <- c(xmin = -129, xmax = -122.5, ymin = 48, ymax = 51)
vi_bbox <- st_bbox(vi_coords, crs = "NAD83")


# High-res coastline for Vancouver Island
vi_coastline <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "Coastline",
    "Coastline.shp"
  )
) |> 
  st_transform(crs = "NAD83")


# Shapefile with lake polygons
lakes <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "FWA_LAKES_POLY",
    "FWLKSPL_polygon.shp"
  )
) |> 
  filter(AREA_SQM > 5e6) |> 
  st_transform(crs = "NAD83")


# Shapefile containing DFO hatchery locations
hatcheries <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "Hatchery_locations_shp",
    "DFO_Hatchery_locations.shp"
  )
) |> 
  st_set_crs(3857) |> 
  st_transform(crs = "NAD83") |> 
  st_crop(vi_bbox) |> 
  # Filter to include only WCVI facilities
  filter(
    Fac_Name %in% c(
      "Sugsaw Creek Hatchery", "Nitinat River Hatchery", "San Juan River Hatchery",
      "Sooke River Hatchery", "Robertson Creek Hatchery", "Thornton Creek Hatchery",
      "Kennedy River Hatchery", "Tofino Hatchery", "Conuma River Hatchery",
      "Tahsis River Hatchery", "Zeballos River Hatchery", "Marble River Hatchery",
      "Cordy Creek Hatchery"
    )
  ) |> 
  mutate(
    `Hatchery program` = factor(
      Program_De,
      levels = c(
        "DFO Major Operations",
        "Community Economic Development Program",
        "Aboriginal Fisheries Strategy",
        "Public Involvement Program"
      ) 
    ) |> 
      fct_rev()
  )


# Switch off spherical geometry for cropping
sf_use_s2(FALSE)


# Use lakes to crop land 
land <- vi_coastline %>%
  st_difference(., st_union(st_geometry(st_intersection(., lakes)))) 


# Bathymetry data
bathy <- read_stars(
  here(
    "WCVI Chinook rivers map",
    "WCVI_bathy_NOAA.tiff"
  )
) |> 
  st_transform(crs = "NAD83") |> 
  setNames("depth") |> 
  mutate(depth = if_else(depth > 0, 0, depth))


# Switch spherical geometry back on
sf_use_s2(TRUE)


# Guide that disables background filling in key glyph
g <- guide_legend(override.aes = list(fill = NA))


# Create basemap without bathymetry
(basemap1 <- ggplot(land) +
  geom_sf(
    colour = NA,
    fill = "grey65"
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    expand = FALSE,
    xlim = c(-129, -122.5), 
    ylim = c(48, 51)
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.background = element_rect(fill = "lightblue1"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.05, 0.1),
    legend.justification = c(0, 0)
  )
)


# Create basemap with bathymetry
(basemap2 <- ggplot(land) +
    geom_stars(data = bathy) +
    geom_sf(
      colour = NA,
      fill = "grey65"
    ) +
    annotation_scale(location = "bl") +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering()
    ) +
    coord_sf(
      expand = FALSE,
      xlim = c(-129, -122.5), 
      ylim = c(48, 51)
    ) +
    labs(x = NULL, y = NULL) +
    guides(fill = "none") +
    scale_fill_distiller(palette = "Blues") +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = c(0.05, 0.1),
      legend.justification = c(0, 0)
    )
)


# Plot
(cn_rivers_map <- basemap1 +
    geom_sf(
      data = stream_lines,
      colour = "#08519C"
    ) +
    geom_sf(
      data = hatcheries,
      aes(colour = `Hatchery program`, size = `Hatchery program`)
    ) +
    geom_sf_label(
      data = filter(hatcheries, FacType2 == "OPS Hatchery"),
      aes(label = Fac_Name),
      hjust = 0,
      vjust = 0,
      alpha = 0.8
    ) +
    scale_size_discrete(range = c(3,6)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_distiller(palette = "Blues") +
    guides(
      fill = "none",
      colour = g,
      size = g
    ) +
    coord_sf(
      expand = FALSE,
      xlim = c(-129, -122.5), 
      ylim = c(48, 51)
    ) +
    theme(legend.background = element_blank())
)


# Alternate version with bathymetry added
(cn_rivers_map_alt <- basemap2 +
    geom_sf(
      data = stream_lines,
      colour = "#08519C"
    ) +
    geom_sf(
      data = hatcheries,
      aes(colour = `Hatchery program`, size = `Hatchery program`)
    ) +
    geom_sf_label(
      data = filter(hatcheries, FacType2 == "OPS Hatchery"),
      aes(label = Fac_Name),
      hjust = 0,
      vjust = 0,
      alpha = 0.8
    ) +
    scale_size_discrete(range = c(3,6)) +
    scale_colour_brewer(palette = "Dark2") +
    scale_fill_distiller(palette = "Blues") +
    guides(
      fill = "none",
      colour = g,
      size = g
    ) +
    coord_sf(
      expand = FALSE,
      xlim = c(-129, -122.5), 
      ylim = c(48, 51)
    ) +
    theme(legend.background = element_rect(colour = "black", fill = "white"))
)
  

# Export the plots
list(
  "WCVI_CN_rivers" = cn_rivers_map,
  "WCVI_CN_rivers_with_bathy" = cn_rivers_map_alt
) |> 
  imap(
    ~ ggsave(
      plot = .x,
      filename = here(
        "WCVI Chinook rivers map",
        paste0("R-PLOT_", .y, ".png")
      ),
      height = 6,
      width = 8,
      unit = "in"
    )
  )


