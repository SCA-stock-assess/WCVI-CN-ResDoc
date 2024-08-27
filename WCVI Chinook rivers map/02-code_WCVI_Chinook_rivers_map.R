# Packages ----------------------------------------------------------------


pkgs <- c("here", "tidyverse", "readxl", "janitor", "sf", "ggspatial")
#install.packages(pkgs)


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(janitor)
library(sf) # Large library providing many tools for working with spatial data 
library(ggspatial) # Add scalebar and North arrow to plots




# Load spatial data for streams and hatchery locations ----------------------------------------


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



# Create basemap and start plotting ---------------------------------------


# Define bounding box for Vancouver Island
vi_coords <- c(xmin = -129.5, xmax = -123, ymin = 48, ymax = 51)
vi_bbox <- st_bbox(vi_coords, crs = "NAD83")


# Disable spherical geometry
sf_use_s2(FALSE)


# High-res coastline for Vancouver Island
vi_coastline <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "FWA_COASTLINES_SP",
    "FWCSTLNSSP_line.shp"
  )
) |> 
  st_transform(crs = "NAD83") |> 
  st_crop(vi_bbox) |> 
  # Constrain data to include only Van Isle watersheds
  filter(
    WTRSHDGRPC %in% c(
      "NEVI", "NIMP", "COMX", "TAHS", "COWN", "VICT", 
      "SANJ", "PARK", "ALBN", "CLAY", "GOLD", "BRKS",
      "TSIT", "HOLB", "CAMB", "SALM"
    )
  ) |> 
  st_union() |> 
  st_polygonize()


# Coastline polygons for BC and WA
full_coastline <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "GSHHS_shp",
    "h",
    "GSHHS_h_L1.shp"
  )
) |> 
  st_transform(crs = "NAD83") |> 
  st_crop(vi_bbox) 


# Find where the two maps cross over and remove the overlapping area
intersection <- st_intersection(vi_coastline, full_coastline)
difference <- st_difference(full_coastline, st_union(st_geometry(intersection)))


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


# Enable spherical geometry
sf_use_s2(TRUE)


# Plot
(cn_rivers_map <- ggplot(difference) +
    geom_sf(
      colour = NA,
      fill = "grey70"
    ) +
    # Super hacky fix to ensure Nootka Sd is fully shown
    geom_rect(
      aes(
        xmin = -129,
        xmax = -125,
        ymin = 48,
        ymax = 50.25
      ),
      fill = "lightblue1"
    ) +
    geom_sf(
      data = vi_coastline, 
      colour = NA,
      fill = "grey70"
    ) +
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
    annotation_scale(location = "bl") +
    annotation_north_arrow(
      location = "tr",
      style = north_arrow_fancy_orienteering()
    ) +
    scale_y_continuous(breaks = c(49, 50)) +
    scale_x_continuous(breaks = c(-125, -127)) +
    scale_size_discrete(range = c(3,6)) +
    scale_colour_brewer(palette = "Dark2") +
    coord_sf(expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme(
      panel.background = element_rect(fill = "lightblue1"),
      panel.grid = element_blank(),
      legend.position = c(0.05, 0.1),
      legend.justification = c(0, 0),
      legend.background = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
)


# Export the plot
ggsave(
  cn_rivers_map,
  filename = here(
    "WCVI Chinook rivers map",
    "R-PLOT_WCVI_Chinook_rivers.png"
    ),
  height = 6,
  width = 8,
  unit = "in"
)


