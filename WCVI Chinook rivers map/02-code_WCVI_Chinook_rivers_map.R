# Packages ----------------------------------------------------------------


pkgs <- c("here", "tidyverse", "readxl", "janitor", "sf", "ggspatial")
#install.packages


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))
library(readxl)
library(janitor)
library(sf)
library(ggspatial)




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
vi_coords <- c(xmin = -129, xmax = -122, ymin = 48, ymax = 51)
vi_bbox <- st_bbox(vi_coords, crs = "WGS84")


# Disable spherical geometry
sf_use_s2(FALSE)


# Shapefile for the high-res coastline
vi_coastline <- read_sf(
  here(
    "WCVI Chinook rivers map",
    "GSHHS_shp",
    "h",
    "GSHHS_h_L1.shp"
  )
) |> 
  st_crop(vi_coords)


# Enable spherical geometry
sf_use_s2(TRUE)


# Plot
ggplot(vi_coastline) +
  geom_sf(
    data = st_as_sfc(vi_bbox),
    fill = "lightblue1"
  ) +
  geom_sf(
    colour = "grey50",
    fill = "grey60"
    ) +
  geom_sf(
    data = stream_lines,
    colour = "white"
  ) +
  annotation_scale(location = "br") +
  coord_sf(expand = FALSE) +
  scale_y_continuous(breaks = c(49, 50)) +
  scale_x_continuous(breaks = c(-124, -126, -128)) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_line(colour = "grey70", linewidth = 0.1),
    panel.ontop = TRUE
  )
