# Packages ----------------------------------------------------------------


pkgs <- c("here", "tidyverse", "readxl", "janitor", "sf", "stars", "ggspatial")
#install.packages(pkgs)


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 12))
library(readxl)
library(janitor)
library(sf) # Large library providing many tools for working with spatial data 
library(stars)
library(ggspatial) # Add scalebar and North arrow to plots




# Load and plot spatial data ----------------------------------------


# List of WCVI Chinook Rivers from Diana's FWA table
cn_streams_list <- read_xlsx(
  here(
    "1. data files", 
    "WCVI_CN_streams_master_list.xlsx"
  )
) |> 
  clean_names() |> 
  separate(
    major_tributaries,
    sep = ", ",
    into = c("trib1", "trib2", "trib3", "trib4")
  ) |> 
  pivot_longer(
    cols = c(mainstem, contains("trib")),
    values_to = "stream",
    values_drop_na = TRUE
  ) |> 
  select(cu, pfma, indicator_status, stream)


# Extract column of stream names as a character vector
cn_stream_names <- pull(.data = cn_streams_list, stream)


# Vancouver island stream arcs from BC Freshwater Atlas
stream_lines <- read_sf(
  here(
    "1. data files",
    "FWA_STREAM_NETWORKS_SP",
    "FWSTRMNTWR_line.shp"
  )
) |> 
  rename_with(str_to_lower) |> 
  # Remove some creeks that do not belong but match names in the list
  filter(!(gnis_name == "Harris Creek" & bllnk == 354138111)) |> # Remove the second (wrong) Harris Ck
  mutate(
    # Some enumerated creeks are unnamed in the FWA. Add StAD names to FWA line keys. 
    gnis_new_name = case_when(
      #bllnk == 354154385 ~ "Harris Creek", # FWA has two Harris Creeks on WCVI, this is the correct one
      bllnk == 354140796 ~ "Warn Bay Creek", 
      bllnk == 354141433 ~ "Chum Creek",
      bllnk == 354152375 ~ "Deserted Creek",
      bllnk == 354151632 ~ "Hammond Creek",
      bllnk == 354149782 ~ "Marvinas Bay Creek",
      bllnk == 354151033 ~ "Easy Creek",
      bllnk == 354153393 ~ "McKay Cove Creek",
      bllnk == 354153029 ~ "Nasparti River",
      bllnk == 354154631 ~ "Hoiss Creek",
      TRUE ~ gnis_name
    ) 
  ) |> 
  filter(gnis_new_name %in% cn_stream_names) |> 
  relocate(gnis_name, .before = gnis_new_name) |> 
  left_join(
    select(cn_streams_list, stream, indicator_status),
    by = c("gnis_new_name" = "stream")
  ) |> 
  mutate(
    indicator_status = 
      factor(
        indicator_status, 
        levels = c("PST indicator", "Intensive/Major Ops.", "Intensive", "Extensive", "Non-indicator")
      )
  )


# Check if all rivers in the streams list could be found in the FWA
stopifnot(length(cn_stream_names) == length(unique(stream_lines$gnis_new_name)))


# See what's missing (toggle to TRUE to run)
if(FALSE) {
cn_stream_names |> 
  as_tibble_col(column_name = "stream") |> 
  left_join(
    distinct(stream_lines, gnis_new_name),
    by = c("stream" = "gnis_new_name"),
    keep = TRUE
  ) |> 
  print(n = length(cn_stream_names))
}


# Define bounding box for Vancouver Island
vi_coords <- c(xmin = -129, xmax = -123, ymin = 48, ymax = 51)
vi_bbox <- st_bbox(vi_coords, crs = "NAD83")


# High-res coastline for Vancouver Island
vi_coastline <- read_sf(
  here(
    "1. data files",
    "BC Coastline",
    "Coastline.shp"
  )
) |> 
  st_transform(crs = "NAD83")


# Shapefile with lake polygons
lakes <- read_sf(
  here(
    "1. data files",
    "FWA_LAKES_POLY",
    "FWLKSPL_polygon.shp"
  )
) |> 
  filter(AREA_SQM > 5e6) |> 
  st_transform(crs = "NAD83")


# Shapefile containing DFO hatchery locations
hatcheries <- read_sf(
  here(
    "1. data files",
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
    "1. data files",
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
    fill = "grey70"
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    expand = FALSE,
    xlim = c(-129, -123), 
    ylim = c(48, 51)
  ) +
  labs(x = NULL, y = NULL) +
  theme(
    panel.background = element_rect(fill = "lightblue1"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.07),
    legend.justification = c(0, 0)
  )
)


# Create basemap with bathymetry
basemap2 <- ggplot(land) +
  geom_stars(data = bathy) +
  geom_sf(
    colour = NA,
    fill = "grey70"
  ) +
  annotation_scale(location = "bl") +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering()
  ) +
  coord_sf(
    expand = FALSE,
    xlim = c(-129, -123), 
    ylim = c(48, 51)
  ) +
  labs(x = NULL, y = NULL) +
  guides(fill = "none") +
  scale_fill_distiller(palette = "Blues") +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.07),
    legend.justification = c(0, 0)
  )



# Plot
(cn_rivers_map <- basemap1 +
    geom_sf(
      data = stream_lines,
      aes(colour = indicator_status),
      lineend = "round",
      linewidth = 0.5
    ) +
    # Overlay lakes on top of stream lines
    geom_sf(
      data = lakes,
      fill = "lightblue1",
      colour = NA
    ) +
    geom_sf(
      data = hatcheries,
      aes(shape = `Hatchery program`, size = `Hatchery program`)
    ) +
    geom_sf_label(
      data = filter(hatcheries, FacType2 == "OPS Hatchery"),
      aes(label = Fac_Name),
      hjust = 0,
      vjust = 0,
      alpha = 0.8
    ) +
    scale_size_discrete(range = c(2,5)) +
    scale_shape_manual(values = c(19, 18, 17, 15)) +
    #scale_colour_viridis_d(option = "B", end = 0.9) +
    scale_colour_brewer(palette = "Set1") +
    guides(
      colour = guide_legend(
        override.aes = list(fill = NA, linewidth = 1), 
        order = 1
      ),
      size = guide_legend(
        override.aes = list(fill = NA), 
        order = 2
      ),
      shape = guide_legend(
        override.aes = list(fill = NA), 
        order = 2
      )
    ) +
    coord_sf(
      expand = FALSE,
      xlim = c(-129, -123), 
      ylim = c(48, 51)
    ) +
    labs(colour = "Indicator status") +
    theme(legend.background = element_blank())
)


# Alternate version with bathymetry added
cn_rivers_map_alt <- basemap2 +
    geom_sf(
      data = stream_lines,
      aes(colour = indicator_status),
      lineend = "round",
      linewidth = 0.5
    ) +
    # Overlay lakes on top of stream lines
    geom_sf(
      data = lakes,
      fill = RColorBrewer::brewer.pal(4, "Blues")[1],
      colour = NA
    ) +
    geom_sf(
      data = hatcheries,
      aes(shape = `Hatchery program`, size = `Hatchery program`)
    ) +
    geom_sf_label(
      data = filter(hatcheries, FacType2 == "OPS Hatchery"),
      aes(label = Fac_Name),
      hjust = 0,
      vjust = 0,
      alpha = 0.8
    ) +
    scale_size_discrete(range = c(2,5)) +
    scale_shape_manual(values = c(19, 18, 17, 15)) +
    #scale_colour_viridis_d(option = "B", end = 0.85) +
    scale_colour_brewer(palette = "Set1") +
    scale_fill_distiller(palette = "Blues") +
  guides(
    fill = "none",
    colour = guide_legend(
      override.aes = list(fill = NA, linewidth = 1), 
      order = 1
    ),
    size = guide_legend(
      override.aes = list(fill = NA), 
      order = 2
    ),
    shape = guide_legend(
      override.aes = list(fill = NA), 
      order = 2
    )
  ) +
  coord_sf(
      expand = FALSE,
      xlim = c(-129, -123), 
      ylim = c(48, 51)
    ) +
    labs(colour = "Indicator status") +
    theme(legend.background = element_rect(colour = "black", fill = "white"))

  

# Export the plots
list(
  "WCVI_CN_rivers" = cn_rivers_map,
  "WCVI_CN_rivers_with_bathy" = cn_rivers_map_alt
) |> 
  imap(
    ~ ggsave(
      plot = .x,
      filename = here(
        "3. R outputs",
        "WCVI Chinook rivers map",
        paste0("R-PLOT_", .y, ".png")
      ),
      height = 6,
      width = 8,
      unit = "in"
    )
  )


