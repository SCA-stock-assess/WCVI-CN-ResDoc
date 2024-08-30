# Packages and functions --------------------------------------------------

pkgs <- c("tidyverse","scales","ggmap","here", "ggOceanMaps", "ggspatial")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 12))
library(scales)
library(ggmap)
library(ggOceanMaps)
library(ggspatial)

# Midpoint from cut functions
get_midpoint <- function(cut_label) {
  mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
}

# Function to detect whole numbers (for lat/long jittering)
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# Increase timeout limit to load data from the web
options(timeout = 240)

# Load and merge RMIS data ------------------------------------------------


# The main data dump file
rmis.raw <- read.table(
  here(
    "Marine CWT recoveries map",
    "PRC2638.txt"
    ), 
  header = TRUE, 
  sep = ",", 
  na.strings = c("","NA")
)


# Lookup table to merge lat and long with recoveries
loc.lu <- read.csv(
  "https://www.rmpc.org/pub/data-archive/AnnualBackup-20240201/LC042_ALL_FULLSET.csv", 
  na.strings = c("","NA")
  ) |> 
  group_by(location_code, latitude, longitude) |> 
  arrange(desc(location_code), desc(latitude), .by_group = TRUE) |> 
  ungroup() |> 
  distinct(location_code, .keep_all = TRUE)


# Release information (to get ages & stocks)
rel.lu <- read.csv(
  "https://www.rmpc.org/pub/data-archive/AnnualBackup-20240201/RL042_CDFO_FULLSET.csv",
  na.strings = c("","NA")
) |> 
  left_join(
    select(loc.lu, location_code, name),
    by = c("hatchery_location_code" = "location_code"),
    relationship = "many-to-one"
  ) |> 
  select(tag_code_or_release_id, brood_year, name) |> 
  rename(hatchery_location_name = name)
  

# Merge
rmis <- rmis.raw %>% 
  left_join(
    loc.lu, 
    by = c("recovery_location_code"= "location_code"),
    relationship = "many-to-one",
    suffix = c("", ".y")
  ) %>% 
  left_join(read.csv(here("Marine CWT recoveries map", "RMIS_fishery_names.csv"))) %>% 
  left_join(
    rel.lu,
    by = c("tag_code" = "tag_code_or_release_id"),
    relationship = "many-to-one"
  ) |> 
  select(-ends_with(".y")) |> 
  mutate(
    recovery_year = str_sub(recovery_date, 1L, 4L) |> as.integer(),
    recovery_date = case_when(
      str_length(recovery_date) == 4 ~ NA_Date_,
      str_length(recovery_date) == 8 ~ paste(
        str_sub(recovery_date, 1L, 4L),
        str_sub(recovery_date, 5L, 6L),
        str_sub(recovery_date, 7L, 8L),
        sep = "-"
      ) |> 
        as.Date(format = "%Y-%m-%d"),
      TRUE ~ NA_Date_
    ),
    recovery_month = recovery_date |> format("%m") |> as.numeric(),
    Age.at.recovery = recovery_year - brood_year
  ) |> 
  drop_na(latitude, longitude) %>% # Remove all rows that are missing location data
  filter(Age.at.recovery %in% c(1:6)) %>% # Keep age 1-6 Chinook only
  mutate(
    age = as.character(Age.at.recovery),
    age_txt = paste("Age", age, sep = " "),
    y_m = paste(recovery_year, recovery_month, sep = "-") %>% as.factor(),
    month = factor(month.name[recovery_month], levels = month.name[1:12]),
    season = case_when(
      month %in% month.name[3:5] ~ "Spring",
      month %in% month.name[6:8] ~ "Summer",
      month %in% month.name[9:11] ~ "Autumn",
      TRUE ~ "Winter") %>% 
      factor(
        levels=c("Winter","Spring","Summer","Autumn"),
        labels = c("Winter (Dec-Feb)", "Spring (Mar-May)","Summer (Jun-Aug)","Autumn (Sep-Nov)")
      ),
    age_month = paste(age, month, sep = "_") %>% 
      factor(levels = paste(
        rep(seq(1,6, by = 1), each =12), 
        month.name[1:12], sep = "_"
      )
      ),
    regime = case_when(
      recovery_year < 1979 ~ "Pre-'79",
      recovery_year %in% 1979:1990 ~ "'79-'90",
      recovery_year %in% 1991:2000 ~ "'91-'00",
      recovery_year >= 2001 ~ "'01-present"
    ) %>% 
      factor(levels = c("Pre-'79","'79-'90","'91-'00","'01-present")),
    maturity = if_else(Age.at.recovery < 3, "subadult", "adult") %>% 
      factor(levels = c("subadult","adult")),
    RCH_orig = if_else(
      str_detect(hatchery_location_name, "Alberni|Robertson|Omega"),
      "RCH",
      "non-RCH WCVI"
    ),
    # Add random noise to the Sitka Sound recoveries
    # across(
    #   latitude:longitude, 
    #   ~ if_else(
    #     str_detect(name, "(?i)sitka"),
    #     jitter(.x, amount = 0.006),
    #     .x
    #   )
    # ),
    binlon = cut(
      longitude, 
      seq(from = min(longitude), to = max(longitude), by = .005), 
      include.lowest = T, 
      right = F
    ),
    binlat = cut(
      latitude, 
      seq(from = min(latitude), to = max(latitude), by = .005), 
      include.lowest = T, 
      right = F
    ),
    # Add random noise to the high seas recoveries
    across(
      latitude:longitude, 
      ~ if_else(
        is.wholenumber(.x),
        jitter(.x, amount = 0.25),
        .x
      ),
      .names = "{.col}_jitter"
    ) 
  ) %>% 
  droplevels()


# Display hatchery releases included in the time series
rmis %>% count(hatchery_location_name)




# Using ggOceanMaps package ---------------------------------------------------


# Group RMIS data to make bubble plot
rmis_stacked <- count(rmis, binlat, binlon) |> 
  rowwise() |> 
  mutate(
    mid_binlat = get_midpoint(binlat),
    mid_binlon = get_midpoint(binlon),
  ) |> 
  ungroup() |> 
  arrange(n)


# Breaks for the custom scale
my_breaks <- c(3, 30, 300, 3000)


# Custom guide style
g <- guide_legend(
  title = "Count of recoveries",
  direction = "horizontal",
  title.position = "top",
  label.position = "bottom",
  override.aes = list(fill = NA),
  nrow = 1
)


# Base map with high-res bathymetry
(NE_Pacific <- basemap(
  limits = c(
    -168,
    -120,
    47,
    61
  ),
  rotate = TRUE,
  bathymetry = TRUE,
  bathy.style = "rcb",
  grid.col = "grey85",
  grid.size = 0.1,
  land.col = "grey75",
  land.border.col = NA,
  lon.interval = 20,
  lat.interval = 5
) 
)


# Data plotted on basemap
(recovery_distribution <- NE_Pacific +
  ggspatial::geom_spatial_point(
    data = rmis_stacked,
    aes(
      x = mid_binlon,
      y = mid_binlat,
      size = n,
      colour = n,
      alpha = n
    ),
    shape = 20,
    stroke = FALSE  
  ) +
  annotation_north_arrow(
    location = "tr",
    style = north_arrow_fancy_orienteering(),
    which_north = "true"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  guides(
    colour = g,
    size = g,
    alpha = g
  ) +
  scale_size_continuous(
    breaks = my_breaks,
    range = c(1,6),
    trans = "log10"
  ) +
  scale_alpha_continuous(
    breaks = my_breaks,
    range = c(0.3, 0.7),
    trans = "log10"
  ) +
  scale_color_viridis_c(
    breaks = my_breaks,
    option = "inferno",
    trans = "log10"
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.07,0.07),
    legend.justification = c(0,0),
    legend.box = "horizontal",
    legend.key.height = unit(0.43, "lines"),
    legend.background = element_rect(colour = "black"),
    legend.margin = margin(rep(0.4, 4), unit = "lines")
  ) 
)


# Save the map
ggsave(
  plot = recovery_distribution,
  filename = here(
    "Marine CWT recoveries map",
    "R-PLOT_RMIS_WCVI_CN_CWT_recoveries.png"
  ),
  height = 4,
  width = 8,
  units = "in"
)


