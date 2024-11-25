# Packages ----------------------------------------------------------------

pkgs <- c(
  'tidyverse','readxl', 'writexl','janitor', 'here'
)
#install.packages(pkgs)

library(tidyverse); theme_set(theme_light(base_size = 18))
library(magrittr)
library(janitor)
library(readxl)
library(here)


# Load and infill the escapement data time series ------------------------------------


# List of indicator rivers
indicator_names <- read_xlsx(
  here(
    "1. data files", 
    "FWA table update version 4 May30.xlsx"
  ),
  sheet = "RiverEscData",
  skip = 2
) |> 
  clean_names() |> 
  select(indicator, stream_name) |> 
  filter(indicator %in% c("Ind7", "Ind10")) |> 
  pull(stream_name) |> 
  make_clean_names()


# Escapement time series
indicator_esc <- read_xlsx(
  here(
    "1. data files", 
    "FWA table update version 4 May30.xlsx"
  ),
  sheet = "RiverEscData (2)",
  skip = 2,
  na = "-"
) |> 
  clean_names() |> 
  select(-x20) |> # Weird empty column at the end
  rename(
    "river" = stream_name,
    "escapement" = spawners,
    "year" = brood_year,
    "sep_releases" = sep_releases_ff_smolts_seapen
  ) |> 
  mutate(river = make_clean_names(river, allow_dupes = TRUE))


# Filter data to only the 1979+ time series of indicator streams
trimmed_esc <- indicator_esc |> 
  filter(
    year >= 1979, # start the time series in 1979
    river %in% indicator_names
  ) |> 
  arrange(river, year)


# which streams and years need infilling?
to_infill <- trimmed_esc |> 
  filter(is.na(escapement)) |> 
  select(river, year) |> 
  mutate(infill = "yes")


# List return years that received some enhancement during brood years
enhancement_history <- trimmed_esc |> 
  filter(sep_releases > 500) |> # Some years received very little enhancement
  # Assume that enhanced years will produce enhanced returns 3, 4, and 5 years later
  mutate(
    brood_year = year,
    return_year1 = year + 3,
    return_year2 = year + 4,
    return_year3 = year + 5
  ) |> 
  # Reshape the data so all return years/populations have their own row
  pivot_longer(
    cols = contains("return_year"),
    names_prefix = "return_year",
    values_to = "return_year",
    names_to = "name"
  ) |> 
  select(return_year, river) |> 
  distinct() |> 
  mutate(enhanced = "yes")


# Are any years available for pAVG calculations?
trimmed_esc |> 
  filter(!is.na(escapement)) |> 
  left_join(
    enhancement_history,
    by = join_by(year == return_year, river == river)
  ) |> 
  filter(is.na(enhanced)) |> 
  pivot_wider(
    id_cols = year,
    names_from = river,
    values_from = escapement
  ) |> 
  filter(!if_any(!year, is.na))
# No years where all streams were surveyed and none were receiving enhanced returns
# Proceed with pMAX method


# pMAX data by enhanced versus not
pmax_data <- trimmed_esc |> 
  filter(!is.na(escapement)) |> 
  left_join(
    enhancement_history,
    by = join_by(river == river, year == return_year)
  ) |> 
  mutate(
    .by = c(river, enhanced),
    max = max(escapement, na.rm = TRUE),
    pmax = escapement/max
  ) |> 
  select(river, year, enhanced, max, pmax)


# List of rivers/years where pmax data are available
pmax_eligible <- pmax_data |> 
  mutate(data = if_else(is.na(enhanced), "yes", "yes - enhanced")) |> 
  select(year, river, data)


# Need to assign neighbouring streams for pMAX infilling
pmax_neighbours <- to_infill |> 
  distinct(river) |> 
  # Manually assign neighbouring streams for pMAX 
  # Note that neighbours are listed in order of proximity/preference (important)
  mutate(
    neighbours = case_when(
      river == "artlish_river" ~ list(c("kaouk_river", "tahsish_river")),
      river == "bedwell_ursus" ~ list(c("tranquil_creek", "moyeha_river", "megin_river")),           
      river == "burman_river" ~ list(c("gold_river_aggregate")),            
      river == "colonial_cayeghle_creeks" ~ list(c("marble_river", "artlish_river", "tahsish_river")),
      river == "gold_river_aggregate" ~ list(c("burman_river")),    
      river == "kaouk_river" ~ list(c("artlish_river", "tahsish_river")),             
      river == "leiner_river" ~ list(c("tahsis_river", "zeballos_river")),            
      river == "megin_river" ~ list(c("moyeha_river", "bedwell_ursus", "tranquil_creek")),             
      river == "moyeha_river" ~ list(c("megin_river", "bedwell_ursus", "tranquil_creek")),            
      river == "nahmint_river" ~ list(c("sarita_river", "tranquil_creek")),           
      river == "sarita_river" ~ list(c("nahmint_river", "tranquil_creek")),            
      river == "tahsis_river" ~ list(c("leiner_river", "zeballos_river")),            
      river == "tranquil_creek" ~ list(c("bedwell_ursus", "nahmint_river", "sarita_river")),          
      river == "zeballos_river" ~ list(c("leiner_river", "tahsis_river"))
    )
  ) |> 
  # Split the list of neighbour populations into their own columns
  separate(
    neighbours,
    into = c("neighbour1", "neighbour2", "neighbour3"),
    sep = ","
  ) |> 
  mutate(
    across(
      contains("neighbour"),
      ~str_remove_all(.x, "c\\(|\"|\\)") |> # Remove leftover symbols from character vectors
        str_trim()
    )
  )


# Put pieces together and select years/rivers for pMAX infilling
pmax_infill_logic <- to_infill |> 
  left_join(pmax_neighbours) |> 
  left_join(
    enhancement_history,
    by = join_by(river == river, year == return_year)
  ) |> 
  # Add columns showing what data are available for each neighbouring population
  left_join(
    rename(pmax_eligible, "data1" = data),
    by = join_by(neighbour1 == river, year == year)
  ) |> 
  left_join(
    rename(pmax_eligible, "data2" = data),
    by = join_by(neighbour2 == river, year == year)
  ) |> 
  left_join(
    rename(pmax_eligible, "data3" = data),
    by = join_by(neighbour3 == river, year == year)
  ) |> 
  arrange(river, year) %T>%
  # Some rivers/years still have no good neighbour:
  {print(filter(., if_all(contains("data"), is.na)))} |> # Megin, Moyeha, and Bedwell in 1984
  mutate(
    # We'll use the Sarita for these populations/years
    neighbour4 = if_else(
      str_detect(river, "bedwell|megin|moyeha") & year == 1984,
      "sarita_river",
      NA_character_
    )
  ) |> 
  left_join(
    rename(pmax_eligible, "data4" = data),
    by = join_by(neighbour4 == river, year == year)
  ) |> 
  # Lengthen data so each neighbour has its own row
  pivot_longer(
    matches("neighbour|data"),
    names_pattern = "(.+)(\\d)",
    names_to = c(".value", "set"),
    values_drop_na = TRUE
  ) |> 
  filter(!is.na(data)) |> 
  # Stipulate rules for which data to use for pMAX infilling
  mutate(
    .by = c(river, year),
    options = n(),
    choice = case_when(
      # If there is only one option, use that
      options == 1 ~ "keep", 
      # If not enhanced and neighbour not enhanced, use those data
      options > 1 & is.na(enhanced) & data == "yes" ~ "keep", 
      # Use enhanced returns to infill enhanced returns
      options > 1 & enhanced == "yes" & data == "yes - enhanced" ~ "keep",
      # If there are only enhanced returns to infill unenhanced returns, we will use those
      options > 1 & is.na(enhanced) & all(data == "yes - enhanced") ~ "keep",
      TRUE ~ "drop" # Drop everything else
    )
  )


# Infill escapement data using neighbouring pMAX values
infilled_esc <- pmax_infill_logic |> 
  # Drop extra options for infill data by prioritizing lowest sets
  # i.e. first neighbour listed with good data is preferred
  arrange(river, year, set) |> 
  distinct(river, year, .keep_all = TRUE) |> 
  select(river, year, infill, enhanced, neighbour) |> 
  # Add the pMAX value for selected neighbouring populations
  left_join(
    select(pmax_data, river, year, pmax),
    by = join_by(neighbour == river, year == year)
  ) |> 
  # Add each system's own max value
  left_join(distinct(pmax_data, river, enhanced, max)) |> 
  # Calculate infilled escapement value
  mutate(escapement = max * pmax) |> 
  select(river, year, infill, neighbour, escapement, pmax) |> 
  # Collate with the not-infilled raw data
  bind_rows(
    trimmed_esc |> 
      filter(!is.na(escapement)) |> 
      select(river, year, escapement)
  )


# Save data file with infilled escapement data
write.csv(
  infilled_esc,
  file = here(
    "3. R outputs",
    "Escapement infill", 
    "R-OUT_infilled_indicators_escapement_timeseries.csv"
  ),
  row.names = FALSE
)

