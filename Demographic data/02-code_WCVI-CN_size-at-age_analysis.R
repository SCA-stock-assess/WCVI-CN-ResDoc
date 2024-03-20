# Packages & functions -----------------------------

pkgs <- c(
  "tidyverse", "magrittr", "ggridges", "here", "read_xl", "fuzzyjoin"
)
#install.packages(pkgs)

library(readxl)
library(tidyverse); theme_set(theme_bw(base_size = 18))
library(janitor)
library(fuzzyjoin)
library(magrittr)
library(ggridges)
library(here)


# Save list of all possible NA strings
na_strings <- c("", " ", "N/A", "#N/A", "NA", "N/S", "na")


# Function to convert FL to POH length and vice versa
# Based on 20 years of biodata collected from WCVI Chinook in ENPRO
convert_fl_poh <- function(measurement, input = c("lf", "poh")) {
  if(!input %in% c("fl", "poh")) {
    stop("Input must be 'fl' or 'poh'") # Throw error if input is incorrectly specified
  } 
  
  if(input == "fl") {
    # These values from a linear regression of NF on POH; R squared of 0.97
    result <- (measurement/1.2228) - 8.3166 
  } else {
    result <- (measurement*1.2228) + 8.3166
  }
  # On average, the conversion will err by ~3%
  
  return(round(result, 0)) # Round to nearest mm
}


# Load and concatenate all CREST biodata ----------------------------------


# Save a list of the data file names
crest_files <- list.files(
  here("Demographic data"),
  pattern = "(?i)crest",
  full.names = TRUE
) |> 
  as_tibble_col(column_name = "path")


# Extract the data from the WCVI Run Reconstruction tabs
crest_data <- crest_files |> 
  mutate(
    data = map(
      .x = path,
      ~ read_xlsx(
        path = .x, 
        sheet = "WCVI_Chinook_Run_Rec",
        na = na_strings,
        col_types = "text" # prevents a deluge of unnecissary coltype warnings
        ),
      .progress = "Data read progress:"
    )
  ) |> 
  pull(data) |> 
  list_rbind() |> 
  clean_names() |>
  mutate(
    across(everything(), parse_guess),
    resolved_stock_origin = case_when(
      is.na(resolved_stock_origin) & !is.na(oto_stock) ~ oto_stock,
      is.na(resolved_stock_origin) & is.na(oto_stock) & !is.na(dna_results_stock_1) ~ dna_results_stock_1,
      TRUE ~ resolved_stock_origin
    ),
    hatchery_origin = case_when(
      is.na(hatchery_origin) & resolved_stock_source == "CWT" ~ "Y",
      is.na(hatchery_origin) & resolved_stock_source == "CWT" ~ "Y",
      TRUE ~ hatchery_origin
    ),
    # All creel data are in FL
    poh_length_mm = if_else(
      sample_type == "Sport",
      convert_fl_poh(length_mm, input = "fl"),
      length_mm
    )
  ) |> 
  # Trim unneeded columns
  select(
    program,
    area_name,
    subarea,
    year,
    month,
    collection_date,
    sample_type,
    adipose_fin_clipped,
    length_mm,
    sex,
    resolved_stock_origin,
    resolved_stock_source,
    hatchery_origin,
    resolved_age,
    oto_stock,
    age_gr,
    cwt_result,
    cwt_brood_year,
    dna_results_stock_1,
    prob_1
  )
# This file includes some escapement, broodstock, and FSC data - to my surprise. 
# Keep it or drop it?
# Or compare lab numbers and remove any that overlap Katie's dataset...?
# Ideally the latter


# Load escapement & FSC biodata from StAD file and from historical biodata ----------


# Escapement data from StAD excel records, collated and cleaned in R by Katie Davidson
esc_data <- list.files(
  here("Demographic data"),
  pattern = "(?i)escapement-fsc",
  full.names = TRUE
) |> 
  # Should be only one file
  read_xlsx(
    sheet = "Esc biodata w RESULTS",
    na = na_strings
  ) |> 
  clean_names() |> 
  filter(species == "Chinook") |> 
  select(
   r_sample_year,
   sample_month,
   matches("sample.*date"),
   stat_area,
   sub_area,
   fishery_river,
   sample_type,
   gear,
   sex,
   contains("length"),
   ad_clipped,
   pads_gr_age,
   r_resolved_total_age,
   r_brood_year,
   r_otolith_lbv_concat,
   om_read_status,
   mrp_status,
   mrp_stock_site_name,
   mrp_tagcode,
   r_origin,
   r_cwt_stock_id,
   r_otolith_stock_id,
   r_resolved_stock_id_method,
   r_resolved_stock_id
  ) |> 
  rename_with(~ str_remove(.x, "^r_")) |> 
  rename(
    "oto_nums_concat" = otolith_lbv_concat,
    "cwt_tag_code" = mrp_tagcode,
    "poh_length" = poh_length_mm,
    "year" = sample_year,
    "area" = stat_area,
    "sample_source" = sample_type
    ) |> 
  mutate(
    sample_date = sample_start_date_dd_mm_yyyy,
    resolved_age = resolved_total_age
  )



# RCH female data from the defunct Biodatabase
rch_f_data <- list.files(
  here("Demographic data"),
  pattern = "(?i)female",
  full.names = TRUE
) |> 
  read_xlsx(na = na_strings) |> 
  clean_names() |> 
  mutate(across(contains("date"), ~as.Date(.x, format = "%d-%b-%y"))) |> 
  select(
    year,
    statarea,
    project,
    sample_source,
    matches("sample.*date"),
    sex_final,
    cwt_tag_code,
    origin,
    contains("length"),
    contains("poh"),
    site_river_location,
    dna_specimen_number,
    adipose_fin_clip,
    gear,
    resolved_age,
    lab_number,
    box_code
  ) |> 
  mutate(
    oto_nums_concat = paste(lab_number, box_code, sep = "-"),
    resolved_stock_id = "Robertson Cr",
    sample_date = sample_start_date
  ) |> 
  rename(
    "sex" = sex_final,
    "poh_length" = post_orbital_hypural_poh,
    "area" = statarea
  )



# Join escapement/FSC and RCH female biodata ----------------------------------


# Check whether any overlap between samples in esc biodata and RCH female biodata
merge_data <- lst(rch_f_data, esc_data)

# Otolith numbers
merge_data |> 
  map(~select(.x, oto_nums_concat)) %>%
  {inner_join(.[[1]], .[[2]])}

# CWT numbers
(overlapping_cwts <- merge_data |> 
    map(~select(.x, cwt_tag_code)) %>%
    {inner_join(.[[1]], .[[2]], na_matches = "never")} |> 
    distinct(cwt_tag_code) |> 
    pull()
)
# 6 tag codes shared between the two datasets
# Assume Katie's dataset is more accurate/thorough and delete these records from
# the RCH females dataset


# Collate subsetted columns from the two dataframes 
merged_esc_fsc <- merge_data |> 
  map(
    ~ .x |> 
      mutate(across(everything(), as.character)) |> 
      select(
      resolved_stock_id,
      year,
      sample_date,
      area,
      sample_source,
      poh_length,
      sex,
      resolved_age,
      origin
    )
  ) |> 
  list_rbind(names_to = "dataset") 



# Collate CREST data with escapement/FSC data -----------------------------

# Check whether any overlap between samples in merged_data and crest data



# Adjust column names in crest dataset and merge with the esc/fsc data
full_data <- crest_data |> 
  rename(
    "resolved_stock_id" = resolved_stock_origin,
    "sample_date" = collection_date,
    "area" = area_name,
    "sample_source" = sample_type,
    "poh_length" = length_mm,
    "origin" = hatchery_origin
  ) |> 
  mutate(
    dataset = "CREST/FOS",
    across(everything(), as.character)
  ) |> 
  select(colnames(merged_esc_fsc)) |> 
  bind_rows(merged_esc_fsc)



# Trim non-WCVI origin fish from the full data ----------------------------


# First need to standardize stock of origin names
split_data <- full_data |> 
  distinct(dataset, resolved_stock_id) |> 
  mutate(
    resolved_stock_id = str_remove_all(
      resolved_stock_id,
      "(?i)river|hatchery|creek|\\br\\b|\\bcr\\b|\\(assumed\\)"
    ) |> 
      str_to_lower()
  ) %>% 
  split(.$dataset) |> 
  map(~select(.x, -dataset))


stock_names <- stringdist_full_join(
  split_data[[1]], 
  split_data[[2]], 
  method = "hamming"
  )

# Plots to examine size-at-age trends -------------------------------------


