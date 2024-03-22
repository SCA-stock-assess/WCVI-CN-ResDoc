# Packages & functions -----------------------------

pkgs <- c(
  "tidyverse", "magrittr", "ggridges", "ggpmisc","here", "read_xl", "fuzzyjoin"
)
#install.packages(pkgs)

library(readxl)
library(tidyverse); theme_set(theme_bw(base_size = 18))
library(janitor)
library(fuzzyjoin)
library(magrittr)
library(ggridges)
library(ggpmisc)
library(here)


# Save list of all possible NA strings
na_strings <- c("", " ", "N/A", "#N/A", "NA", "N/S", "na")



# Load historical ENPRO data from 1992-2018 -------------------------------


# ENPRO file that I found randomly on the network drives
enpro_data <- list.files(
  here("Demographic data"),
  pattern = "(?i)enpro",
  full.names = TRUE
) |> 
  read_xlsx(sheet = "Data") |> 
  clean_names() |> 
  mutate(
    age_cwt = if_else(!is.na(cwt_tag_code), year - brood_year, NA_real_),
    across(everything(), as.character)
  ) |> 
  select(
    year,
    statarea,
    project,
    sample_source, 
    matches("sample.*date"),
    life_stage,
    sex_final,
    cwt_tag_code,
    post_orbital_hypural_poh,
    site_river_location,
    adipose_fin_clip,
    enpro_source_facility,
    brood_year,
    contains("age"),
    contains("length"),
    -total_length, # No numeric variables in this column, just "TRUE" or NAs
    -length_unknown_type # These values are of no use
  ) |>
  # Fix up ages and assign stocks
  mutate(
    # Convert complete GR ages to total ages
    across(
      c(resolved_age, age_gr), 
      ~if_else(
        str_detect(.x,"[:digit:]{2}"),
        str_extract(.x, "^[:digit:]{1}"),
        .x
      )
    ),
    # Fix up resolved_age values, drawing from other age columns where available
    # Will assume sub1s for partial ages when stocks have been filtered to WCVI-origin
    resolved_age = case_when(
      !is.na(age_cwt) ~ age_cwt,
      resolved_age %in% c("0", "No Age", "Reading Error") ~ age_gr,
      str_detect(resolved_age, "0[:digit:]") & !is.na(age_gr) ~ age_gr,
      str_detect(resolved_age, "0[:digit:]") & is.na(age_gr) ~ str_remove(resolved_age, "0"),
      is.na(resolved_age) & !is.na(age_gr) ~ age_gr,
      T ~ resolved_age
    ),
    # Use sampling sites as stock names when no ENPRO facility is assigned
    resolved_stock_id = if_else(
      is.na(enpro_source_facility) & sample_source %in% c("River Assessment", "Hatchery Assessment"), 
      site_river_location, 
      enpro_source_facility
    ),
    across(everything(), parse_guess)
  ) |> 
  rename(
    "poh_length" = post_orbital_hypural_poh,
    "nose_fork_length" = fork_length_nose_fork
  )


# Investigate potential conversions between other lengths to poh
poh_conv_models <- enpro_data |> 
  select(contains("length"), -poh_length) |> 
  colnames() |> 
  as_tibble_col(column_name = "length_type") |> 
  mutate(
    data = map(
      .x = length_type,
      ~ enpro_data |> 
        select(poh_length, all_of(.x)) |> 
        filter(!if_any(everything(), is.na)) |> 
        rename("length2" = 2)
    ),
    mod = map(
      .x = data,
      ~lm(poh_length ~ length2, data = .x)
    ),
    coef = map(mod, coef),
    r_sq = map(mod, summary) |> map('r.squared')
  ) |> 
  unnest_wider(coef)


# Show the regressions
poh_conv_models |> 
  select(length_type, data) |> 
  unnest(data) |> 
  ggplot(aes(length2, poh_length)) +
  facet_wrap(~length_type, nrow = 1, strip.position = "bottom") +
  stat_binhex() +
  scale_fill_viridis_c(option = "rocket") +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  labs(x = NULL) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank()
  )


# Save function to convert lengths
convert_to_poh <- function(measurement, input = c("fl", "sl")) {
  if(!input %in% c("fl", "sl")) {
    stop("Input must be 'fl' or 'sl'") # Throw error if input is incorrectly specified
  } 
  
  pred_data <- mutate(
    .data = poh_conv_models,
    val = measurement,
    pred = `(Intercept)` + val*length2
  )
  

    if(input == "fl") {
    # These values from a linear regression of NF on POH; R squared of 0.97
    return(pred_data[pred_data$length_type == "nose_fork_length",]$pred)
  } else {
    return(pred_data[pred_data$length_type == "standard_length",]$pred)
  }
  # On average, the conversion will err by ~3%
  
}



# Load and collate all CREST biodata ----------------------------------


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

# List of WCVI river (and hatchery) names
wcvi_cn_rivers <- tibble(
  wcvi_names = c(
    "artlish", "ash", "bedwell", "burman","cayeghle",
    "clemens", "colonial", "colonial cayeghle","conuma", "cypre", "effingham", "gold",
    "great central lake", "kaouk", "kauwinch", "kennedy", "leiner",
    "lowry", "marble", "megin", "moyeha", "nahmint", "nitinat",
    "omega pacific", "robertson", "san juan", "sarita", "sproat",
    "stamp", "tahsis", "tahsish", "thornton", "tofino", "toquaht",
    "tranquil", "ursus", "zeballos"
  )
)


# Create lookup table to bridge messy stock names to clean WCVI names
stock_cleanup <- full_data |> 
  count(dataset, resolved_stock_id) |> 
  # First need to standardize stock of origin names
  mutate(
    cleaned_stock_id = str_remove_all(
      resolved_stock_id,
      "^.*(?=-)|\\(assumed\\)" # remove anyhing preceding a dash, and "(assumed)"
    ) |> 
      str_replace_all("[:punct:]", " ") |> # Remove any punctuation characters
      # Remove all versions of river, hatchery, etc from names
      str_remove_all("(?i)\\b(river|hatchery|creek|fishway|dam|system|inlet|harbour|seapen|r|cr|h)\\b") |> 
      str_replace_all("[:space:]+", " ") |> # Collapse all spaces to single
      str_trim() # Remove leading/trailing blank spaces
  ) |> 
  filter(!is.na(resolved_stock_id)) |> 
  stringdist_left_join(
    wcvi_cn_rivers, 
    by = c(cleaned_stock_id = "wcvi_names"),
    max_dist = 1,
    ignore_case = TRUE,
    distance_col = "dist"
  ) %T>%
  # Print (but don't save) duplicated rows 
  {filter(
    ., 
    .by = c(dataset, resolved_stock_id),
    n() > 1
  )  |> 
      print()
  } |> 
  # Trim duplicated rows
  arrange(dist) |> # Place lowest "dist" values highest to remove poorer matches
  filter(!duplicated(cbind(dataset, resolved_stock_id))) %T>% 
  # Print (but don't save) duplicated rows 
  {filter(
    ., 
    .by = c(dataset, resolved_stock_id),
    n() > 1
  )  |> 
      print()
  }
# Looks good! Need to keep checking this to ensure nothing slips through as new 
# data are added


# Now filter the data to remove all non WCVI-origin fish, as well as 
# Any fish with missing ages or lengths
trim_data <- full_data |> 
  left_join(
    select(.data = stock_cleanup, dataset, resolved_stock_id, wcvi_names),
    relationship = "many-to-one"
    ) |> 
  # Clean up values in age and sex
  mutate(
    resolved_age = if_else(
      str_detect(resolved_age, "^(3|4|5|6)"),
      str_extract(resolved_age, "^.{1}"),
      NA_character_
    ) |> 
      as.numeric()
  ) |> 
  # Remove non-WCVI stocks, and missing values in length or age
  filter(!if_any(c(wcvi_names, poh_length, resolved_age), is.na)) 



# Clean up messy variables in the data ------------------------------------


# Cleaned version
clean_data <- trim_data |> 
  mutate(
    sex = case_when(
      str_detect(sex, "(?i)^(j|m)") ~ "male",
      str_detect(sex, "(?i)^f") ~ "female",
      T ~ NA_character_
    ),
    origin = case_when(
      str_detect(origin, "(?i)hat|y") ~ "hatchery",
      str_detect(origin, "(?i)nat") ~ "natural",
      T ~ "unknown"
    ),
    sample_date = convert_to_date(sample_date), # Convenient janitor fn
    area = str_extract(area, "(2|12)[:graph:]*(?=\\b)"),
    sample_source = case_when(
      str_detect(sample_source, "(?i)(gill|gn)") ~ "gill net",
      str_detect(sample_source, "(?i)(seine|sn)") ~ "seine net",
      str_detect(sample_source, "(?i)(troll|tr)") ~ "troll",
      str_detect(sample_source, "(?i)brood|hatch") ~ "hatchery",
      T ~ str_to_lower(sample_source)
    )
  )


# Plots to examine size-at-age trends -------------------------------------


