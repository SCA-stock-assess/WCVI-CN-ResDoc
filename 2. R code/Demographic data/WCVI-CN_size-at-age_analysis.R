# Packages & functions -----------------------------

pkgs <- c(
  "tidyverse", "magrittr", "ggridges", "ggpmisc",
  "here", "read_xl", "fuzzyjoin", "broom.mixed", "merTools"
)
#install.packages(pkgs)

library(readxl)
library(writexl)
library(here)
library(lme4)
library(broom.mixed)
library(merTools) # MASS/dplyr conflict over select(); ensure dplyr::select() takes precedent
library(tidyverse); theme_set(theme_bw(base_size = 18))
library(janitor)
library(fuzzyjoin)
library(magrittr)
library(ggridges)
library(ggpmisc)


# Save list of all possible NA strings
na_strings <- c("", " ", "N/A", "#N/A", "NA", "N/S", "na")

# List of WCVI river (and hatchery) names
wcvi_cn_rivers <- tibble(
  wcvi_names = c(
    "artlish", "ash", "bedwell", "burman","cayeghle",
    "clemens", "colonial", "colonial cayeghle","conuma", "cypre", "effingham", "gold",
    "great central lake", "kaouk", "kauwinch", "kennedy", "leiner",
    "lowry", "marble", "megin", "moyeha", "nahmint", "nitinat",
    "omega pacific", "robertson", "san juan", "sarita", "sproat",
    "stamp", "stamp above falls", "tahsis", "tahsish", "thornton", "tofino", 
    "toquaht","tranquil", "ursus", "zeballos"
  )
)


# Function to clean stock IDs
clean_stock_id <- function(stock_name) {
  
  clean_name <- stock_name |> 
    #remove anything preceding a dash, and "(assumed)"
    str_remove_all("^.*(?=-)|\\(assumed\\)") |> 
    str_replace_all("[:punct:]", " ") |> # Remove any punctuation characters
    # Remove all versions of river, hatchery, etc from names
    str_remove_all(
      "(?i)\\b(river|hatchery|creek|fishway|dam|system|inlet|harbour|band|seapen|estuary|r|cr|h)\\b"
    ) |> 
    str_replace_all("[:space:]+", " ") |> # Collapse all spaces to single
    str_trim() |> # Remove leading/trailing blank spaces
    str_to_lower()
  
  return(clean_name)
}


# Load SEP WCVI major ops data from 1998-2023 -------------------------------


# EPRO adult biosampling files (xlsx)
epro_partial <- list.files(
  here("1. data files"),
  pattern = "(?i)all_adult_biosampling_[[:digit:]]{4}.*xlsx",
  full.names = TRUE
) |> 
  as_tibble_col(column_name = "path") |> 
  rowwise() |> 
  mutate(
    sheet = excel_sheets(path),
    data = list(
      read_excel(
        path, 
        sheet = sheet, 
        na = na_strings
      ) |> 
        mutate(across(everything(), as.character))
    )
  ) |> 
  select(-(path:sheet)) |> 
  unnest(data) |>
  clean_names()


# EPRO data (incl 2021-2023 with results file)
epro_data <- list.files(
  here("1. data files"),
  pattern = "(?i)all_adult_biosampling.*2021-2023",
  full.names = TRUE
) |> 
  read_excel(sheet = 2) |> 
  clean_names() |> 
  # Convert all columns to character to avoid variable-type conflict errors
  mutate(across(everything(), as.character)) |> 
  # Add the 2020 data that were imported above
  bind_rows(epro_partial) |> 
  mutate(
    across(everything(), parse_guess),
    start_date = as.Date(start_date, format = "%m/%d/%Y"),
    year = str_extract_all(spawning_stock, "\\d{4}") |> as.integer(),
    resolved_age = case_when(
      !is.na(r_resolved_total_age) ~ r_resolved_total_age,
      cwt_age_yrs %in% c(1:6) ~ cwt_age_yrs,
      !is.na(scale_total_age_yrs) ~ scale_total_age_yrs,
      !is.na(scale_gilbert_age) ~ round(scale_gilbert_age, -1) / 10,
      str_detect(scale_part_age, "\\dM") ~ as.numeric(str_extract(scale_part_age, "\\d")) + 1,
      T ~ NA_real_
    ),
    resolved_stock_id = case_when(
      !is.na(r_resolved_stock_id) ~ r_resolved_stock_id,
      str_detect(spawning_stock, "(?i)sarita") ~ "Sarita R (assumed)",
      str_detect(spawning_stock, "(?i)burman") ~ "Burman R (assumed)",
      str_detect(spawning_stock, "(?i)nahmint") ~ "Nahmint R (assumed)",
      str_detect(spawning_stock, "(?i)robertson") ~ "Robertson Cr (assumed)",
      str_detect(spawning_stock, "(?i)conuma") ~ "Conuma R (assumed)",
      T ~ NA_character_
    ),
    area = case_when(
      str_detect(spawning_stock, "(?i)sarita|nahmint|robertson") ~ 23,
      str_detect(spawning_stock, "(?i)burman|conuma|gold") ~ 25,
      str_detect(spawning_stock, "(?i)nitinat") ~ 22,
      str_detect(spawning_stock, "(?i)san juan") ~ 21,
      T ~ NA_real_
    ),
    major_ops = "yes",
    brood_year = year - resolved_age,
    origin = case_when(
      !is.na(r_origin) ~ r_origin,
      external_marks == "Clipped" ~ "Hatchery",
      !hatch_code %in% c("Destroyed", "No Mark", "Not Marked", "No Sample", NA) ~ "Hatchery",
      hatch_code %in% c("Not Marked", "No Mark") ~ "Natural",
      T ~ "Unknown"
    ),
    sex = case_when(
      str_detect(maturity_class, "(?i)female") ~ "female",
      str_detect(maturity_class, "(?i)male|jack|jimmy") ~ "male",
      TRUE ~ "Unknown"
    )
  ) |> 
  select(
    year,
    area,
    spawning_stock,
    major_ops,
    parent_activity_type,
    sex,
    start_date,
    maturity_class,
    cwt_tag_code,
    contains("length"),
    source_location_type,
    external_marks,
    brood_year,
    resolved_age,
    resolved_stock_id,
    origin
  ) |> 
  rename(
    "project" = spawning_stock,
    "sample_source" = parent_activity_type,
    "sample_date" = start_date,
    "life_stage" = maturity_class,
    "poh_length" = poh_length_mm,
    "nose_fork_length" = nose_to_fork_length_mm,
    "standard_length" = standard_length_mm,
    "site_river_location" = source_location_type,
    "adipose_fin_clip" = external_marks
  )


# ENPRO (+misc) file that I found randomly on the network drives
enpro_data <- list.files(
  here("1. data files"),
  pattern = "(?i)enpro",
  full.names = TRUE
) |> 
  read_xlsx(sheet = "Data") |> 
  clean_names() |> 
  # Keep only WCVI hatchery projects
  filter(str_detect(str_to_lower(project), paste(wcvi_cn_rivers$wcvi_names, collapse = "|"))) |> 
  mutate(
    age_cwt = if_else(!is.na(cwt_tag_code), year - brood_year, NA_real_),
    across(everything(), as.character),
    major_ops = if_else(str_detect(project, "(?i)\\d{4}.*chinook"), "yes", "no")
  ) |> 
  select(
    year,
    statarea,
    project,
    major_ops,
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
    ) |> 
      clean_stock_id(),
    across(everything(), parse_guess),
    origin = if_else(is.na(adipose_fin_clip) & is.na(enpro_source_facility), "unknown", "hatchery")
  ) |> 
  rename(
    "poh_length" = post_orbital_hypural_poh,
    "nose_fork_length" = fork_length_nose_fork,
    "sex" = sex_final,
    "area" = statarea,
    "sample_date" = sample_start_date
  )


# Join EPRO to ENPRO+ data
sep_data <- list(enpro_data, epro_data) |> 
  map(~mutate(.x, across(everything(), as.character))) |> 
  list_rbind() |> 
  mutate(
    across(everything(), parse_guess),
    resolved_stock_id = clean_stock_id(resolved_stock_id)
  )



# Investigate potential conversions between other lengths to poh
poh_conv_models <- sep_data |> 
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
  
  fl_intercept <- poh_conv_models[poh_conv_models$length_type == "nose_fork_length",]$`(Intercept)`
  fl_x <- poh_conv_models[poh_conv_models$length_type == "nose_fork_length",]$length2
  sl_intercept <- poh_conv_models[poh_conv_models$length_type == "standard_length",]$`(Intercept)`
  sl_x <- poh_conv_models[poh_conv_models$length_type == "standard_length",]$length2
  
  
  if(input == "fl") {
    # These values from a linear regression of NF on POH; R squared of 0.97
    return(fl_intercept + measurement*fl_x)
  } else {
    return(sl_intercept + measurement*sl_x)
  }
  # On average, the conversion will err by ~3%
  
}


# Convert fork lengths to poh lengths, where available
sep_data <- sep_data |> 
  mutate(
    poh_length_corrected = if_else(
      is.na(poh_length) & str_detect(nose_fork_length, "\\d{2,4}"),
      convert_to_poh(as.numeric(nose_fork_length), input = "fl"),
      poh_length
    )
  )


# Load and collate all CREST biodata ----------------------------------


# Extract the data from the WCVI Run Reconstruction tabs
crest_data <- list.files(
  here("1. data files"),
  pattern = "CREST.*",
  full.names = TRUE
) |> 
  as_tibble_col(column_name = "path") |> 
  mutate(
    data = map(
      .x = path,
      ~ read_xlsx(
        path = .x, 
        sheet = "WCVI_Chinook_Run_Rec",
        na = na_strings,
        col_types = "text" # prevents a deluge of unnecessary coltype warnings
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
    ) |> 
      clean_stock_id(),
    hatchery_origin = case_when(
      is.na(hatchery_origin) & resolved_stock_source == "CWT" ~ "Y",
      is.na(hatchery_origin) & resolved_stock_source == "CWT" ~ "Y",
      TRUE ~ hatchery_origin
    ),
    # All creel data are in FL
    poh_length_mm = if_else(
      sample_type == "Sport",
      convert_to_poh(length_mm, input = "fl"),
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
  ) |> 
  rename(
    "resolved_stock_id" = resolved_stock_origin,
    "sample_date" = collection_date,
    "area" = area_name,
    "sample_source" = sample_type,
    "poh_length" = length_mm,
    "origin" = hatchery_origin
  )
# This file includes some escapement, broodstock, and FSC data - to my surprise. 
# Keep it or drop it?
# Or compare lab numbers and remove any that overlap Katie's dataset...?
# Ideally the latter



# Load escapement & FSC biodata from StAD file and from historical biodata ----------


# Escapement data from StAD excel records, collated and cleaned in R by Katie Davidson
stad_data <- list.files(
  here("1. data files"),
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
    resolved_age = resolved_total_age,
    resolved_stock_id = clean_stock_id(resolved_stock_id)
  )



# RCH female data from the defunct Biodatabase
rch_f_data <- list.files(
  here("1. data files"),
  pattern = "(?i)rch_female_cn",
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
    resolved_stock_id = clean_stock_id("Robertson Cr"),
    sample_date = sample_start_date
  ) |> 
  rename(
    "sex" = sex_final,
    "poh_length" = post_orbital_hypural_poh,
    "area" = statarea
  )




# Join escapement/FSC and RCH female biodata ----------------------------------


# Check whether any overlap between samples in esc biodata and RCH female biodata
merge_data <- lst(rch_f_data, stad_data)


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
merge1 <- merge_data |> 
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



# Collate SEP data with escapement/rch fem data -------------------------


# Check the degree of overlap in years
merge2_rule <- sep_data |> 
  mutate(year = as.character(year)) |> 
  count(resolved_stock_id, year, name = "sep_n") |> 
  full_join(count(merge1, year, resolved_stock_id)) |> 
  # Stipulate rules for keeping one dataset versus the other |> 
  mutate(
    to_keep = case_when(
      is.na(n) & !is.na(sep_n) ~ "sep",
      !is.na(n) & is.na(sep_n) ~ "other",
      sep_n > n ~ "sep",
      n >= sep_n ~ "other"
    )
  ) %>% 
  split(.$to_keep)


# Merge ENPRO data with the previous section's data
merge2 <- sep_data |> 
  mutate(
    dataset = "sep",
    across(everything(), as.character)
  ) |> 
  select(colnames(merge1)) |> 
  # Remove the rows that will come from the other data
  anti_join(select(merge2_rule$other, year, resolved_stock_id)) |> 
  bind_rows(
    merge1 |> 
      anti_join(select(merge2_rule$sep, year, resolved_stock_id))
  )


# Collate CREST data with escapement/FSC/ENPRO data -----------------------------


# Look at the types of esc/fsc samples in CREST
crest_data |> 
  mutate(year = as.character(year)) |>
  filter(str_detect(sample_source, "(?i)esc|brood|fsc")) |> 
  count(resolved_stock_id, area) |> 
  arrange(desc(n)) |> 
  print(n=200)
# The bulk of the samples don't have any stock or area information associated


# Check degree of overlap between samples in merged_data and crest data
# and stipulate rule for keeping data
merge3_rule <- crest_data |> 
  mutate(year = as.character(year)) |>
  filter(str_detect(sample_source, "(?i)esc|brood|fsc")) |> # Only look at the escapement/fsc samples
  count(resolved_stock_id, year, sample_source, name = "crest_n") |> 
  full_join(count(merge2, year, resolved_stock_id)) |> 
  # Trim data so only the rows to remove from CREST remain
  filter(!is.na(crest_n) & !is.na(n)) %T>%
  {print(arrange(., desc(crest_n)), n = 50)} |> 
  # In some instances, there is a lot of CREST data and very little in
  # the other dataset, so we'll keep those CREST data
  filter(
    # These data are likely in *EPRO*
    !(resolved_stock_id == "nitinat" & year %in% c("2020", "2019") & sample_source == "Escapement"),
    !(resolved_stock_id == "conuma" & year == "2018" & sample_source == "Escapement"),
    !(resolved_stock_id == "gold" & year %in% c("2015", "2017") & sample_source == "Escapement"),
    !(resolved_stock_id == "tahsis" & year =="2018" & sample_source == "Escapement"),
    !(resolved_stock_id == "leiner" & year =="2017" & sample_source == "Escapement")
  ) %T>%
  print()


# Adjust column names in crest dataset and merge with the esc/fsc data
full_data <- crest_data |> 
  mutate(
    dataset = "CREST/FOS",
    across(everything(), as.character)
  ) |> 
  anti_join(merge3_rule) |> 
  select(colnames(merge2)) |> 
  bind_rows(merge2)



# Trim non-WCVI origin fish from the full data ----------------------------


# Create lookup table to bridge messy stock names to clean WCVI names
stock_cleanup <- full_data |> 
  count(dataset, resolved_stock_id) |> 
  filter(!is.na(resolved_stock_id)) |> 
  stringdist_left_join(
    wcvi_cn_rivers, 
    by = c(resolved_stock_id = "wcvi_names"),
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
    resolved_age = case_when(
      # Assume sub1s for partial ages
      str_detect(resolved_age, "M[:digit:]|[:digit:]M") ~ 
        as.character(as.numeric(str_extract(resolved_age, "[:digit:]")) + 1),
      str_detect(resolved_age, "^(3|4|5|6)") ~ str_extract(resolved_age, "^.{1}"),
      TRUE ~ NA_character_
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
    sample_date = convert_to_datetime( # Convenient janitor fn
      sample_date,
      string_conversion_failure = "warning"
      ) |> 
      as.Date(), 
    area = str_extract(area, "(2|12)[:graph:]*(?=\\b)"),
    sample_source = case_when(
      str_detect(sample_source, "(?i)(gill|gn)") ~ "gill net",
      str_detect(sample_source, "(?i)(seine|sn)") ~ "seine net",
      str_detect(sample_source, "(?i)(troll|tr)") ~ "troll",
      str_detect(sample_source, "(?i)brood|hatch") ~ "hatchery",
      T ~ str_to_lower(sample_source)
    ),
    across(where(is.character), parse_guess)
  ) |> 
  filter(
    # Remove some extreme outlier/error measurements
    !poh_length > 1300,
    !(poh_length < 350 & resolved_age > 3),
    # Keep only sport and escapement samples from CREST
    !(dataset == "CREST/FOS" & str_detect(sample_source, "(?i)esc|sport", negate = TRUE))
  )


# Exploratory plots to look at structure of data between datasets ----------------------------


# Question 1: Is there a broad trend evident in all WCVI-origin fish?
clean_data |> 
  filter(resolved_age %in% c(3:6)) |> 
  ggplot(aes(year, poh_length)) +
  facet_wrap(
    ~resolved_age, 
    nrow = 1
  ) +
  geom_bin_2d(binwidth = c(1, 50)) +
  geom_smooth()
# Possible decrease?


# Question 2: Can we resolve any stock-specific trends in size-at-age?
clean_data |> 
  filter(resolved_age %in% c(3:6)) |> 
  group_by(resolved_stock_id) |> 
  filter(!length(unique(year)) < 5) %>%
  split(.$resolved_stock_id) |> 
  imap(
    ~ .x |> 
      ggplot(aes(year, poh_length)) +
      facet_wrap(
        ~resolved_age, 
        nrow = 1
      ) +
      geom_point(shape = 21, colour = "black", fill = alpha("white", 0)) +
      geom_smooth(se = FALSE) +
      labs(title = .y)
  )
# Not much is clear here
  

# Question 3: Where are the majority of observations coming from over time?
clean_data |> 
  count(year, dataset) |> 
  ggplot(aes(year, dataset)) +
  geom_tile(aes(fill = n))
# CREST data takes over after 2018... Could be a different length measure


# CREST data consistently longer. 
clean_data |> 
  filter(!sample_source == "sport") |> 
  mutate(
    crest_poh = if_else(
      dataset == "CREST/FOS", 
      convert_to_poh(poh_length, "fl"),
      NA_real_
      )
    ) |> 
  summarize(
    .by = c(resolved_age, dataset), 
    avg_length = mean(poh_length),
    avg_crest_poh = mean(crest_poh)
  ) |> 
  pivot_longer(contains("avg")) |> 
  pivot_wider(
    names_from = dataset, 
    values_from = value
  ) |> 
  filter(!is.na(`CREST/FOS`)) |> 
  group_by(resolved_age) |> 
  fill(sep:stad_data) |> 
  mutate(avg_diff = `CREST/FOS`/((sep+stad_data)/2))
# The POH conversion developed using EPRO data might work...
# An age-specific conversion might work even better


# Question 4: Same as Question 2, but what if we remove CREST data?
clean_data |> 
  filter(
    resolved_age %in% c(3:6),
    dataset != "CREST/FOS"
  ) |> 
  group_by(resolved_stock_id) |> 
  filter(!length(unique(year)) < 5) %>%
  split(.$resolved_stock_id) |> 
  imap(
    ~ .x |> 
      ggplot(aes(year, poh_length)) +
      facet_wrap(
        ~resolved_age, 
        nrow = 1
      ) +
      geom_point(shape = 21, colour = "black", fill = alpha("white", 0)) +
      geom_smooth(se = FALSE) +
      labs(title = .y)
  )


# Question 5: Same as Question 1, but broken out by dataset
clean_data |> 
  filter(!(dataset == "CREST/FOS" & sample_source != "sport")) |> 
  filter(resolved_age %in% c(3:6)) |> 
  ggplot(aes(year, poh_length)) +
  facet_grid(dataset ~ resolved_age) +
  geom_bin_2d(binwidth = c(1, 50)) +
  stat_poly_line() +
  stat_poly_eq()
# Can anything be done to reduce the noise in the datasets?


# Question 6: How are lengths in the CREST data?
crest_data |> 
  filter(
    resolved_age %in% c(3:6),
    poh_length < 1300
  ) |> 
  group_by(sample_source, resolved_age) |> 
  filter(!(length(unique(year)) < 5)) |> 
  ggplot(aes(year, poh_length)) +
  facet_grid(resolved_age ~ sample_source) +
  geom_bin_2d(binwidth = c(1, 50)) +
  stat_poly_line() +
  stat_poly_eq()
# Can anything be done to reduce the noise in the datasets?



# Plots showcasing trends in size-at-age using reliable data --------------


# What we learned from the last section is that:
# - lengths in CREST cannot easily be compared to the SEP or STAD escapement data
# - Only CREST sport and SEP escapement data have long enough time series for 
#   a valid size-at-age analysis
# - Most stock do not have enough data for a size-at-age analysis


# Trim data to only stocks with a 10+yr time series
strict_data <- clean_data |> 
  filter(
    !(dataset == "CREST/FOS" & sample_source != "sport"),
    resolved_age %in% 3:5
  ) |> 
  mutate(
    .by = c(dataset, resolved_age, year, resolved_stock_id),
    n = n(),
    dataset = if_else(dataset == "CREST/FOS", "Sport", "Escapement") |> 
      as.factor(),
    resolved_age = as.character(resolved_age)
  ) |> 
  mutate(
    .by = c(dataset, resolved_age, resolved_stock_id),
    years = length(unique(year))
  ) |> 
  group_by(dataset, resolved_stock_id) |> 
  filter(!any(years < 10))  |> 
  ungroup()


# Save strict data as an output file
strict_data |> 
  write_xlsx(
    path = here(
      "3. R outputs",
      "Demographic data",
      "R-OUT_cleaned_CN_biodata_size-at-age.xlsx"
    )
  )

# Plot time series data by age and stock
strict_data %>%
  split(.$dataset) |> 
  imap(
    ~ ggplot(.x, aes(year, poh_length)) +
      facet_grid(resolved_stock_id ~ resolved_age) +
      stat_bin2d(binwidth = c(1, 50)) +
      stat_poly_line() +
      stat_poly_eq(use_label(c("eq", "R2"))) +
      scale_fill_viridis_c(option = "rocket") +
      labs(title = .y)
  )


# Using ggridges
strict_data %>%
  split(.$dataset) |> 
  imap(
    ~ .x |> 
      ggplot(
        aes(
          poh_length, 
          fct_rev(as.character(year)),
          fill = n
          )
        ) +
      facet_grid(
        resolved_age ~ resolved_stock_id,
        scales = "free"
      ) +
      geom_density_ridges() +
      scale_fill_viridis_c(option = "mako", direction = -1) +
      labs(
        title = .y,
        x = "Length (mm)",
        y = "Year"
      ) +
      theme_ridges()
  )



# Model size-at-age data --------------------------------------------------


# First pass model structure
mod1 <- lmer(
  poh_length ~ resolved_age*scale(year, scale = FALSE)*dataset + 
    (scale(year, scale = FALSE)|resolved_stock_id),
  contrasts = list(resolved_age = 'contr.sum', dataset = 'contr.sum'),
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa"),
  data = mutate(strict_data, resolved_age = factor(resolved_age))
)
# Model converges

# Plot residuals
plot(mod1)
plot(mod1, dataset ~ resid(., type = "pearson"))
# Looks alright


# Model summary
broom.mixed::tidy(mod1, conf.int = TRUE)
car::Anova(mod1, type = 3)


# Estimate changes in length per year by dataset
vnames <- c("(Intercept)", "scale(year, scale = FALSE)", "scale(year, scale = FALSE):dataset1")
m <- fixef(mod1)[vnames]
v <- as.matrix(vcov(mod1))[vnames, vnames]

set.seed(3)
sampvals <- MASS::mvrnorm(100000, mu = m, Sigma = v) |> 
  as_tibble() |> 
  rename(
    "intercept" = 1,
    "year" = 2,
    "diff" = 3
  ) |> 
  mutate(
    val_sport = year + diff,
    val_escapement = year - diff,
    slope_sport = val_sport/intercept,
    slope_escapement = val_escapement/intercept
  )

# Summarize sample vals to show estimated % change in lengths over time
sampvals |> 
  pivot_longer(
    cols = contains("_"),
    names_sep = "_",
    names_to = c("measure", "dataset")
  ) |> 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |> 
  summarize(
    .by = dataset,
    avg_slope = mean(slope),
    lci = quantile(slope, c(0.025)),
    uci = quantile(slope, c(0.975))
  ) |> 
  # Convert to percentages, and use per-decade values
  mutate(across(avg_slope:uci, ~.x*100*10))
  

# Estimate changes in length per year by age
vnames2 <- str_subset(names(fixef(mod1)), "Intercept|(?<=(|resolved_age\\d:))scale\\(year(?!.*\\):)")
m2 <- fixef(mod1)[vnames2]
v2 <- as.matrix(vcov(mod1))[vnames2, vnames2]

sampvals2 <- MASS::mvrnorm(100000, mu = m2, Sigma = v2) |> 
  as_tibble() |> 
  rename(
    "intercept" = 1,
    "year" = 2,
    "a3adj" = 3,
    "a4adj" = 4
  ) |> 
  mutate(
    val_a3 = year + a3adj,
    val_a4 = year + a4adj,
    val_a5 = year - (a3adj + a4adj),
    across(matches("val_a\\d"), \(x) x/intercept, .names = "{str_replace(.col, 'val', 'slope')}")
  )

# Summarize sample vals to show estimated % change in lengths over time
sampvals2 |> 
  pivot_longer(
    cols = contains("_"),
    names_sep = "_",
    names_to = c("measure", "age")
  ) |> 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |> 
  summarize(
    .by = age,
    avg_slope = mean(slope),
    lci = quantile(slope, c(0.025)),
    uci = quantile(slope, c(0.975))
  ) |> 
  # Convert to percentages, and use per-decade values
  mutate(across(avg_slope:uci, ~.x*100*10))


# Dataset of model predictions
mod1_preds <- strict_data |> 
  tidyr::expand(resolved_age, nesting(dataset, year, resolved_stock_id)) %>%
  mutate(pred = predictInterval(mod1, newdata = .)) |> 
  unnest(pred) 


# Split data to plot predictions
pred_data <- strict_data |> 
  mutate(group = "observed") |> 
  bind_rows(mutate(mod1_preds, group = "predicted"))  |> 
  # Fix up names for Facet titles
  mutate(
    resolved_age = paste("Age", resolved_age),
    resolved_stock_id = str_to_title(resolved_stock_id),
    y_axis = if_else(dataset == "Sport", "Nose-fork length (mm)", "POH length (mm)")
  ) |> 
  nest(.by = c(dataset, y_axis, group)) |> 
  pivot_wider(
    names_from = group,
    values_from = data
  ) |> 
  rowwise() |> 
  mutate(
    sum_data = list(
      summarize(
        .data = observed,
        .by = c(resolved_age, resolved_stock_id, year),
        mean = mean(poh_length),
        lci = quantile(poh_length, 0.025),
        uci = quantile(poh_length, 0.975),
        n = n()
      )
    ),
    ggplot = list(
      observed |> 
        ggplot(aes(year, poh_length)) +
        facet_grid(resolved_stock_id ~ resolved_age) +
        # Some options to deal with overplotting
        #stat_bin2d(binwidth = c(1, 50)) +
        #stat_binhex(binwidth = c(1, 75)) +
        # geom_pointrange(
        #   data = sum_data,
        #   aes(
        #     y = mean,
        #     ymin = lci,
        #     ymax = uci,
        #     size = n
        #   )
        # ) +
        geom_point(
          size = 0.5,
          alpha = 0.2,
          position = position_jitter(width = 0.25)
        ) +
        geom_line(
          data = predicted,
          aes(y = fit),
          colour = "blue"
        ) +
        geom_ribbon(
          data = predicted,
          aes(ymin = lwr, ymax = upr),
          colour = NA,
          fill = "blue",
          alpha = 0.25
        ) +
        scale_fill_viridis_c(
          option = "rocket",
          limits = c(0, NA)
        ) +
        scale_size_continuous(range = c(0.1, 1)) +
        labs(
          title = paste("Data source:", dataset),
          y = y_axis
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  )


# Show plots
(pred_plots <- pull(pred_data, ggplot) |> 
  set_names(unique(pred_data$dataset))
)


# Save plots
pred_plots |> 
  iwalk(
    ~ggsave(
      .x,
      filename = here(
        "3. R outputs",
        "Demographic data",
        paste0("R-plot_LMM_pred_size-at-age_", .y, ".png")
      ),
      height = 8,
      width = 8,
      units = "in"
    )
  )

