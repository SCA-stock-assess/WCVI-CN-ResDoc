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


# Function to clean stock IDs
clean_stock_id <- function(stock_name) {
  
  clean_name <- stock_name |> 
    #remove anything preceding a dash, and "(assumed)"
    str_remove_all("^.*(?=-)|\\(assumed\\)") |> 
    str_replace_all("[:punct:]", " ") |> # Remove any punctuation characters
    # Remove all versions of river, hatchery, etc from names
    str_remove_all("(?i)\\b(river|hatchery|creek|fishway|dam|system|inlet|harbour|seapen|r|cr|h)\\b") |> 
    str_replace_all("[:space:]+", " ") |> # Collapse all spaces to single
    str_trim() |> # Remove leading/trailing blank spaces
    str_to_lower()
  
  return(clean_name)
}


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
    resolved_age = resolved_total_age,
    resolved_stock_id = clean_stock_id(resolved_stock_id)
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



# Collate ENPRO data with escapement/rch fem data -------------------------


# Check the degree of overlap in years
merge2_rule <- enpro_data |> 
  mutate(year = as.character(year)) |> 
  count(resolved_stock_id, year, name = "enpro_n") |> 
  full_join(count(merge1, year, resolved_stock_id)) |> 
  # Stipulate rules for keeping one dataset versus the other |> 
  mutate(
    to_keep = case_when(
      is.na(n) & !is.na(enpro_n) ~ "enpro",
      !is.na(n) & is.na(enpro_n) ~ "other",
      enpro_n > n ~ "enpro",
      n >= enpro_n ~ "other"
    )
  ) %>% 
  split(.$to_keep)


# Merge ENPRO data with the previous section's data
merge2 <- enpro_data |> 
  mutate(
    dataset = "enpro",
    across(everything(), as.character)
  ) |> 
  select(colnames(merge1)) |> 
  # Remove the rows that will come from the other data
  anti_join(select(merge2_rule$other, year, resolved_stock_id)) |> 
  bind_rows(
    merge1 |> 
      anti_join(select(merge2_rule$enpro, year, resolved_stock_id))
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
  # Remove some extreme outlier/error measurements
  filter(
    !poh_length > 1300,
    !(poh_length < 350 & resolved_age > 3)
  )


# Plots to examine size-at-age trends -------------------------------------


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
# If anything, this plot show size-at-age increasing
# However, these data are likely confounded by differences in sample 
# sources or stock compositions over the years


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
# Is the spike in recent years real or driven by a change in sampling methodology?  
  

# Question 3: Where are the majority of observations coming from over time?
clean_data |> 
  count(year, dataset) |> 
  ggplot(aes(year, dataset)) +
  geom_tile(aes(fill = n))
# CREST data takes over after 2018... Could be a different length measure


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
  filter(resolved_age %in% c(3:6)) |> 
  ggplot(aes(year, poh_length)) +
  facet_grid(dataset ~ resolved_age) +
  geom_bin_2d(binwidth = c(1, 50)) +
  stat_poly_line() +
  stat_poly_eq()
# Can anything be done to reduce the noise in the datasets?


# Question 6: How are lengths in the CREST data for non-sport samples?
clean_data |> 
  filter(
    resolved_age %in% c(3:6),
    dataset == "CREST/FOS"
  ) |> 
  ggplot(aes(year, poh_length)) +
  facet_grid(~ sample_source) +
  geom_bin_2d(binwidth = c(1, 50)) +
  stat_poly_line() +
  stat_poly_eq()
# Can anything be done to reduce the noise in the datasets?

