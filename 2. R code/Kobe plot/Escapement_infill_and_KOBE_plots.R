# Packages ----------------------------------------------------------------

pkgs <- c(
  'tidyverse','RColourBrewer','gganimate','ggrepel',
  'readxl', 'writexl','janitor', 'here', "ggtext"
)
#install.packages(pkgs)

library(tidyverse); theme_set(theme_light(base_size = 18))
library(magrittr)
library(ggtext)
library(janitor)
library(RColorBrewer)
library(gganimate)
library(ggrepel)
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

  
# Load ER data time series and build dataframe for Kobe plot ---------------


# Estimated Robertson CN exploitation rate time series
rch_er <- read.csv(
  here(
    "1. data files", 
    "CTC_2024ERA_MREERdata_11.06.2024.csv"
  )
) |> 
  clean_names() |> 
  select(year, rate) |> 
  rename("er" = 2)


# Reference points data set
indicator_rp <- read_xlsx(
  here(
    "1. data files", 
    "FWA table update version 4 May30.xlsx"
  ),
  sheet = "RiverEscData", # This sheet has all the reference point values
  skip = 2
) |> 
  clean_names() |> 
  select(cu, indicator, watershed, stream_name, matches("life_cycle|lc")) |> 
  rename("river" = stream_name) |> 
  mutate(
    river = make_clean_names(river), # Clean up river names
    across(contains("lc"), as.numeric) 
  ) |> 
  filter(river %in% indicator_names) |> 
  # Summarize reference points across indicators to get SMU aggregate values
  summarize(
    smsy = sum(lc_smsy_median),
    srep = sum(lc_srep_median)
  ) |> 
  # Calculate Umsy
  mutate(
    alpha = exp((0.5-smsy/srep)/0.07),
    beta = log(alpha)/srep,
    umsy = (0.5*log(alpha)) - (0.07*(log(alpha))^2),
  )
  

# Make dataframe for KOBE plot
kobe_data <- infilled_esc |> 
  # Sum escapements across indicators for each year, and add the overall S/Umsy values
  summarize(
    .by = year,
    escapement = sum(escapement),
    umsy_lc = indicator_rp$umsy, # Life cycle method
    smsy_lc = indicator_rp$smsy, # Life cycle method
    umsy_rr = 0.57, # Equilibrium trade-off method (from run reconstruction data)
    smsy_rr = 18999 # Equilibrium trade-off method (from run reconstruction data)
  ) |> 
  left_join(rch_er) |> # Add ER time series
  # Lengthen data so each method has its own row
  pivot_longer(
    matches("(u|s)msy"),
    names_sep = "_",
    names_to = c(".value", "method")
  ) |> 
  mutate(
    x = escapement/smsy,
    y = er/umsy,
    # Stipulate which quadrant each year belongs to 
    quadrant = case_when(
      y >= 1 & x <= 1 ~ 1,
      y >= 1 & x > 1 ~ 5, 
      y < 1 & between(x,0.8,1) ~ 2, # Amber zone
      y < 1 & x <= 0.8 ~ 3,
      y < 1 & x > 1 ~ 4
    ) %>% factor(),
    # formal labels for each method
    method = factor(
      method, 
      levels = c("lc", "rr"), 
      labels = c("Life cycle (low productivity)", "Run reconstruction (moderate productivity)")
      )
  ) |> 
  # Remove years with missing data
  filter(!if_any(!year, is.na)) |> 
  arrange(year)


# Save the output data
writexl::write_xlsx(
  select(.data = kobe_data, -quadrant),
  here(
    "3. R outputs",
    "Kobe plot", 
    paste0("R-OUT_Kobe_plot_raw_data_", Sys.Date(), ".xlsx")
  )
)


# Plots -------------------------------------------------------------------


# Dataframe for quadrant labels
quadLabs <- data.frame(
  label = c(
    "Overfishing",
    "Overfishing but spawning<br>biomass sufficient",
    "Fisheries reduced to<br>allow rebuilding",
    "Sustainable fishery"
  ),
  x = c(0.1, 1.9, 0.1, 1.9),
  y = c(1.85, 1.85, 0.15, 0.15),
  hjust = c(0, 1, 0, 1)
) 


# Inspired by code from FSAR group
(kobe_plots <- kobe_data |> 
    ggplot(aes(x, y)) +
    facet_wrap(~method, nrow = 1) + # Make one panel for each method
    #add "crosshairs"
    geom_vline(
      xintercept = 1, 
      lty = 2,
      colour = "grey50"
    ) +
    geom_hline(
      yintercept = 1, 
      lty = 2,
      colour = "grey50"
    ) +
    # Label Umsy on plots
    geom_text(
      data = distinct(kobe_data, method, umsy),
      aes(
        label = paste0("U[MSY]==", 100*round(umsy, 2), "*\'%\'"),
        x = 1.5,
        y = 1
      ),
      colour = "grey50",
      vjust = -0.5,
      parse = TRUE
    ) +
    # Label Smsy on plots
    geom_text(
      data = distinct(kobe_data, method, smsy),
      aes(
        label = paste0("S[MSY]==", smsy),
        x = 1,
        y = 1.5
      ),
      colour = "grey50",
      vjust = -0.5,
      angle = 90,
      parse = TRUE
    ) +
    geom_path(aes(alpha = year)) + #if you want to connect the dots
    geom_point(aes(color = year), size=3) +
    # Add rectangle and text label for the 85% Smsy zone
    annotate(
      "rect", 
      xmin = 0.8, 
      xmax = 1, 
      ymin = -1, 
      ymax = 1, 
      alpha = .2
    ) +
    annotate(
      "text",
      label = "85*\'%\'~S[MSY]", 
      x = 0.9, 
      y = 0.25, 
      angle = 90,
      parse = TRUE
    ) +
    # Add text labels at first and last year points
    geom_richtext(
      data = filter(kobe_data, year== min(year)|year== max(year)),
      aes(
        label = rep(
          c(
            paste0("'", str_sub(min(kobe_data$year), 3L)), 
            paste0("'", str_sub(max(kobe_data$year), 3L)) 
          ), 
          each = 2 # Depending on year order in input data, may need to delete "each =" 
        )
      ),
      hjust = 0-.2, 
      vjust = 0-.2,
      label.colour = NA,
      fill = alpha("white", 0.75)
    ) +
    # Add red circles around first and last year data points
    geom_point(
      data = filter(kobe_data, year== min(year)|year== max(year)),
      colour = "red",
      shape = 21,
      size = 3,
      stroke = 1.25
    ) +
    # Overlay text labels describing each quadrant
    geom_richtext(
      data = quadLabs, 
      aes(
        label = label,
        hjust = hjust
      ),
      label.colour = NA,
      fill = alpha("white", 0.75)
    ) +
    # Constrain plot coordinates to spatially represent x and y equally
    coord_fixed(
      ratio = 1,
      xlim = c(0,2),
      ylim = c(0,2),
      expand = FALSE
    ) +
    scale_colour_viridis_c(
      breaks = c(1980, 2000, 2020),
      guide = guide_colorbar(
        title = "Year",
        title.hjust = 0.5,
        title.position = "top"
      )
    ) +
    scale_alpha(range = c(0.05, 0.85)) +
    guides(alpha = "none") +
    # Fancy mathy axis labels
    labs(
      y = expression(frac(Exploitation~rate, U[MSY])), 
      x = expression(frac(Spawner~abundance,S[MSY])),
      parse = TRUE
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "top",
      panel.spacing.x = unit(1.5, "lines"), # Move panels further apart
      panel.grid = element_blank()
    )
)


# Save the filled Kobe plots
kobe_plots |> 
  ggsave(
    filename = here(
      "3. R outputs",
      "Kobe plot", 
      paste0("R-PLOT_WCVI_CN_two-methods.png")
    ),
    height = 7,
    width = 12,
    units = "in",
    dpi = "print"
  )


