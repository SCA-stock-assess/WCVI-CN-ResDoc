# Packages ----------------------------------------------------------------

pkgs <- c(
  'tidyverse','RColourBrewer','gganimate','ggrepel',
  'readxl', 'writexl','janitor', 'here', "ggtext", "gsl"
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
library(gsl)


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


# Reference points data set for individual rivers
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
  filter(indicator %in% c("Ind10", "Ind7")) |> 
  # Summarize reference points across indicators to get SMU aggregate values
  summarize(
    smsy = sum(lc_smsy_median),
    srep = sum(lc_srep_median)
  ) |> 
  # Calculate Umsy using explicit solution from Scheuerell (2016)
  mutate(umsy = 1 - gsl::lambert_W0(exp(1-1)))
  

# Reference points at SMU level from equilibrium trade-off analysis
rp_eq <- here(
  "3. R outputs",
  "Equilibrium trade-off analysis",
  "R-OUT_SMU_ref-pt_values_eq-trade-off.csv"
) |> 
  read.csv()


# Time series of indicator escapement from infilling
infilled_esc <- here(
  "3. R outputs",
  "Escapement infill",
  "R-OUT_infilled_indicators_escapement_timeseries.csv"
) |> 
  read.csv()


# Make dataframe for KOBE plot
kobe_data <- infilled_esc |> 
  # Sum escapements across indicators for each year, and add the overall S/Umsy values
  summarize(
    .by = year,
    escapement = sum(escapement),
    umsy_lc = indicator_rp$umsy, # Life cycle method
    smsy_lc = indicator_rp$smsy, # Life cycle method
    umsy_rr = rp_eq$mid[rp_eq$variable == "Umsy"], # Equilibrium trade-off method
    smsy_rr = rp_eq$mid[rp_eq$variable == "Smsy"] # Equilibrium trade-off method
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
        label = paste0("S[MSY]==", round(smsy, 0)),
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


