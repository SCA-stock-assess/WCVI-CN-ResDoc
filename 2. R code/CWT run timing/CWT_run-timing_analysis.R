# Packages ----------------------------------------------------------------

pkgs <- c("readxl", "here", "tidyverse", "janitor", "ggridges")
#install.packages(pkgs)

# Load packages
library(readxl); library(here); library(tidyverse); library(janitor); library(ggridges)



# Load CWT data -----------------------------------------------------------


# CWT recovery data
recoveries <- list.files(
  here("1. data files"),
  pattern = "(?i)cwt_run_timing",
  full.names = TRUE
) |> 
  read.csv(na.strings = c("")) |> 
  clean_names() |> 
  filter(
    rc_mrp_area_code %in% c("025", "125"),
    str_detect(rc_sport_site_name, "(?i)bajo|beano|ferrer|maquinna"),
    rl_stock_prod_area_cdfo %in% c("NWVI", "SWVI")
  ) |> 
  mutate(
    rc_recovery_date = as.Date(rc_recovery_date, format = "%Y/%m/%d"),
    doy = format(rc_recovery_date, "%j") |> as.numeric(),
    date = as.Date(paste(2015, rc_recovery_month, day, sep = "-"))
  )


# Calculate 50% date
dates_50 <- recoveries |> 
  uncount(sum_rc_observed_number) |> 
  summarize(
    .by = rl_stock_prod_area_cdfo,
    date_50 = quantile(date, 0.5, type = 1)
  ) |> 
  mutate(
    date_50 = format(date_50, "%d-%b"),
    label = paste("50% date:", date_50)
    )


# Plot(s) -----------------------------------------------------------------


# Ridgeline plot
(ridges <- recoveries |> 
  uncount(sum_rc_observed_number) |> 
  ggplot(aes(date, fct_rev(rl_stock_prod_area_cdfo))) +
  geom_density_ridges(
    alpha = 0.5,
    scale = 0.95,
    quantile_lines = TRUE,
    quantiles = 2,
    vline_colour = "red",
    vline_width = 1
  ) +
  geom_text(
    data = dates_50,
    aes(
      x = min(recoveries$date), 
      label = label
    ),
    vjust = -6,
    hjust = 0.5,
    size = 6
  ) +
  scale_x_date() +
  scale_y_discrete(expand = expansion(mult = c(0, 1.05))) +
  labs(
    y = "CWT stock",
    x = "Date"
  ) +
  theme_ridges()
)

  
# Save ridgeline plot
ggsave(
  ridges,
  filename = here(
    "3. R outputs",
    "CWT run timing",
    "R-PLOT_CWT_run_timing_WVI_ridges.png"
  ),
  height = 5,
  width = 8, units = "in"
)
