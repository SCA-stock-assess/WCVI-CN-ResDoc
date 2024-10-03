# Packages ----------------------------------------------------------------

pkgs <- c("here", "tidyverse")
#install.packages(pkgs)


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 12))



# Load data and plot ------------------------------------------------------


# Load from csv file
surv_data <- here(
  "Survival time series",
  "RCH_CWT_survival_data.csv"
) |> 
  read.csv() |> 
  rename(
    "Survival to age-2_CWT" = age2_surv,
    "Survival to adult return_CWT" = adult_surv,
    "Survival to adult return_OTO" = adult_surv_nat_stamp
  ) |> 
  pivot_longer(cols = !brood_year) |> 
  mutate(
    method = if_else(str_detect(name, "_CWT"), "CWT", "Otolith mark"),
    name = str_remove_all(name, "_.*")
  ) |> 
  # Keep complete brood years only
  filter(brood_year <= 2023 - 5)


# Time series plot
(surv_ts_plot <- ggplot(
  surv_data,
  aes(x = brood_year + 1, y = value)
) +
  facet_wrap(
    ~name,
    strip.position = "left",
    scales = "free_y",
    ncol = 1
  ) +
  geom_line(
    aes(lty = method),
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    y = NULL,
    x = "Ocean-entry year",
    lty = "Data source"
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(colour = "black")
  )
)


# Export the plot
ggsave(
  plot = surv_ts_plot,
  filename = here(
    "Survival time series",
    "R-PLOT_RCH_CWT_survival_time_series.png"
  ),
  height = 4, 
  width = 8,
  units = "in"
)
