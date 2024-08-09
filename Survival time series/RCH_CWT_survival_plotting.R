# Packages ----------------------------------------------------------------

pkgs <- c("here", "tidyverse")
#install.packages(pkgs)


# Load packages
library(here)
library(tidyverse); theme_set(theme_bw(base_size = 14))



# Load data and plot ------------------------------------------------------


# Load from csv file
surv_data <- here(
  "Survival time series",
  "RCH_CWT_survival_data.csv"
) |> 
  read.csv() |> 
  rename(
    "Survival to age-2" = age2_surv,
    "Survival to adult return" = adult_surv
  ) |> 
  pivot_longer(cols = !brood_year)


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
  geom_line(linewidth = 1) +
  geom_point(
    data = filter(
      surv_data,
      name == "Survival to adult return",
      brood_year > 2019
    )
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    y = NULL,
    x = "Ocean-entry year"
  ) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank()
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
