# Packages ----------------------------------------------------------------

pkgs <- c("here", "tidyverse", "readxl", "janitor")
#install.packages(pkgs)


# Load packages
library(here); library(tidyverse); library(readxl); library(janitor)




# Load and plot data ------------------------------------------------------


# File is stored under Kobe plot folder
full_ts <- here(
  "Kobe plot",
  "01-data_FWA table update version 4 April17.xlsx"
) |> 
  read_xlsx(
    sheet = "RiverEscData (2)",
    skip = 2
  ) |> 
  select(1:Spawners) |> 
  clean_names() |> 
  # Add qualitative assignments for data collection era
  mutate(
    group = if_else(str_detect(watershed, "(?i)somass"), "Somass", "Other") |> 
      factor(levels = c("Somass", "Other")),
    era = case_when(
      brood_year < 1995 & group != "Somass" ~ "Fishery officer counts",
      brood_year >= 1995 & group != "Somass" ~ "Formal survey",
      brood_year < 1985 & group == "Somass" ~ "Fishery officer counts",
      brood_year >= 1985 & group == "Somass" ~ "Formal survey" 
    ),
    indicator = factor(
      indicator, 
      levels = c(
        "Esc", 
        "Ind7", 
        "Ind17", 
        "Hatchery"
      ),
      labels = c(
        "Extensive indicators",
        "7 wild indicators",
        "10 intensive indicators",
        "SEP major operations systems"
      )
    )
  )


# Small dataframe for the rectangles
eras <- full_ts |> 
  mutate(
    .by = c(brood_year, group),
    agg_spawners = sum(spawners, na.rm = TRUE)
  ) |> 
  mutate(
    .by = group,
    agg_spawners = max(agg_spawners)
  ) |> 
  summarize(
    .by = c(era, group),
    xmin = min(brood_year),
    xmax = max(brood_year),
    y = max(agg_spawners)/2
  ) |> 
  mutate(x = xmin + (xmax-xmin)/2) |> 
  filter(!era == "Formal survey")


# Plot
ggplot(
  full_ts,
  aes(x = brood_year, y = spawners)
) +
  facet_grid(
    group~.,
    #scales = "free_y",
    #space = "free"
  ) +
  geom_rect(
    data = eras,
    aes(
      x = xmin,
      y = 0,
      ymin = -Inf,
      ymax = Inf,
      xmin = xmin-2,
      xmax = xmax + 0.5
    ),
    fill = "grey75",
    colour = NA,
    alpha = 0.5
  ) +
  geom_col(aes(fill = fct_rev(indicator))) +
  geom_text(
    data = eras,
    aes(
      x = x, 
      #y = y,
      y = 60000,
      label = era
    ),
    size = 6,
    colour = "grey60"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::comma
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  coord_cartesian(xlim = c(min(full_ts$brood_year), max(full_ts$brood_year))) +
  scale_fill_viridis_d(option = "mako", end = 0.9) +
  labs(
    x = "Survey year",
    y = "Estimated spawner abundance",
    fill = "Population category"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    #strip.text = element_blank(),
    strip.background = element_rect(fill = "white"),
    legend.background = element_rect(colour = "black")
  )


# Alternative plot where alpha shows fishery officer counts
(ts_alpha <- ggplot(
  full_ts,
  aes(x = brood_year, y = spawners)
) +
  geom_col(
    aes(
      fill = fct_rev(indicator),
      alpha = fct_rev(era)
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::comma
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  coord_cartesian(xlim = c(min(full_ts$brood_year), max(full_ts$brood_year))) +
  scale_fill_viridis_d(option = "mako", end = 0.9) +
  scale_alpha_discrete(range = c(1, 0.60)) +
  guides(alpha = "none") +
  labs(
    x = "Survey year",
    y = "Estimated spawner abundance",
    fill = "Population category"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    #strip.text = element_blank(),
    strip.background = element_rect(fill = "white"),
    legend.background = element_rect(colour = "black")
  )
)


# Save the shaded plot 
ggsave(
  ts_alpha,
  filename = here(
    "Full abundance time series (incl BC-16s)",
    "R-PLOT_full_abundance_ts_alpha.png"
  ),
  height = 4, 
  width = 9,
  units = "in"
)

