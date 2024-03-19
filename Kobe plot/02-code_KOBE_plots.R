# Packages ----------------------------------------------------------------

pkgs <- c('tidyverse','RColourBrewer','gganimate','ggrepel','readxl','janitor', 'here')
#install.packages(pkgs)
#devtools::install_github("thomasp85/transformr") # transformr package required for gganimate

library(tidyverse); theme_set(theme_light(base_size = 20))
library(janitor)
library(RColorBrewer)
library(gganimate)
library(ggrepel)
library(readxl)
library(here)


# Load and tidy the data --------------------------------------------------


# Escapement time series
indicator_esc <- read_xlsx(
  here("Kobe plot", "01-data_HOLT SR parameters KOBE PLOT.xlsx"),
  sheet = "Raw KOBE data"
  ) |> 
  clean_names() |> 
  select(1:17) |> 
  pivot_longer(
    2:last_col(),
    names_to = "river",
    values_to = "escapement"
  )


# Estimated Robertson CN exploitation rate time series
rch_er <- read_xlsx(
  here("Kobe plot", "01-data_HOLT SR parameters KOBE PLOT.xlsx"),
  sheet = "Raw KOBE data"
) |> 
  clean_names() |> 
  select(year, contains("non_terminal_er")) |> 
  rename("er" = 2)


# Reference points data set
indicator_rp <- read_xlsx(
  here("Kobe plot", "01-data_HOLT SR parameters KOBE PLOT.xlsx"),
  sheet = "Raw Holt RP data"
) |> 
  clean_names() |> 
  # Add SR parameters
  mutate(
    smsy_divide_srep = smsy_median/srep_median,
    alpha = exp((0.5-smsy_divide_srep)/0.07),
    beta = log(alpha)/srep_median,
    umsy = (0.5*log(alpha)) - (0.07*(log(alpha))^2),
    recruits_at_0.85smsy = alpha*0.85*smsy_median*(exp(-beta*0.85*smsy_median))
  ) |> 
  # Clean up river names
  mutate(river = make_clean_names(river))


# Select rivers to include in KOBE plot
rivers <- c(
  "artlish",
  "burman",
  #"conuma",
  "gold",
  "kaouk",
  "leiner",
  "tahsis",
  "tahsish",
  "zeballos",
  "bedwell",
  "cypre",
  "megin",
  "moyeha",
  "nahmint",
  #"nitinat",
  "sarita",
  #"somass_stamp_sproat",
  "tranquil",
  "san_juan",
  "colonial_cayeagle",
  "east",
  "goodspeed",
  "keith",
  "klaskish",
  "mahatta",
  "marble",
  "washlawlis"
)


# Make dataframe for KOBE plot
kobe_data <- indicator_esc |> 
  filter(
    river %in% rivers,
    !is.na(escapement)
  ) |> 
  left_join(select(.data = indicator_rp, river, umsy, smsy_median)) |> 
  summarize(
    .by = year,
    escapement = sum(escapement),
    umsy = mean(umsy),
    smsy = sum(smsy_median)
  ) |> 
  left_join(rch_er) |> 
  mutate(
    y = er/umsy,
    x = escapement/smsy,
    quadrant = case_when(
      y >= 1 & x <= 1 ~ 1,
      y >= 1 & x > 1 ~ 5, # Nothing falls in this zone
      y < 1 & between(x,0.8,1) ~ 2, # Amber zone
      y < 1 & x <= 0.8 ~ 3,
      y < 1 & x > 1 ~ 4
    ) %>% factor(),
    colour = case_when(
      quadrant == 1 ~ "firebrick2",
      quadrant == 2 ~ "darkgoldenrod1",
      quadrant == 3 ~ "darkorange2",
      quadrant == 4 ~ "springgreen3",
      quadrant == 5 ~ "darkgoldenrod1" # Nothing falls in this quadrant
    )
  ) |> 
  # Remove years with missing data
  filter(!if_any(!year, is.na)) 


# Plots -------------------------------------------------------------------


# Dataframe for quadrant labels
quadLabs <- data.frame(
  label = c(
    "Overfishing",
    "Overfishing but spawning\nbiomass sufficient",
    "Fisheries reduced to\nallow rebuilding",
    "Sustainable fishery"
  ),
  x = c(0.1, 1.9, 0.1, 1.9),
  y = c(1.85, 1.85, 0.15, 0.15),
  hjust = c(0, 1, 0, 1)
) 


# Base KOBE plot
(base_kobe <- kobe_data %>% 
    arrange(year) %>% 
    ggplot(aes(x, y)) +
    geom_hline(
      yintercept = 1,
      lty = 2,
      colour = "grey70"
    ) +
    geom_vline(
      xintercept = 1, 
      lty = 2, 
      colour = "grey70"
    ) +
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
      label = "Amber zone", 
      x = 0.9, 
      y = 0.25, 
      angle = 90
    ) +
    geom_text(
      data = quadLabs, 
      aes(
        label = label,
        hjust = hjust
      )
    ) +
    coord_cartesian(
      xlim = c(0,2),
      ylim = c(0,2),
      expand = FALSE
    ) +
    scale_size_continuous(labels = scales::comma) +
    guides(
      colour = "none",
      alpha = "none"
    ) +
    labs(
      x = "Spawner abundance / Spawner abundance at MSY",
      y = "Fishery ER / ER at MSY",
      size = "Spawner\nabundance"
    ) +
    theme(
      legend.position = c(0.65,0.7),
      legend.background = element_rect(colour="black"),
      panel.grid = element_blank()
    )
)


# Add points and path
(filled_kobe <- base_kobe +
  geom_path(aes(alpha = year)) +
  geom_point(
    shape = 21,
    fill = kobe_data$colour,
    size = 3,
     colour = ifelse(
       kobe_data$year %in% c(min(kobe_data$year), max(kobe_data$year)),
       "blue",
       kobe_data$colour
     )
  ) +
  geom_label_repel(
    data = filter(kobe_data, year %in% c(min(year), max(year))),
    aes(label = year),
    colour = "blue",
    fill = alpha("white", alpha = 0.75),
    size = 5,
    min.segment.length = 0,
    segment.colour = "blue",
    segment.size = 0.75,
    box.padding = 0.5,
    label.size = 0
  ) +
  scale_alpha_continuous(range = c(0.025,0.8))
)


# Animated version
(animated_kobe <- base_kobe +
    geom_path(aes(alpha = year)) +
    geom_point(
      colour = kobe_data$colour,
      size = 3,
    ) +
    scale_alpha_continuous(range = c(0.025,0.8)) +
    transition_reveal(along = year, keep_last = T) +
    labs(subtitle = 'Year: {frame_along}') 
)

# Animate it
anim <- animate(animated_kobe, duration = 60, fps = 20, 
                width = 13.3333, height = 7.5, units = "in", 
                res = 150, #Resolution is in pixels-per-inch (ppi)
                renderer = gifski_renderer(), 
                end_pause = 20*10) # End pause is in frames: 20 frames * 10 = 10 seconds

anim_save(here("Kobe plot", "KOBE_ts_animation.gif"), animation = anim)

