# Packages ----------------------------------------------------------------

pkgs <- c(
  'tidyverse','RColourBrewer','gganimate','ggrepel',
  'readxl', 'writexl','janitor', 'here', "ggtext"
)
#install.packages(pkgs)

library(tidyverse); theme_set(theme_light(base_size = 18))
library(ggtext)
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
  #"cypre",
  "megin",
  "moyeha",
  "nahmint",
  #"nitinat",
  "sarita",
  #"somass_stamp_sproat",
  "tranquil",
  "san_juan",
  "colonial_cayeagle",
  #"east",
  #"goodspeed",
  #"keith",
  #"klaskish",
  #"mahatta",
  #"washlawlis",
  "marble"
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
    smsy = sum(smsy_median),
    rivers = paste0(river, collapse = "; ")
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
  relocate(rivers, .after = last_col()) |> # Move "rivers" list to the end
  # Remove years with missing data
  filter(!if_any(!year, is.na)) 


# Alternate Kobe data using the equilibrium harvest rate approach
kobe_data_alt <- indicator_esc |> 
  filter(
    river %in% rivers,
    !is.na(escapement)
  ) |> 
  summarize(
    .by = year,
    escapement = sum(escapement)
  ) |> 
  left_join(rch_er) |>
  mutate(
    smsy = 18057,
    umsy = 0.56,
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


# Save the output data
writexl::write_xlsx(
  select(.data = kobe_data, -colour, -quadrant),
  here(
    "Kobe plot", 
    paste0("R-OUT_Kobe_plot_raw_data_", Sys.Date(), ".xlsx")
  )
)


# Save alternate output data
writexl::write_xlsx(
  select(.data = kobe_data_alt, -colour, -quadrant),
  here(
    "Kobe plot", 
    paste0("R-OUT_Kobe_plot_raw_data_alternate_", Sys.Date(), ".xlsx")
  )
)


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
    coord_fixed(
      ratio = 1,
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
      alpha = 0.75,
      size = 3,
      colour = ifelse(
        kobe_data$year %in% c(min(kobe_data$year), max(kobe_data$year)),
        "blue",
        alpha(kobe_data$colour, 0.75)
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
      segment.curvature = 0.03,
      segment.angle = 90,
      box.padding = 0.7,
      label.size = 0
    ) +
    scale_alpha_continuous(range = c(0.025,0.8))
)


# Save the filled Kobe plot
ggsave(
  here("Kobe plot", "R-PLOT_WCVI_CN_Nat_Kobe.png"),
  filled_kobe,
  height = 7,
  width = 7,
  units = "in"
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
                res = 150, # Resolution is in pixels-per-inch (ppi)
                renderer = gifski_renderer(), 
                end_pause = 20*10) # End pause is in frames: 20 frames * 10 = 10 seconds

anim_save(here("Kobe plot", "KOBE_ts_animation.gif"), animation = anim)



# Code from FSAR group ----------------------------------------------------


kobe_dfs <- list("RR" = kobe_data_alt, "LifeCycle" = kobe_data)


(kobe_plots <- kobe_dfs |> 
    map(
      ~ .x |> 
        ggplot(aes(x, y)) +
        #draw data and error bars on final year
        geom_path(aes(alpha = year)) + #if you want to connect the dots
        geom_point(aes(color = year), size=3) +
        # geom_errorbar(data = filter(kobe_df, year == max(kobe_df$year)),
        #               aes(x = S_Smsy, ymin = U_Umsy_LCI, ymax = U_Umsy_UCI), width = 0) +
        # geom_errorbarh(data = filter(kobe_df, year == max(kobe_df$year)),
        #                aes(y = U_Umsy, xmin = S_Smsy_LCI, xmax = S_Smsy_UCI), height = 0) +
        #add "crosshairs"
        geom_vline(xintercept = 1, lty = 2) +
        geom_hline(yintercept = 1, lty = 2) +
        # geom_vline(xintercept = 0.8, lty = 3) +
        #add labels to 80% Smsy, first and last year of data
        # annotate("text", x = 0.8, y = .4, hjust = 0,
        #          label = expression(italic(paste("80%",S)[MSY]))) +
        annotate(
          "rect", 
          xmin = 0.8, 
          xmax = 1, 
          ymin = -1, 
          ymax = 1, 
          alpha = .2
        ) +
        geom_richtext(
          data = filter(.x, year== min(.x$year)|year== max(.x$year)),
          aes(label = c("'79", "'22")), #CHANGE THESE WITH NEW DATA!
          hjust = 0-.2, 
          vjust = 0-.2,
          label.colour = NA,
          fill = alpha("white", 0.75)
        ) +
        geom_point(
          data = filter(.x, year== min(.x$year)|year== max(.x$year)),
          colour = "red",
          shape = 21,
          size = 3
        ) +
        geom_text(
          data = quadLabs, 
          aes(
            label = label,
            hjust = hjust
          )
        ) +
        coord_fixed(
          ratio = 1,
          xlim = c(0,2),
          ylim = c(0,2),
          expand = FALSE
        ) +
        scale_colour_viridis_c(name="Year") +
        scale_alpha(range = c(0.05, 0.9)) +
        guides(alpha = "none") +
        labs(y="U/Umsy", x= "S/Smsy") +
        theme_bw(base_size = 12) +
        theme(
          legend.position = c(0.75, 0.65),
          legend.background = element_rect(colour = "black")
        )
    )
)


# Save the filled Kobe plots
kobe_plots |> 
  iwalk(
    ~ ggsave(
      filename = here(
        "Kobe plot", 
        paste0("R-PLOT_WCVI_CN_", .y, ".png")
      ),
      plot = .x,
      height = 7,
      width = 7,
      units = "in",
      dpi = "print"
    )
  )

