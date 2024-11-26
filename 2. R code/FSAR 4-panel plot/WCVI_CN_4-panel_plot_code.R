# Packages ----------------------------------------------------------------

pkgs <- c("readxl", "tidyverse", "ggpubr", "here", "janitor", "geomtextpath")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 8))
library(geomtextpath)
library(janitor)
library(readxl)
library(ggpubr)
library(ggtext)



# Import data -------------------------------------------------------------


# Escapement data from infilling method
esc_time_series <- here(
  "3. R outputs", 
  "Escapement infill", 
  "R-OUT_infilled_indicators_escapement_timeseries.csv"
) |> 
  read.csv() |> 
  mutate(
    core = if_else(
      str_detect(river, "(?i)bedwell|megin|moyeha|artlsih|kaouk|tahsish|marble"),
      1,
      0
    ) 
  ) 


# Summarize data across all 17 indicators, and across only the core 7
esc_summary <- esc_time_series |> 
  summarize(
    .by = year,
    nat_escapement = sum(escapement),
    nat_escapement_core = sum(escapement[core == 1])
  )


# Read the data file with ocean harvest and ER data, then merge
# escapement data from the infill process
data <- here(
  "1. data files", 
  "FSAR_4-panel_data.xlsx"
) |> 
  read_xlsx() |> 
  left_join(esc_summary) |> 
  filter(!if_any(everything(), is.na)) |> 
  pivot_longer(!year) %>%
  split(.$name) |> 
  map(~spline(.x$year, .x$value)) |> 
  map(as.data.frame) |> 
  list_rbind(names_to = "name") %>%
  bind_rows(
    tibble(
      x = c(NA, seq.int(min(.$x), max(.$x))), 
      name = "recruitment", 
      y = c(1, rep(NA, length(seq.int(min(.$x), max(.$x)))))
    )
  ) |> 
  mutate(
    y = if_else(name == "ocean_catch", y/1000, y),
    group = if_else(name == "nat_escapement_core", "core", "all"),
    name = case_when(
      name == "ocean_er" ~ "Exploitation rate in ocean fisheries",
      name == "ocean_catch" ~ "Catch in ocean fisheries (1000s)",
      str_detect(name, "nat_escapement") ~ "Aggregate spawner abundance index",
      name == "recruitment" ~ "Recruitment"
    )
  )


# Biological benchmark data
bb <- here("1. data files", "WCVI_CN_streams_master_list.xlsx") |> 
  read_xlsx() |> 
  clean_names() |> 
  filter(
    str_detect(
      mainstem,
      "(?i)artlish|bedwell|burman|cayeghle|gold|kaouk|leiner|marble|megin|
      moyeha|nahmint|sarita|san.*juan|tahsis|tahsish|tranquil|zeballos"
    )
  )


# Biological benchmarks for indicator populations
Umsy <- 0.43
Umsy_lci <- 0.30
Umsy_uci <- 0.54

Smsy_17 <- bb |> 
  pull(smsy_lc) |> 
  sum()

Sgen_17 <- bb |> 
  pull(sgen_lc) |> 
  sum()

Smsy_7 <- bb |> 
  filter(str_detect(mainstem, "(?i)bedwell|megin|moyeha|artlsih|kaouk|tahsish|marble")) |> 
  pull(smsy_lc) |> 
  sum()

Sgen_7 <- bb |> 
  filter(str_detect(mainstem, "(?i)bedwell|megin|moyeha|artlsih|kaouk|tahsish|marble")) |> 
  pull(sgen_lc) |> 
  sum()



# Build plots -------------------------------------------------------------


# Make one plot for each variable
plots <- data |> 
  nest(.by = name) |> 
  rowwise() |> 
  mutate(
    plot = list(
      ggplot(
        data, 
        aes(
          x = x, 
          y = y, 
          colour = group
        )
      ) +
        geom_line(linewidth = 0.75) +
        scale_y_continuous(expand = expansion(mult = (c(0, 0.07)))) +
        scale_colour_manual(values = c("black", "grey70")) +
        coord_cartesian(ylim = c(0, max(data$y))) +
        labs(
          x = NULL,
          y = name
        ) +
        guides(colour = "none") +
        theme(
          axis.title = element_text(size = 9),
          panel.grid = element_blank()
        ) 
    )
  ) |> 
  pull(plot) |> 
  set_names(nm = unique(data$name))


# Make specific adjustments to each panel
(plots[[3]] <- plots[[3]] +
    scale_y_continuous(labels = scales::percent) +
    # Add labelled horizontal reference line for Umsy
    geom_labelhline(
      yintercept = Umsy,
      label = "*U*<sub>MSY</sub> based on low productivity",
      hjust = 0.1,
      rich = TRUE,
      colour = "black",
      fill = "white",
      boxcolor = NA,
      alpha = 0.8,
      lty = 2,
      linewidth = 0.4,
      size = 2.5,
      label.padding = unit(0.05, "lines")
    ) +
    # Add inter-quartile confidence band around Umsy 
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = Umsy_lci,
      ymax = Umsy_uci,
      fill = "black",
      alpha = 0.10
    ) 
)


(plots[[1]] <- plots[[1]] +
    geom_labelhline(
      yintercept = Smsy_17*0.85,
      hjust = 0.1,
      colour = "black",
      label = "85% *S*<sub>MSY</sub> based on low productivity",
      rich = TRUE,
      fill = "white",
      boxcolor = NA,
      alpha = 0.8,
      lty = 2,
      linewidth = 0.4,
      size = 2.5,
      label.padding = unit(0.05, "lines")
    ) +
    geom_labelhline(
      yintercept = Sgen_17,
      hjust = 0.1,
      colour = "black",
      label = "*S*<sub>gen</sub> based on low productivity",
      rich = TRUE,
      fill = "white",
      boxcolor = NA,
      alpha = 0.8,
      lty = 2,
      linewidth = 0.4,
      size = 2.5,
      label.padding = unit(0.05, "lines")
    ) +
    geom_labelhline(
      yintercept = Smsy_7*0.85,
      hjust = 0.1,
      colour = "grey50",
      label = "85% *S*<sub>MSY</sub> based on low productivity",
      rich = TRUE,
      fill = "white",
      boxcolor = NA,
      alpha = 0.8,
      lty = 2,
      linewidth = 0.3,
      size = 1.5,
      label.padding = unit(0.05, "lines")
    ) +
    geom_labelhline(
      yintercept = Sgen_7,
      hjust = 0.1,
      colour = "grey50",
      label = "*S*<sub>gen</sub> based on low productivity",
      rich = TRUE,
      fill = "white",
      boxcolor = NA,
      alpha = 0.8,
      lty = 2,
      linewidth = 0.3,
      size = 1.5,
      label.padding = unit(0.05, "lines")
    ) 
)


# Arrange plots in a 4-panel grid
(four_panel_plot <- ggarrange(
  plotlist = list(
    plots[["Catch in ocean fisheries (1000s)"]],
    plots[["Aggregate spawner abundance index"]],
    plots[["Exploitation rate in ocean fisheries"]],
    plots[["Recruitment"]]
  ), 
  nrow = 2, 
  ncol = 2, 
  align = "hv", 
  common.legend = TRUE,
  labels = "auto",
  label.x = 0.87,
  label.y = 0.97
)
)


# Save the 4-panel grid plot
ggsave(
  plot = four_panel_plot,
  filename = here(
    "3. R outputs",
    "FSAR 4-panel plot", 
    "R-PLOT_4-panel_plot_WCVI_CN.png"
  ),
  height = 5,
  width = 7,
  units = "in",
  dpi = "print"
)

