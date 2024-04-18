# Packages ----------------------------------------------------------------

pkgs <- c("readxl", "tidyverse", "ggpubr", "here")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 8))
library(readxl)
library(ggpubr)
library(ggtext)



# Import data -------------------------------------------------------------


data <- here("FSAR 4-panel plot", "FSAR_4-panel_data.xlsx") |> 
  read_xlsx() |> 
  mutate(ocean_er = ocean_er/100) |> 
  pivot_longer(!year) %>%
  split(.$name) |> 
  map(~spline(.x$year, .x$value)) |> 
  map(as.data.frame) |> 
  list_rbind(names_to = "name") |> 
  bind_rows(
    tibble(
      x = c(NA, 1977:2023), 
      name = "recruitment", 
      y = c(1, rep(NA, length(1977:2023)))
    )
  ) |> 
  mutate(
    y = if_else(name == "ocean_catch", y/1000, y),
    name = case_when(
      name == "ocean_er" ~ "Exploitation rate in ocean fisheries",
      name == "ocean_catch" ~ "Catch in ocean fisheries (1000s)",
      name == "nat_escapement" ~ "Escapement of natural indicator rivers",
      name == "recruitment" ~ "Recruitment"
    )
  )



# Build plots -------------------------------------------------------------


# Make one plot for each variable
plots <- data |> 
  nest(.by = name) |> 
  rowwise() |> 
  mutate(
    plot = list(
      ggplot(data, aes(x = x, y = y)) +
        geom_line(linewidth = 0.75) +
        scale_y_continuous(expand = expansion(mult = (c(0, 0.07)))) +
        coord_cartesian(ylim = c(0, max(data$y))) +
        labs(
          x = NULL,
          y = name
        ) +
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
    geom_hline(
      yintercept = c(0.56, 0.44),
      colour = c("red", "blue"),
      lty = 2,
      linewidth = 0.75
    ) +
    annotate(
      "richtext",
      x = 1995,
      y = c(0.565, 0.445),
      vjust = 0,
      hjust = 0,
      label = c(
        "*U*<sub>MSY</sub> from equilibrium trade-off",
        "*U*<sub>MSY</sub> from individual population benchmarks"
      ),
      colour = c("red", "blue"),
      label.colour = NA,
      fill = alpha("white", alpha = 0.8),
      size = 2.5
    )
)


(plots[[1]] <- plots[[1]] +
    geom_hline(
      yintercept = c(18939, 13869)*0.8,
      colour = c("red", "blue"),
      lty = 2,
      linewidth = 0.75
    ) +
    annotate(
      "richtext",
      x = 1977,
      y = c(18950, 13910)*0.8,
      vjust = 0,
      hjust = 0,
      label = c(
        "80% *S*<sub>MSY</sub> from equilibrium trade-off",
        "80% *S*<sub>MSY</sub> from individual population benchmarks"
      ),
      colour = c("red", "blue"),
      label.colour = NA,
      fill = alpha("white", alpha = 0.8),
      size = 2.5
    )
)


# Arrange plots in a 4-panel grid
(four_panel_plot <- ggarrange(
  plotlist = list(
    plots[["Catch in ocean fisheries (1000s)"]],
    plots[["Escapement of natural indicator rivers"]],
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
  filename = here("FSAR 4-panel plot", "R-PLOT_4-panel_plot_WCVI_CN.png"),
  height = 5,
  width = 7,
  units = "in",
  dpi = "print"
)

