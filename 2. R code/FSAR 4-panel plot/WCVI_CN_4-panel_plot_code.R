# Packages ----------------------------------------------------------------

pkgs <- c("readxl", "tidyverse", "ggpubr", "here")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 8))
library(readxl)
library(ggpubr)
library(ggtext)



# Import data -------------------------------------------------------------


data <- here(
  "1. data files", 
  "FSAR_4-panel_data.xlsx"
) |> 
  read_xlsx() |> 
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
    name = case_when(
      name == "ocean_er" ~ "Exploitation rate in ocean fisheries",
      name == "ocean_catch" ~ "Catch in ocean fisheries (1000s)",
      name == "nat_escapement" ~ "Aggregate spawner abundance index",
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
      yintercept = c(0.44),
      colour = c("blue"),
      lty = 2,
      linewidth = 0.5
    ) +
    # Add inter-quartile confidence band around Umsy 
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = 0.30,
      ymax = 0.54,
      fill = "blue",
      alpha = 0.15
    ) +
    annotate(
      "richtext",
      x = max(data$x, na.rm = TRUE),
      y = c(
        #0.565, 
        0.43
      ),
      vjust = 0,
      hjust = 1,
      label = c(
        #"*U*<sub>MSY</sub> based on moderate productivity",
        "*U*<sub>MSY</sub> based on low productivity"
      ),
      colour = c("blue"),
      label.colour = NA,
      fill = NA,
      size = 2.5
    )
)


(plots[[1]] <- plots[[1]] +
    geom_hline(
      yintercept = c(19107*0.85, 8493),
      colour = c("blue", "red"),
      lty = 2,
      linewidth = 0.5
    ) +
    annotate(
      "richtext",
      x = min(data$x, na.rm = TRUE),
      y = c(19107*0.85, 8493)+250,
      vjust = 0,
      hjust = 0,
      label = c(
        "85% *S*<sub>MSY</sub> based on low productivity",
        "*S*<sub>gen</sub> based on low productivity"
      ),
      colour = c("blue", "red"),
      label.colour = NA,
      fill = alpha("white", alpha = 0.8),
      size = 2.5
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

