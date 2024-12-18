# Load packages -----------------------------------------------------------

pkgs <- c("tidyverse", "here", "readxl", "janitor")
#install.packages(pkgs)

library(here)
library(tidyverse); theme_set(theme_bw(base_size = 18))
library(readxl)


# Load OUT file and filter/clean ------------------------------------------


# Read brood-year-based data
out_data <- list.files(
  here("1. data files"),
  pattern = "OUT File Export",
  full.names = TRUE
) |> 
  read_xlsx(sheet = "BROOD YEAR METHOD") |> 
  janitor::clean_names() |> 
  filter(
    stock_code == "RBT",
    complete_br_yr == "Y",
    cohort_size_anm > 30
  )


# Recent 12 year average maturation rates 
out_data |> 
  filter(max(brood_year) - brood_year < 13) |> 
  group_by(age) |> 
  summarize(avg_mat_rate = mean(mat_rate))



# Plot --------------------------------------------------------------------


# Maturation rates
(mat_plot <- out_data |> 
   mutate(age = as.factor(age)) |> 
   ggplot(aes(
     x = brood_year, 
     y = mat_rate, 
     colour = fct_rev(age)
   )
   ) +
   geom_point(aes(size = cohort_size_anm)) +
   geom_path(aes(group = age), lty = 2) +
   scale_colour_viridis_d(
     option = "mako", 
     end = 0.9
   ) +
   scale_y_continuous(labels = scales::percent) +
   scale_size_continuous(breaks = seq(5e3, 2e4, by = 5e3)) +
   coord_cartesian(expand = FALSE, clip = "off") +
   labs(
     y = "Maturation rate",
     x = "Brood year",
     colour = "Age\n(years)",
     size = "Estimated CWTs"
   ) +
   theme_minimal()
)

# Add regression lines
mat_plot +
   geom_smooth(
     aes(weight = cohort_size_anm),
     method = "lm"
   ) 
  

# Save the plot without regression lines
ggsave(
  mat_plot,
  filename = here(
    "3. R outputs",
    "Demographic data",
    "R-plot_RBT-CN_mat-rates_BY.png"
  ),
  height = 5,
  width = 7,
  units = "in"
)
