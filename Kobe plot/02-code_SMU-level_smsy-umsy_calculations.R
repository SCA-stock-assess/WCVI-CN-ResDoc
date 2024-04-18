# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "here", "readxl", "janitor")
#install.packages(pkgs)

library(here)
library(tidyverse)
library(readxl)
library(janitor)


# Productivity distributions for each CU ----------------------------------


# Values from Holt et al 2023 
# (https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41195255.pdf)
# Table 10 (Page 56)
productivity <- tribble(
  ~cu, ~mean_log_a, ~srep,
  "SWVI", 1.14, 15180,
  "NWVI", 1.58, 3350,
  "NoKy", 1.53, 26325
) |> 
  mutate(
    sd_log_a = 0.5,
    alpha = exp(mean_log_a),
    beta = mean_log_a/srep
  )


# Define functions for calculating Seq and Heq ----------------------------


# Seq - equilibrium population escapement at fixed harvest rate
Seq <- function(Smsy, Umsy, Ueq) {
  
  numerator <- Umsy - log((1-Umsy)/(1-Ueq))
  
  Seq = Smsy * (numerator/Umsy)
  
    return(Seq)
}


# Heq - equilibrium harvest 
Heq <- function(Ueq, Seq) {
  Heq = (Ueq*Seq)/(1-Ueq)
  
  return(Heq)
}


# Versions from Carrie Holt's email
Seq_holt <- function(alpha, beta, U) {
  
  Seq = (alpha - (-log(1-U)))/beta
  
  return(Seq) # Don't allow Seq < 0
}

Heq_holt <- function(alpha, beta, U) {
  
  Seq = (alpha - (-log(1-U)))/beta
  
  Heq = Seq * exp(alpha-beta*Seq)-Seq
  
  return(Heq)
}


# Simple example with Carrie's method -------------------------------------


# Define range of harvest rates
Ueq <- tibble(Ueq = seq(0, 0.95, by = 0.05))


# Get data summarized by CU
holt_params <- productivity |> 
  select(1:3) |> 
  rowwise() |> 
  mutate(
    data = list(
      rnorm(10000, mean = mean_log_a, sd = 0.5) |> 
        as_tibble_col(column_name = "log_a") |>  
        filter(log_a > 0) |> 
        mutate(beta = log_a/srep) |> 
        crossing(Ueq) |> 
        mutate(
          Seq = Seq_holt(alpha = log_a, beta = beta, U = Ueq),
          Heq = Heq_holt(alpha = log_a, beta = beta, U = Ueq),
          #across(Seq:Heq, ~if_else(.x < 0, 0, .x))
        )
    )  
  )


# Calculate CU-level Umsy
umsy_vals <- productivity |> 
  mutate(umsy = beta * ((mean_log_a*(0.5-0.07*mean_log_a))/beta)) |> 
  pull(umsy)


# Unpack and summarize data
eq_sum_data <- holt_params |> 
  unnest(data) |> 
  #filter(!if_any(Seq:Heq, ~(is.na(.x) | is.infinite(.x)))) |> 
  mutate(
    .by = cu,
    id = row_number(),
    across(Seq:Heq, ~if_else(.x < 0, 0, .x))
  ) |> 
  summarize(
    .by = c(id, Ueq),
    across(Seq:Heq, sum)
  ) |> 
  summarize(
    .by = Ueq,
    Seq = median(Seq),
    Heq_mid = median(Heq),
    Heq_lwr = quantile(Heq, 0.25),
    Heq_upr = quantile(Heq, 0.75)
  ) |> 
  mutate(
    umsy = list(umsy_vals),
    num_cu_exceed_umsy = map2(
      .x = Ueq,
      .y = umsy,
      ~ifelse(.x > .y, 1, 0)
    ) |> 
      map(sum) |> 
      unlist()
  )

  
# Calculate secondary y axis transformation
ratio <- max(eq_sum_data$Heq_upr)/max(eq_sum_data$num_cu_exceed_umsy)


# label Smsy
smsy_lab <- tibble(
  Heq_mid = max(eq_sum_data$Heq_mid),
  label = eq_sum_data$Seq[eq_sum_data$Heq_mid == max(eq_sum_data$Heq_mid)]
)


# Plot summarized data
(eq_plot <- ggplot(eq_sum_data, aes(x = Seq, y = Heq_mid)) +
  geom_line(
    colour = "blue",
    linewidth = 1
  ) +
  geom_ribbon(
    aes(ymin = Heq_lwr, ymax = Heq_upr),
    fill = "blue",
    alpha = 0.3
  ) +
  geom_line(
    aes(y = num_cu_exceed_umsy*ratio),
    colour = "red",
    linewidth = 1
  ) +
  annotate(
    "segment",
    x = smsy_lab$label,
    y = 0, 
    yend = smsy_lab$Heq_mid,
    lty = 2,
    colour = "grey50",
    linewidth = 0.75
  ) +
  annotate(
    "text",
    x = smsy_lab$label + 1e3,
    y = 1000,
    colour = "grey50",
    label = paste0("Seq = ", round(smsy_lab$label, 0)),
    angle = 270,
    hjust = 1
  ) +
  scale_x_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      transform = ~1-(./max(.)),
      labels = scales::percent,
      name = "Aggregate equilibrium ER",
      breaks = seq(0, 0.95, by = 0.2)
    )
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(
      transform = ~./ratio,
      name = "Number of CUs where agg. ER > Umsy"
    )
  ) +
  labs(
    y = "Aggregate equilibrium harvest",
    x = "Aggregate equilibrium spawners"
  ) +
  theme_bw()
)


# Save plot
ggsave(
  eq_plot,
  filename = here("Kobe plot", "R-PLOT_equilibrium_harvest_curves.png"),
  height = 5,
  width = 7, units = "in"
)


# Old stuff probably no longer needed -------------------------------------


# Using RPs from 3 different methods
rp_data <- list.files(
  here("Kobe plot"),
  pattern = "(?i)bootstrappedRPs",
  full.names = TRUE
) |> 
  read_excel(sheet = "wcviCK-BootstrappedRPs_ExtInd_R", range = cell_cols("B:J")) |> 
  clean_names() |> 
  filter(!str_detect(stock, "\\*")) |> # Remove rows flagged with asterisks (follow up w/Wilf on these)
  pivot_wider(
    names_from = rp,
    values_from = value:upr,
    names_sep = "_"
  ) |> 
  rename_with(~str_to_lower(str_replace(.x, "value_", "median_"))) |> 
  left_join(productivity) |> 
  mutate(
    #beta = mean_log_a/median_srep,
    umsy = beta*median_smsy,
    recruits_at_0.85smsy = alpha*0.85*median_smsy*(exp(-beta*0.85*median_smsy))
  ) |> 
  # Clean up river names
  mutate(stock = make_clean_names(stock, allow_dupes = TRUE))


# Are any stocks duplicated across methods?
rp_data |> 
  filter(
    .by = c(stock, method),
    n() > 1
  )



# Add harvest rates to data and execute calculations
eq_data <- rp_data |> 
  crossing(Ueq) |> 
  rowwise() |> 
  mutate(
    across(
      c(median_smsy, lwr_smsy, upr_smsy), 
      ~Seq(Smsy = .x, Umsy = umsy, Ueq = Ueq),
      .names = "{paste0('Seq_', str_remove_all(.col, '_smsy'))}"
    ),
    across(
      contains("Seq"),
      ~Heq(Ueq = Ueq, Seq = .x),
      .names = "{paste0('Heq_', str_remove_all(.col, 'Seq_'))}"
    )
  ) |> 
  ungroup() |> 
  # Calculate aggregate values for the SMU
  mutate(
    .by = c(Ueq, method),
    across(
      matches("(S|H)eq"), 
      sum, 
      .names = "agg_{.col}"
    ),
    agg_ER_median = agg_Heq_median / (agg_Seq_median + agg_Heq_median),
    agg_ER_lwr = agg_Heq_lwr/(agg_Seq_median + agg_Heq_lwr),
    agg_ER_upr = agg_Heq_upr/(agg_Seq_median + agg_Heq_upr),
    num_stocks_exceed_umsy_median = sum(agg_ER_median > umsy),
    num_stocks_exceed_umsy_lwr = sum(agg_ER_lwr > umsy),
    num_stocks_exceed_umsy_upr = sum(agg_ER_upr > umsy)
  )


# Take a stab at plot
eq_plot <- function(var) {
  
  data <- eq_data |> 
    filter(method == var) |> 
    distinct(Ueq, pick(matches("agg_(H|S)eq")), pick(matches("num_stocks_exceed"))) |> 
    mutate(across(matches("(H|S)eq"), ~if_else(.x < 0, 0, .x)))
  
  
  trans_ratio <- max(data$agg_Heq_upr)/max(data$num_stocks_exceed_umsy_median)
  
  
  plot <- ggplot(data, aes(x = agg_Seq_median, y = agg_Heq_median)) +
    geom_line(colour = "blue") +
    geom_ribbon(
      aes(ymin = agg_Heq_lwr, ymax = agg_Heq_upr),
      fill = "blue",
      alpha = 0.3
    ) +
    geom_line(
      aes(y = num_stocks_exceed_umsy_median * trans_ratio), 
      colour = "red"
    ) +
    geom_ribbon(
      aes(
        ymin = num_stocks_exceed_umsy_lwr * trans_ratio, 
        ymax = num_stocks_exceed_umsy_upr * trans_ratio
      ),
      fill = "red",
      alpha = 0.3
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        transform = ~./trans_ratio,
        name = "Number of stocks where\naggregate ER > Umsy"
      )
    ) +
    labs(title = var)
  
  return(plot)
}


# Plots for each method
set_names(unique(eq_data$method)) |> 
  map(~eq_plot(var = .x))




