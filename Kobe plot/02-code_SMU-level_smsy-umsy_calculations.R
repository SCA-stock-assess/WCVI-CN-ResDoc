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


# Load RP data ------------------------------------------------------------


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


# Apply functions to WCVI data --------------------------------------------


# Define range of harvest rates
Ueq <- tibble(Ueq = seq(0, 1, by = 0.01))


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




# Simple example with Carrie's method -------------------------------------


# Get data summarized by CU
holt_params <- rp_data |> 
  filter(method == "RunRecon") |>
  summarize(
    .by = c(cu, mean_log_a, sd_log_a, beta),
    across(matches("median_s.{3}"), sum)
  ) |> 
  crossing(Ueq) |> 
  mutate(
    Seq = Seq_holt(alpha = mean_log_a, beta = beta, U = Ueq),
    Heq = Heq_holt(alpha = mean_log_a, beta = beta, U = Ueq),
    across(Seq:Heq, ~if_else(.x < 0, 0, .x)),
  )


# Plot
holt_params |> 
  filter(!Ueq == 1) |> 
  summarize(
    .by = c(Ueq),
    across(Seq:Heq, sum)
  ) |> 
  ggplot(aes(x = Seq, y = Heq)) +
  geom_line(colour = "blue")


