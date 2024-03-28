
# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "here", "readxl", "janitor")
#install.packages(pkgs)

library(here)
library(tidyverse)
library(readxl)
library(janitor)


# Load RP data ------------------------------------------------------------


# Using kobe plot data as working example for now
rp_data <- list.files(
  here("Kobe plot"),
  pattern = "(?i)sr parameters",
  full.names = TRUE
) |> 
  read_excel(sheet = "Raw Holt RP data") |> 
  clean_names() |> 
  mutate(
    smsy_divide_srep = smsy_median/srep_median,
    alpha = exp((0.5-smsy_divide_srep)/0.07),
    beta = log(alpha)/srep_median,
    umsy = (0.5*log(alpha)) - (0.07*(log(alpha))^2),
    recruits_at_0.85smsy = alpha*0.85*smsy_median*(exp(-beta*0.85*smsy_median))
  ) |> 
  # Clean up river names
  mutate(river = make_clean_names(river))



# Productivity distributions for each CU ----------------------------------


# As a dataframe
productivity <- rp_data |> 
  distinct(cu) |> 
  mutate(
    # Values from Holt et al 2023 
    # (https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41195255.pdf)
    # Table 10 (Page 56)
    mean_log_a = case_when(
      cu == "SWVI" ~ 1.14,
      cu == "NWVI" ~ 1.58,
      cu == "Nootka_Kyuquot" ~ 1.53
    ),
    sd_log_a = 0.5
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



# Apply functions to WCVI data --------------------------------------------


# Define range of harvest rates
Ueq <- tibble(Ueq = seq(0, 0.95, by = 0.05))


# Add harvest rates to data and execute calculations
eq_data <- rp_data |> 
  crossing(Ueq) |> 
  rowwise() |> 
  mutate(
    Seq_vals = Seq(Smsy = smsy_median, Umsy = umsy, Ueq = Ueq),
    Heq_vals = Heq(Ueq = Ueq, Seq = Seq_vals)
  ) |> 
  ungroup() |> 
  # Calculate aggregate values for the SMU
  mutate(
    .by = Ueq,
    agg_Heq = sum(Heq_vals),
    agg_Seq = sum(Seq_vals),
    agg_ER = agg_Heq/(agg_Heq + agg_Seq), # **Not sure this is correct**
    num_stock_exceeding_umsy = sum(agg_ER > umsy)
  )

  
# Take a stab at plot
eq_data |> 
  distinct(agg_Heq, agg_Seq, Ueq, num_stock_exceeding_umsy) |> 
  mutate(across(agg_Heq:agg_Seq, ~if_else(.x < 0, 0, .x))) |> 
  ggplot(aes(x = agg_Seq, y = agg_Heq)) +
  geom_line(colour = "blue") +
  geom_line(
    aes(y = num_stock_exceeding_umsy * 1111), 
    colour = "red"
  ) +
  scale_y_continuous(sec.axis = ~./1111)

  
  
  
