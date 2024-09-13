# Packages ----------------------------------------------------------------

pkgs <- c("tidyverse", "here", "readxl", "janitor")
#install.packages(pkgs)

library(here)
library(tidyverse)
library(readxl)
library(janitor)


# Disable scientific notation in outputs
options(scipen = 999)


# Productivity distributions for each CU ----------------------------------


# Values from Holt et al 2023 
# (https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41195255.pdf)
# Table 10 (Page 56)
productivity <- tribble(
  ~cu, ~mean_log_a, ~mean_log_srep, ~sd_log_srep,
  "SWVI", 1.14, 9.711, 0.286,
  "NWVI", 1.58, 8.196, 0.32,
  "NoKy", 1.53, 10.247, 0.285
) |> 
  mutate(
    sd_log_a = 0.5,
    mean_srep = exp(mean_log_srep - 0.5*sd_log_srep^2),
    sd_srep = exp(sd_log_srep)
  ) |> 
  rowwise() |> 
  # Bootstrap beta values from distribution of alpha and srep
  mutate(
    beta = list(
      tibble(
        log_a = rnorm(1000, mean = mean_log_a, sd = sd_log_a),
        srep = rnorm(1000, mean = mean_srep, sd = sd_srep)
      ) |> 
        mutate(beta = log_a/srep) |> 
        summarize(
          mean_beta = mean(beta),
          sd_beta = sd(beta)
        )
    )
  ) |> 
  unnest(beta)


# Define functions for calculating Srep and Heq ----------------------------


# Seq - equilibrium population escapement at fixed harvest rate
Seq <- function(alpha, beta, U) {
  
  Seq = (alpha - (-log(1-U)))/beta
  
  return(Seq) 
}


# Heq - equilibrium harvest 
Heq <- function(alpha, beta, U) {
  
  Seq = (alpha - (-log(1-U)))/beta
  
  Heq = (Seq * exp(alpha-(beta*Seq))) - Seq
  
  return(Heq)
}



# Calculate equilibrium harvest curves -------------------------------------


# Define range of harvest rates
U <- tibble(U = seq(0, 0.99, by = 0.01))


# Get data summarized by CU
cu_params <- productivity |> 
  select(cu, matches("log_a|(?<!log)_srep", perl = TRUE)) |> 
  rowwise() |> 
  mutate(
    data = list(
      tibble(
        log_a = rnorm(1000, mean = mean_log_a, sd = sd_log_a),
        srep = rnorm(1000, mean = mean_srep, sd = sd_srep)
      ) |> 
        #filter(log_a > 0) |> 
        mutate(beta = log_a/srep) |> 
        crossing(U) |> 
        mutate(
          Seq = Seq(alpha = log_a, beta = beta, U = U),
          Heq = Heq(alpha = log_a, beta = beta, U = U),
          across(Seq:Heq, ~if_else(U == 1, 0, .x))
        )
    )  
  )


# Calculate CU-level Umsy
umsy_vals <- cu_params |> 
  unnest(data) |> 
  distinct(cu, log_a, beta) |> 
  mutate(umsy = beta * ((log_a*(0.5-0.07*log_a))/beta)) |> 
  summarise(
    .by = cu,
    umsy_mid = median(umsy),
    umsy_lwr = quantile(umsy, 0.25),
    umsy_upr = quantile(umsy, 0.75)
  )


# Unpack and summarize data
eq_sum_data <- cu_params |> 
  unnest(data) |> 
  filter(!if_any(Seq:Heq, ~(is.na(.x) | is.infinite(.x)))) |> 
  mutate(
    .by = cu,
    id = row_number(),
    #across(Seq:Heq, ~if_else(.x < 0, 0, .x))
  ) |> 
  summarize(
    .by = c(id, U),
    across(Seq:Heq, sum)
  ) |> 
  summarize(
    .by = U,
    Seq = median(Seq),
    Heq_mid = median(Heq),
    Heq_lwr = quantile(Heq, 0.25),
    Heq_upr = quantile(Heq, 0.75)
  ) |> 
  mutate(
    umsy_mid = list(umsy_vals$umsy_mid),
    umsy_lwr = list(umsy_vals$umsy_lwr),
    umsy_upr = list(umsy_vals$umsy_upr),
    across(
      contains("umsy"),
      ~map2(
        .x = U,
        .y = .,
        ~ifelse(.x > .y, 1, 0)
      ) |> 
        map(sum) |> 
        unlist(),
      .names = "num_cu_exceed_{.col}"
    )
  )


  
# Calculate secondary y-axis transformation
ratio <- max(eq_sum_data$Heq_upr)*1.05/max(eq_sum_data$num_cu_exceed_umsy_upr)


# Calculate Smsy median, upper, and lower values
mid_Heq_Smsy = max(eq_sum_data$Heq_mid)
lwr_Heq_Smsy = max(eq_sum_data$Heq_lwr)
upr_Heq_Smsy = max(eq_sum_data$Heq_upr)


# label Smsy
smsy_lab <- tibble(
  Smsy_mid = eq_sum_data$Seq[eq_sum_data$Heq_mid == mid_Heq_Smsy],
  # Heq and Smsy lower and upper bounds are reversed
  Smsy_upr = eq_sum_data$Seq[eq_sum_data$Heq_lwr == lwr_Heq_Smsy],
  Smsy_lwr = eq_sum_data$Seq[eq_sum_data$Heq_upr == upr_Heq_Smsy],
  Heq_mid = mid_Heq_Smsy
) |> 
  mutate(label = Smsy_mid)


# U labels at 5% increments
U_lab <- eq_sum_data |> 
  mutate(U= as.character(U)) |>
  select(U, Seq) |> 
  right_join(tibble(U = as.character(seq(0, 0.95, by = 0.05)))) |> 
  distinct(Seq, .keep_all = TRUE) |> 
  mutate(U = as.numeric(U))


# Plot summarized data
(eq_plot <- ggplot(eq_sum_data, aes(x = Seq, y = Heq_mid)) +
    # Add vertical lines showing harvest rate steps
    geom_vline(
      xintercept = U_lab$Seq,
      colour = "grey90"
    ) +
    # Label harvest rate steps
    annotate(
      "text",
      x = U_lab$Seq,
      y = upr_Heq_Smsy,
      label = scales::percent(U_lab$U),
      # Make text vertical and offset to the right of the lines
      angle = 270,
      vjust = -0.5,
      hjust = 0,
      colour = "grey75"
    ) +
    # Add stepped line showing # CUs where agg ER exceeds Umsy
    geom_step(
      aes(y = num_cu_exceed_umsy_mid*ratio),
      colour = "red",
      linewidth = 1.25
    ) +
    # Add stepped CI corresponding to the stepped line
    geom_rect(
      aes(
        xmin = Seq,
        xmax = lead(Seq),
        ymin = num_cu_exceed_umsy_lwr*ratio, 
        ymax = num_cu_exceed_umsy_upr*ratio
      ),
      fill = "red",
      alpha = 0.2
    ) +
    geom_line(
      colour = "blue",
      linewidth = 1
    ) +
    geom_ribbon(
      aes(ymin = Heq_lwr, ymax = Heq_upr),
      fill = "blue",
      alpha = 0.2
    ) +
    # Pointrange showing the estimated Smsy
    geom_pointrange(
      data = smsy_lab,
      aes(x = Smsy_mid, xmin = Smsy_lwr, xmax = Smsy_upr),
      size = 1,
      linewidth = 1
    ) +
    # Add text annotation that states midpoint and IQR of Smsy
    annotate(
      "text",
      x = smsy_lab$label,
      y = smsy_lab$Heq_mid,
      label = paste0(
        "S[MSY] ==~", 
        round(smsy_lab$label, 0),
        "~(IQR:~",
        round(smsy_lab$Smsy_lwr),
        "-",
        round(smsy_lab$Smsy_upr),
        ")"
      ),
      hjust = 0.1,
      vjust = -1,
      parse = TRUE
    ) +
    scale_x_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      labels = scales::comma,
      expand = expansion(mult = c(0, 0)),
      sec.axis = sec_axis(
        transform = ~./ratio,
        name = "Number of CUs where agg. ER > Umsy"
      )
    ) +
    coord_cartesian(
      xlim = c(0, max(eq_sum_data$Seq)),
      ylim = c(0, max(eq_sum_data$Heq_upr*1.05))
      ) +
    labs(
      y = "Aggregate equilibrium harvest",
      x = "Aggregate equilibrium spawners"
    ) +
    theme_classic() +
    # Match axis title colour to corresponding line colour
    theme(
      axis.title.y.left = element_text(colour = "blue"),
      axis.title.y.right = element_text(colour = "red"),
    )
)


# Save plot
ggsave(
  eq_plot,
  filename = here(
    "Equilibrium trade-off analysis", 
    "R-PLOT_equilibrium_harvest_curves.png"
  ),
  height = 4.5,
  width = 7, units = "in"
)


