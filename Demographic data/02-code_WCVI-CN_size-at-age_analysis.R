# Packages, functions, & global look-up tables -----------------------------

pkgs <- c("tidyverse", "odbc", "dbplyr", "magrittr", "directlabels", 
          "ggridges", "here", "ggstream" 
)
#install.packages(pkgs)

# Load packages
library(ggridges) # Ridgeline plots for ggplot
library(ggstream) # streamcharts for ggplot
library(directlabels) # Direct labelling on ggplot layers
library(tidyverse); theme_set(theme_bw(base_size = 18)) # Data management universe
library(janitor) # For tidying column names
library(odbc) # Manages connections to oracle databases
library(dbplyr) # Translates dplyr verbs to SQL for connected databases 
library(magrittr) # Extended pipelines and code semantics
library(here) # Detect and set sub-directory location for current year



# Load data from DFO online database front ends ---------------------------


# Extract age data from web services using Nick Komick's saaWeb package.
# Will request password entry twice, once for each 'saaWeb::' call
wcvi_cn_ages <- saaWeb::getAgeBatchList() |> 
  filter(
    Species == "Chinook",
    Sector == "SC"
  ) |> 
  pull(Id)  |> # Extract column of age batch Ids from resulting dataframe
  saaWeb::getAgeBatchScaleResults() |> 
  filter(!is.na(Id)) |> 
  clean_names()
# Only includes data from 2020 to present


# Reformat ages as a lookup table
age_vals_lu <- wcvi_cn_ages |> 
  select(container_id, fish_number, gr_age) |> 
  mutate(across(c(container_id, fish_number), as.numeric)) |> 
  rename(
    "scale_book" = "container_id",
    "scale_no" = "fish_number"
  )
