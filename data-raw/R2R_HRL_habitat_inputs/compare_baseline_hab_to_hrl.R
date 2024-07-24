library(tidyverse)

# compare project catalog to HRL
all_existing_and_hrl_data <- readRDS(here::here('data-raw', "R2R_HRL_habitat_inputs", "all_habitat_data_for_hrl_inputs_all_runs.rdata")) |> 
  rename(hrl_acres = total_acres) 
project_catalog <- read_csv(here::here('data-raw', 'R2R_baseline_habitat_inputs', 'R2R_project_catalog_summary.csv')) |> 
  rename(baseline_acres = total_acres)

head(all_existing_and_hrl_data)
head(project_catalog)

hab_join <- all_existing_and_hrl_data |> 
  left_join(project_catalog) |> glimpse()

hab_join |> 
  pivot_longer(cols = c(hrl_acres, baseline_acres), names_to = 'hab_type', values_to = "acres") |> 
  ggplot() +
  geom_col(aes(x = hab_type, y = acres, fill = habitat_type), position = "dodge") +
  facet_wrap(~ watershed, scales = "free_y")
