# run this script to update all the habitat data
# Base data
source("data-raw/cached-habitat.R")

# R2R baseline scaling on various hydrologies
source("data-raw/R2R_baseline_habitat_inputs/cache_baseline_hab.R")
source("data-raw/R2R_baseline_habitat_inputs/cache_eff_with_baseline_hab.R")
source("data-raw/R2R_baseline_habitat_inputs/cache_eff_sj_with_baseline_hab.R") # TODO not sure this does anything
source("data-raw/R2R_baseline_habitat_inputs/cache_max_flow_with_added_habitat_project.R")
source('data-raw/R2R_baseline_habitat_inputs/cache_lto_12a_with_baseline_hab.R')

# Max Habitat scaling on various hydrologies
source("data-raw/R2R_TMH_habitat_inputs/cache_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_eff_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_run_of_river_tmh_data.R")

# HRL habitat
source("data-raw/R2R_HRL_habitat_inputs/cache_va_hrl_data.R")
source("data-raw/R2R_HRL_habitat_inputs/cache_va_hrl_eff_data.R") # EFF in dry years
