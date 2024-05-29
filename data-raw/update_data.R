# run this script to update all the habitat data
# Base data
source("data-raw/cached-habitat.R")

# R2R baseline scaling on various hydrologies
source("data-raw/R2R_baseline_habitat_inputs/cache_baseline_hab.R")
source("data-raw/R2R_baseline_habitat_inputs/cache_eff_with_baseline_hab.R")
source("data-raw/R2R_baseline_habitat_inputs/cache_max_flow_with_added_habitat_project.R")

# Max Habitat scaling on various hydrologies
source("data-raw/R2R_TMH_habitat_inputs/cache_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_eff_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_run_of_river_tmh_data.R")
