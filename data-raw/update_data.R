# run this script to update all the habitat data
# Base data
source("data-raw/cached-habitat.R") # TODO add action 5 baseline

# R2R baseline scaling on various hydrologies
source("data-raw/R2R_baseline_habitat_inputs/cache_baseline_hab.R")
# TODO add script for caching baseline with action 5 -- see cache_lto_12a_with_baseline_hab.R

# Max Habitat scaling on various hydrologies
source("data-raw/R2R_TMH_habitat_inputs/cache_tmh_data.R")
source("data-raw/R2R_TMH_habitat_inputs/cache_run_of_river_tmh_data.R")
# TODO add script for caching tmh with action 5 (upper sacramento only)
