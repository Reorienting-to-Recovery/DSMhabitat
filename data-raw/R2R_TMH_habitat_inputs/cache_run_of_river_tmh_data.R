# file to cache Run of River theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH. 

library(tidyverse)
library(DSMhabitat)
library(lubridate)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')

# FALL RUN: does not extend past reservoirs -------------------------------
# Winter and Spring Run: extend past reservoirs ---------------------------

# update DSMhabitat values ------------------------------------------------
watersheds_trunc <- DSMscenario::watershed_labels[!(DSMscenario::watershed_labels %in%  c('North Delta', "South Delta", "Sutter Bypass", "Yolo Bypass"))]

run_of_river_tmh_fr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "run_of_river")
run_of_river_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "run_of_river")
run_of_river_tmh_sr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "run_of_river")

run_of_river_tmh_fr_spawn == DSMhabitat::fr_spawn$biop_itp_2018_2019 # check, should be different 
run_of_river_tmh_fr_spawn == DSMhabitat::fr_spawn$run_of_river # check, should be different

## inchannel habitat to both fry and juvenile habitat objects ---------------
run_of_river_tmh_fr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "run_of_river")$fry
run_of_river_tmh_fr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "run_of_river")$juv

run_of_river_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "run_of_river")$fry
run_of_river_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "run_of_river")$juv

run_of_river_tmh_sr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "run_of_river")$fry
run_of_river_tmh_sr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "run_of_river")$juv
run_of_river_tmh_sr_fry[which(is.na(r_to_r_tmh_sr_fry))] <- run_of_river_tmh_fr_fry[which(is.na(run_of_river_tmh_sr_fry))]
run_of_river_tmh_sr_juv[which(is.na(r_to_r_tmh_sr_juv))] <- run_of_river_tmh_fr_juv[which(is.na(run_of_river_tmh_sr_juv))]

run_of_river_tmh_fr_fry == DSMhabitat::fr_fry$biop_itp_2018_2019 # test - should be different 
run_of_river_tmh_fr_fry == DSMhabitat::fr_fry$run_of_river # test - should be different 

##floodplain: -------------------------------------------------------------
run_of_river_tmh_fr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "run_of_river")
run_of_river_tmh_sr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "run_of_river")
run_of_river_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "run_of_river")

run_of_river_tmh_fr_flood == DSMhabitat::fr_fp$biop_itp_2018_2019 # test - should be different 
run_of_river_tmh_fr_flood == DSMhabitat::fr_fp$run_of_river

##delta: -------------------------------------------------------------------
run_of_river_tmh_delta <- delta_tmh_processing()

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
### Fall run:
fr_fp <- modifyList(DSMhabitat::fr_fp, list(run_of_river_tmh = run_of_river_tmh_fr_flood))
usethis::use_data(fr_fp, overwrite = TRUE)
### Spring Run:
sr_fp <- modifyList(DSMhabitat::sr_fp, list(run_of_river_tmh = run_of_river_tmh_sr_flood))
usethis::use_data(sr_fp, overwrite = TRUE)
#### Winter Run:
wr_fp <- modifyList(DSMhabitat::wr_fp, list(run_of_river_tmh = run_of_river_tmh_wr_flood))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Fall Run: 
fr_fry <- modifyList(DSMhabitat::fr_fry, list(run_of_river_tmh = run_of_river_tmh_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- modifyList(DSMhabitat::fr_juv, list(run_of_river_tmh = run_of_river_tmh_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)
### Spring Run: 
sr_fry <- modifyList(DSMhabitat::sr_fry, list(run_of_river_tmh = run_of_river_tmh_sr_fry))
usethis::use_data(sr_fry, overwrite = TRUE)

sr_juv <- modifyList(DSMhabitat::sr_juv, list(run_of_river_tmh = run_of_river_tmh_sr_juv))
usethis::use_data(sr_juv, overwrite = TRUE)
### Winter Run: 
wr_fry <- modifyList(DSMhabitat::wr_fry, list(run_of_river_tmh = run_of_river_tmh_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- modifyList(DSMhabitat::wr_juv, list(run_of_river_tmh = run_of_river_tmh_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Fall run: 
fr_spawn <- modifyList(DSMhabitat::fr_spawn, list(run_of_river_tmh = run_of_river_tmh_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)
### Spring Run: 
sr_spawn <- modifyList(DSMhabitat::sr_spawn, list(run_of_river_tmh = run_of_river_tmh_sr_spawn))
usethis::use_data(sr_spawn, overwrite = TRUE)
### Winter Run:
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list(run_of_river_tmh = run_of_river_tmh_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- modifyList(DSMhabitat::delta_habitat, list(run_of_river_tmh = run_of_river_tmh_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)
# do some checks, but make sure you build library first 

table(DSMhabitat::fr_spawn$run_of_river == DSMhabitat::fr_spawn$run_of_river_tmh)
table(DSMhabitat::fr_fp$run_of_river == DSMhabitat::fr_fp$run_of_river_tmh)
table(DSMhabitat::sr_juv$run_of_river == DSMhabitat::sr_juv$run_of_river_tmh)

# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
### fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::fr_spawn$run_of_river, "spawn")

tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::fr_spawn$r_to_r_tmh, "spawn")

tmh_comparison_plot(tmh_data =DSMhabitat::wr_spawn$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::wr_spawn$r_to_r_tmh, "spawn")

tmh_comparison_plot(tmh_data = DSMhabitat::sr_spawn$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::sr_spawn$r_to_r_tmh, "spawn")

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fry$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::fr_fry$r_to_r_tmh, "fry")

# winter run
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::wr_fry$r_to_r_tmh, "fry")
# spring run 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fry$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::sr_fry$r_to_r_tmh, "fry")

## floodplain exploratory plot:  -------------------------------------------
# fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fp$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::fr_fp$r_to_r_tmh, "flood")
# winter run:
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::wr_fp$r_to_r_tmh, "flood")
# spring run: 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fp$run_of_river_tmh, 
                    sit_habitat = DSMhabitat::sr_fp$r_to_r_tmh, "flood")

