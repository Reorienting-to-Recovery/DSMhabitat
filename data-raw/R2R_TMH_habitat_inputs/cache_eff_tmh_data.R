# file to cache Theoretical Max Habitat data objects 
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

r_to_r_tmh_fr_spawn_eff <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "eff_sac")
r_to_r_tmh_wr_spawn_eff <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "eff_sac")
r_to_r_tmh_sr_spawn_eff <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "eff_sac")

r_to_r_tmh_fr_spawn_eff - DSMhabitat::fr_spawn$r_to_r_tmh_eff # check, should be different 


## inchannel habitat to both fry and juvenile habitat objects ---------------
r_to_r_tmh_fr_fry_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "eff_sac")$fry
r_to_r_tmh_fr_juv_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "eff_sac")$juv

r_to_r_tmh_wr_fry_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "eff_sac")$fry
r_to_r_tmh_wr_juv_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "eff_sac")$juv

r_to_r_tmh_sr_fry_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "eff_sac")$fry
r_to_r_tmh_sr_juv_eff <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "eff_sac")$juv
r_to_r_tmh_sr_fry_eff[which(is.na(r_to_r_tmh_sr_fry_eff))] <- r_to_r_tmh_fr_fry_eff[which(is.na(r_to_r_tmh_sr_fry_eff))]
r_to_r_tmh_sr_juv_eff[which(is.na(r_to_r_tmh_sr_juv_eff))] <- r_to_r_tmh_fr_juv_eff[which(is.na(r_to_r_tmh_sr_juv_eff))]

r_to_r_tmh_fr_juv_eff - DSMhabitat::fr_juv$r_to_r_tmh_eff # test - should be different 

##floodplain: -------------------------------------------------------------
r_to_r_tmh_fr_flood_eff <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_sr_flood_eff <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_wr_flood_eff <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "biop_itp_2018_2019")

r_to_r_tmh_fr_flood_eff == r_to_r_tmh_sr_flood_eff # test - should be different 


##delta: -------------------------------------------------------------------
r_to_r_tmh_delta_eff <- delta_tmh_processing()

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
### Fall run:
fr_fp <- modifyList(DSMhabitat::fr_fp, list(r_to_r_tmh_eff = r_to_r_tmh_fr_flood_eff)) 
usethis::use_data(fr_fp, overwrite = TRUE)
### Spring Run:
sr_fp <- modifyList(DSMhabitat::sr_fp, list(r_to_r_tmh_eff = r_to_r_tmh_sr_flood_eff))
usethis::use_data(sr_fp, overwrite = TRUE)
#### Winter Run:
wr_fp <- modifyList(DSMhabitat::wr_fp, list(r_to_r_tmh_eff = r_to_r_tmh_wr_flood_eff))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Fall Run: 
fr_fry <- modifyList(DSMhabitat::fr_fry, list(r_to_r_tmh_eff = r_to_r_tmh_fr_fry_eff))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- modifyList(DSMhabitat::fr_juv, list(r_to_r_tmh_eff = r_to_r_tmh_fr_juv_eff))
usethis::use_data(fr_juv, overwrite = TRUE)
### Spring Run: 
sr_fry <- modifyList(DSMhabitat::sr_fry, list(r_to_r_tmh_eff = r_to_r_tmh_sr_fry_eff))
usethis::use_data(sr_fry, overwrite = TRUE)

sr_juv <- modifyList(DSMhabitat::sr_juv, list(r_to_r_tmh_eff = r_to_r_tmh_sr_juv_eff))
usethis::use_data(sr_juv, overwrite = TRUE)
### Winter Run: 
wr_fry <- modifyList(DSMhabitat::wr_fry, list(r_to_r_tmh_eff = r_to_r_tmh_wr_fry_eff))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- modifyList(DSMhabitat::wr_juv, list(r_to_r_tmh_eff = r_to_r_tmh_wr_juv_eff))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Fall run: 
fr_spawn <- modifyList(DSMhabitat::fr_spawn, list(r_to_r_tmh_eff = r_to_r_tmh_fr_spawn_eff))
usethis::use_data(fr_spawn, overwrite = TRUE)
### Spring Run: 
sr_spawn <- modifyList(DSMhabitat::sr_spawn, list(r_to_r_tmh_eff = r_to_r_tmh_sr_spawn_eff))
usethis::use_data(sr_spawn, overwrite = TRUE)
### Winter Run:
wr_spawn <- modifyList(DSMhabitat::wr_spawn, list(r_to_r_tmh_eff = r_to_r_tmh_wr_spawn_eff))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- c(DSMhabitat::delta_habitat, r_to_r_tmh_eff = list(r_to_r_tmh_delta_eff))
usethis::use_data(delta_habitat, overwrite = TRUE)

# do some checks, but make sure you build library first 

table(DSMhabitat::fr_spawn$biop_itp_2018_2019 == DSMhabitat::fr_spawn$r_to_r_tmh_eff)
table(DSMhabitat::fr_fp$biop_itp_2018_2019 == DSMhabitat::fr_fp$r_to_r_tm_effh)
table(DSMhabitat::sr_juv$biop_itp_2018_2019 == DSMhabitat::sr_juv$r_to_r_tmh_eff)

# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
### fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::fr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data =DSMhabitat::wr_spawn$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::wr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data = DSMhabitat::sr_spawn$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::sr_spawn$biop_itp_2018_2019, "spawn")

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fry$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::fr_fry$biop_itp_2018_2019, "fry")

# winter run
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::wr_fry$biop_itp_2018_2019, "fry")
# spring run 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fry$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::sr_fry$biop_itp_2018_2019, "fry")

## floodplain exploratory plot:  -------------------------------------------
# fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fp$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::fr_fp$biop_itp_2018_2019, "flood")
# winter run:
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::wr_fp$biop_itp_2018_2019, "flood")
# spring run: 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fp$r_to_r_tmh_eff, 
                    sit_habitat = DSMhabitat::sr_fp$biop_itp_2018_2019, "flood")

