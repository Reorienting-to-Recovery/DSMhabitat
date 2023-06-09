# file to cache Theoretical Max Habitat data objects 
# and some exploratory plots to compare SIT existing 
# to TMH. 

library(tidyverse)
library(DSMhabitat)
library(lubridate)

options(warn = -1)

source('data-raw/R2R_TMH_habitat_inputs/tmh_helper_functions.R')

# FALL RUN: does not extend past reservoirs -------------------------------
# Winter and Spring Run: extend past reservoirs ---------------------------

# update DSMhabitat values ------------------------------------------------
watersheds_trunc <- DSMscenario::watershed_labels[!(DSMscenario::watershed_labels %in%  c('North Delta', "South Delta", "Sutter Bypass", "Yolo Bypass"))]

r_to_r_tmh_fr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_sr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "biop_itp_2018_2019")

r_to_r_tmh_fr_spawn == r_to_r_tmh_wr_spawn # check, should be different 


## inchannel habitat to both fry and juvenile habitat objects ---------------
r_to_r_tmh_fr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "biop_itp_2018_2019")$fry
r_to_r_tmh_fr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "biop_itp_2018_2019")$juv

r_to_r_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "biop_itp_2018_2019")$fry
r_to_r_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "biop_itp_2018_2019")$juv

r_to_r_tmh_sr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "biop_itp_2018_2019")$fry
r_to_r_tmh_sr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "biop_itp_2018_2019")$juv
r_to_r_tmh_sr_fry[which(is.na(r_to_r_tmh_sr_fry))] <- r_to_r_tmh_fr_fry[which(is.na(r_to_r_tmh_sr_fry))]
r_to_r_tmh_sr_juv[which(is.na(r_to_r_tmh_sr_juv))] <- r_to_r_tmh_fr_juv[which(is.na(r_to_r_tmh_sr_juv))]

r_to_r_tmh_sr_juv == r_to_r_tmh_wr_juv # test - should be different 

##floodplain: -------------------------------------------------------------
r_to_r_tmh_fr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "fr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_sr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "sr", calsim_run = "biop_itp_2018_2019")
r_to_r_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr", calsim_run = "biop_itp_2018_2019")

r_to_r_tmh_fr_flood == r_to_r_tmh_sr_flood # test - should be different 


##delta: -------------------------------------------------------------------
r_to_r_tmh_delta <- delta_tmh_processing()

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
### Fall run:
fr_fp <- c(DSMhabitat::fr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_flood))
usethis::use_data(fr_fp, overwrite = TRUE)
### Spring Run:
sr_fp <- c(DSMhabitat::sr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_flood))
usethis::use_data(sr_fp, overwrite = TRUE)
#### Winter Run:
wr_fp <- c(DSMhabitat::wr_fp[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_flood))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Fall Run: 
fr_fry <- c(DSMhabitat::fr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- c(DSMhabitat::fr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)
### Spring Run: 
sr_fry <- c(DSMhabitat::sr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_fry))
usethis::use_data(sr_fry, overwrite = TRUE)

sr_juv <- c(DSMhabitat::sr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_juv))
usethis::use_data(sr_juv, overwrite = TRUE)
### Winter Run: 
wr_fry <- c(DSMhabitat::wr_fry[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- c(DSMhabitat::wr_juv[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Fall run: 
fr_spawn <- c(DSMhabitat::fr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)
### Spring Run: 
sr_spawn <- c(DSMhabitat::sr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_sr_spawn))
usethis::use_data(sr_spawn, overwrite = TRUE)
### Winter Run:
wr_spawn <- c(DSMhabitat::wr_spawn[1:3], r_to_r_tmh = list(r_to_r_tmh_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- c(DSMhabitat::delta_habitat, r_to_r_tmh = list(r_to_r_tmh_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)



# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
### fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::fr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data =DSMhabitat::wr_spawn$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::wr_spawn$biop_itp_2018_2019, "spawn")

tmh_comparison_plot(tmh_data = DSMhabitat::sr_spawn$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::sr_spawn$biop_itp_2018_2019, "spawn")

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fry$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::fr_fry$biop_itp_2018_2019, "fry")

# winter run
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::wr_fry$biop_itp_2018_2019, "fry")
# spring run 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fry$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::sr_fry$biop_itp_2018_2019, "fry")

## floodplain exploratory plot:  -------------------------------------------
# fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fp$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::fr_fp$biop_itp_2018_2019, "flood")
# winter run:
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::wr_fp$biop_itp_2018_2019, "flood")
# spring run: 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fp$r_to_r_tmh, 
                    sit_habitat = DSMhabitat::sr_fp$biop_itp_2018_2019, "flood")

