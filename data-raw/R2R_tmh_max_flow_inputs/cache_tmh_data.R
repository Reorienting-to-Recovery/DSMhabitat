# file to cache Theoretical Max Habitat data objects that are 
# scaled on Max Flow (run of river) and some exploratory plots to 
# compare SIT existing to TMH and TMH with Max Flow. 


library(tidyverse)
library(DSMhabitat)
library(lubridate)

source('data-raw/R2R_tmh_max_flow_inputs/tmh_helper_functions.R')

# FALL RUN: does not extend past reservoirs -------------------------------
# Winter and Spring Run: extend past reservoirs ---------------------------

# update DSMhabitat values ------------------------------------------------
watersheds_trunc <- DSMscenario::watershed_labels[!(DSMscenario::watershed_labels %in%  c('North Delta', "South Delta", "Sutter Bypass", "Yolo Bypass"))]

r_to_r_tmh_fr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "fr")
r_to_r_tmh_wr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "wr")
r_to_r_tmh_sr_spawn <- spawn_tmh_processing(watersheds = watersheds_trunc, species = "sr")

r_to_r_tmh_fr_spawn == r_to_r_tmh_wr_spawn # check


## inchannel habitat to both fry and juvenile habitat objects ---------------
r_to_r_tmh_fr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr")$fry
r_to_r_tmh_fr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "fr")$juv

r_to_r_tmh_wr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr")$fry
r_to_r_tmh_wr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "wr")$juv

r_to_r_tmh_sr_fry <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr")$fry
r_to_r_tmh_sr_juv <- rearing_tmh_processing(watersheds = watersheds_trunc, species = "sr")$juv

r_to_r_tmh_fr_fry == DSMhabitat::fr_fry$r_to_r_tmh_max_flow # test 
r_to_r_tmh_fr_juv == DSMhabitat::fr_juv$r_to_r_tmh_max_flow # test 
r_to_r_tmh_sr_juv == r_to_r_tmh_wr_juv

##floodplain: -------------------------------------------------------------
r_to_r_tmh_fr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "fr")
r_to_r_tmh_sr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "sr")
r_to_r_tmh_wr_flood <- floodplain_tmh_processing(watersheds = watersheds_trunc, species = "wr")

r_to_r_tmh_fr_fp == DSMhabitat::fr_fp$r_to_r_tmh_max_flow # test 
r_to_r_tmh_fr_fp == r_to_r_tmh_sr_fp


##delta: -------------------------------------------------------------------

watersheds <- c('North Delta', 'South Delta')
r_to_r_tmh_delta <- DSMhabitat::delta_habitat$sit_input

for(i in 1:length(watersheds)) {
  ws = watersheds[i]
  habitat = "rear"
  
  # see: TMH methodology for calcs 
  max_hab_df = data.frame(watershed = c("North Delta", "South Delta"),
                             max_hab = c(41720, 102792)) 
  
  max_hab_acres <- max_hab_df |> 
    filter(watershed == ws) |> 
    pull(max_hab)
  # Instead of taking hab at the median flow to compare take median hab 
  # Check in with Mark on this assumption 
  existing_acres <- median(DSMhabitat::delta_habitat$sit_input[ , , ws]) |> 
    DSMhabitat::square_meters_to_acres()
  
  # Note: If the maximum theoretical habitat was less than the existing SIT habitat, 
  # the theoretical maximum habitat value was used for baseline and model runs. 
  adj_factor = (max_hab_acres - existing_acres) / existing_acres + 1
  
  new_hab_acres <- DSMhabitat::delta_habitat$sit_input[ , , ws] * adj_factor
  
  r_to_r_tmh_delta[, , ws ] <- new_hab_acres 
  
}

# save data objects -------------------------------------------------------

# Save as data object to DSMhabitat
## FLOODPLAIN:
### Fall run:
fr_fp <- c(DSMhabitat::fr_fp[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_fr_flood))
usethis::use_data(fr_fp, overwrite = TRUE)
### Spring Run:
sr_fp <- c(DSMhabitat::sr_fp[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_sr_flood))
usethis::use_data(sr_fp, overwrite = TRUE)
#### Winter Run:
wr_fp <- c(DSMhabitat::wr_fp[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_wr_flood))
usethis::use_data(wr_fp, overwrite = TRUE)

## IN CHANNEL REARING:
### Fall Run: 
fr_fry <- c(DSMhabitat::fr_fry[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_fr_fry))
usethis::use_data(fr_fry, overwrite = TRUE)

fr_juv <- c(DSMhabitat::fr_juv[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_fr_juv))
usethis::use_data(fr_juv, overwrite = TRUE)
### Spring Run: 
sr_fry <- c(DSMhabitat::sr_fry[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_sr_fry))
usethis::use_data(sr_fry, overwrite = TRUE)

sr_juv <- c(DSMhabitat::sr_juv[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_sr_juv))
usethis::use_data(sr_juv, overwrite = TRUE)
### Winter Run: 
wr_fry <- c(DSMhabitat::wr_fry[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_wr_fry))
usethis::use_data(wr_fry, overwrite = TRUE)

wr_juv <- c(DSMhabitat::wr_juv[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_wr_juv))
usethis::use_data(wr_juv, overwrite = TRUE)

## SPAWNING: 
### Fall run: 
fr_spawn <- c(DSMhabitat::fr_spawn[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_fr_spawn))
usethis::use_data(fr_spawn, overwrite = TRUE)
### Spring Run: 
sr_spawn <- c(DSMhabitat::sr_spawn[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_sr_spawn))
usethis::use_data(sr_spawn, overwrite = TRUE)
### Winter Run:
wr_spawn <- c(DSMhabitat::wr_spawn[1:4], r_to_r_tmh_max_flow = list(r_to_r_tmh_wr_spawn))
usethis::use_data(wr_spawn, overwrite = TRUE)

delta_habitat <- c(DSMhabitat::delta_habitat[1:2], r_to_r_tmh_max_flow = list(r_to_r_tmh_delta))
usethis::use_data(delta_habitat, overwrite = TRUE)



# Exploratory Plots:  -----------------------------------------------------
## spawning plot:  ---------------------------------------------------------
### fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_spawn$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::fr_spawn$r_to_r_tmh_max_flow, "spawn")

tmh_comparison_plot(tmh_data =DSMhabitat::wr_spawn$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::wr_spawn$r_to_r_tmh_max_flow, "spawn")

tmh_comparison_plot(tmh_data = DSMhabitat::sr_spawn$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::sr_spawn$r_to_r_tmh_max_flow, "spawn")

## fry and juv plots:  -----------------------------------------------------
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fry$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::fr_fry$r_to_r_tmh_max_flow, "fry")

# winter run
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fry$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::wr_fry$r_to_r_tmh_max_flow, "fry")
# spring run 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fry$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::sr_fry$r_to_r_tmh_max_flow, "fry")

## floodplain exploratory plot:  -------------------------------------------
# fall run: 
tmh_comparison_plot(tmh_data = DSMhabitat::fr_fp$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::fr_fp$r_to_r_tmh_max_flow, "flood")
# winter run:
tmh_comparison_plot(tmh_data = DSMhabitat::wr_fp$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::wr_fp$r_to_r_tmh_max_flow, "flood")
# spring run: 
tmh_comparison_plot(tmh_data = DSMhabitat::sr_fp$r_to_r_tmh, 
                    tmh_with_max_flow = DSMhabitat::sr_fp$r_to_r_tmh_max_flow, "flood")
